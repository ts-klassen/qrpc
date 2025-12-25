use std::{
    collections::{HashMap, HashSet},
    env,
    fs,
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::{anyhow, bail, Context, Result};
use futures_util::StreamExt;
use lapin::{
    message::Delivery,
    options::{
        BasicAckOptions, BasicConsumeOptions, BasicGetOptions, BasicPublishOptions,
        BasicQosOptions, ExchangeDeclareOptions, QueueBindOptions, QueueDeclareOptions,
    },
    types::FieldTable,
    BasicProperties, Channel, Connection, ConnectionProperties, ExchangeKind,
};
use rand::seq::SliceRandom;
use reqwest::{header::CONTENT_TYPE, Client, Url};
use serde::Deserialize;
use serde_json::json;
use tokio::sync::Mutex;
use tracing::{error, info, warn};
use tracing_subscriber::EnvFilter;
use voicevox_core::{
    blocking::{Onnxruntime, OpenJtalk, Synthesizer, VoiceModelFile},
    StyleId, VoiceModelId,
};

const DEFAULT_CONFIG_PATH: &str = "/opt/qrpc/etc/q_vvx_worker/config.toml";
const DEFAULT_AMQP_ADDR: &str = "amqp://guest:guest@localhost:5672/%2f";
const DEFAULT_ASSETS_DIR: &str = "/opt/qrpc/pkg/share/voicevox_core";
const DEFAULT_DICT_DIR: &str = "dict/open_jtalk_dic_utf_8-1.11";
const DEFAULT_HTTP_TIMEOUT_SECS: u64 = 30;
const EVENT_EXCHANGE: &str = "qrpc_event_exchange";
const JOB_EXCHANGE: &str = "q_vvx_tts_exchange";
const WAKE_EXCHANGE: &str = "q_vvx_tts_wake_exchange";
const WAKE_QUEUE: &str = "q_vvx_tts_wake";
const WAKE_ROUTING_KEY: &str = "wake";
const JOB_QUEUE_PREFIX: &str = "q_vvx_tts_";
const MESSAGE_ON_COMPLETE: &str = "q_vvx_worker:upload_complete";
const MESSAGE_ON_FAILED: &str = "q_vvx_worker:upload_failed";
const MAX_TEXT_BYTES: usize = 1500;
const EVENT_QUEUE_TTL_MS: i64 = 3_600_000;
const EVENT_QUEUE_EXPIRES_MS: i64 = 3_900_000;
const CONFIG_ARG: &str = "--config";
const TLGRF_TAGS_ARG: &str = "--tlgrf_tegs";
const TLGRF_TAGS_ARG_ALT: &str = "--tlgrf_tags";

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info"));
    tracing_subscriber::fmt().with_env_filter(filter).compact().init();

    let args = Args::parse()?;
    let cfg = Config::load(&args.config_path)?;
    let synth = SynthResources::new(&cfg)?;
    let http = Client::builder()
        .timeout(Duration::from_secs(cfg.http_timeout_secs))
        .build()
        .context("failed to build HTTP client")?;

    Worker::new(cfg, synth, http).await?.run().await
}

struct Worker {
    cfg: Config,
    synth: SynthResources,
    http: Client,
    _conn: Connection,
    job_channel: Channel,
    wake_consumer: lapin::Consumer,
    event_publisher: EventPublisher,
    speaker_ids: Vec<u32>,
    current_model: Option<LoadedModel>,
}

impl Worker {
    async fn new(cfg: Config, synth: SynthResources, http: Client) -> Result<Self> {
        info!(amqp = %cfg.amqp_addr, "starting q_vvx_worker");

        let conn = Connection::connect(&cfg.amqp_addr, ConnectionProperties::default())
            .await
            .context("failed to connect to RabbitMQ")?;
        let job_channel = conn
            .create_channel()
            .await
            .context("failed to open job channel")?;
        let event_channel = conn
            .create_channel()
            .await
            .context("failed to open event channel")?;

        job_channel
            .basic_qos(1, BasicQosOptions::default())
            .await
            .context("failed to set qos")?;

        job_channel
            .exchange_declare(
                JOB_EXCHANGE,
                ExchangeKind::Direct,
                ExchangeDeclareOptions {
                    durable: true,
                    ..Default::default()
                },
                FieldTable::default(),
            )
            .await
            .context("failed to declare job exchange")?;

        job_channel
            .exchange_declare(
                WAKE_EXCHANGE,
                ExchangeKind::Direct,
                ExchangeDeclareOptions {
                    durable: true,
                    ..Default::default()
                },
                FieldTable::default(),
            )
            .await
            .context("failed to declare wake exchange")?;

        for speaker_id in cfg.speaker_models.keys() {
            let queue_name = job_queue_name(*speaker_id);
            job_channel
                .queue_declare(
                    &queue_name,
                    QueueDeclareOptions {
                        durable: true,
                        ..Default::default()
                    },
                    FieldTable::default(),
                )
                .await
                .with_context(|| format!("failed to declare job queue {queue_name}"))?;

            job_channel
                .queue_bind(
                    &queue_name,
                    JOB_EXCHANGE,
                    &speaker_id.to_string(),
                    QueueBindOptions::default(),
                    FieldTable::default(),
                )
                .await
                .with_context(|| format!("failed to bind job queue {queue_name}"))?;
        }

        job_channel
            .queue_declare(
                WAKE_QUEUE,
                QueueDeclareOptions {
                    auto_delete: true,
                    durable: false,
                    ..Default::default()
                },
                FieldTable::default(),
            )
            .await
            .context("failed to declare wake queue")?;

        job_channel
            .queue_bind(
                WAKE_QUEUE,
                WAKE_EXCHANGE,
                WAKE_ROUTING_KEY,
                QueueBindOptions::default(),
                FieldTable::default(),
            )
            .await
            .context("failed to bind wake queue")?;

        let wake_consumer = job_channel
            .basic_consume(
                WAKE_QUEUE,
                "q_vvx_worker_wake",
                BasicConsumeOptions::default(),
                FieldTable::default(),
            )
            .await
            .context("failed to start wake consumer")?;

        event_channel
            .exchange_declare(
                EVENT_EXCHANGE,
                ExchangeKind::Topic,
                ExchangeDeclareOptions {
                    durable: true,
                    ..Default::default()
                },
                FieldTable::default(),
            )
            .await
            .context("failed to declare qrpc_event exchange")?;

        let speaker_ids = cfg.sorted_speaker_ids();

        Ok(Self {
            cfg,
            synth,
            http,
            _conn: conn,
            job_channel,
            wake_consumer,
            event_publisher: EventPublisher::new(event_channel),
            speaker_ids,
            current_model: None,
        })
    }

    async fn run(mut self) -> Result<()> {
        info!(
            speakers = self.speaker_ids.len(),
            "worker ready for jobs"
        );

        loop {
            if let Some(speaker_id) = self.current_model.as_ref().map(|model| model.speaker_id) {
                if let Some(delivery) = self.try_get_job(speaker_id).await? {
                    self.process_delivery(delivery).await?;
                    continue;
                }
            }

            match self.select_speaker_with_messages().await? {
                Some(speaker_id) => {
                    if let Some(delivery) = self.try_get_job(speaker_id).await? {
                        self.process_delivery(delivery).await?;
                    }
                }
                None => {
                    self.wait_for_wake().await?;
                }
            }
        }
    }

    async fn try_get_job(&self, speaker_id: u32) -> Result<Option<Delivery>> {
        let queue_name = job_queue_name(speaker_id);
        let message = self
            .job_channel
            .basic_get(
                &queue_name,
                BasicGetOptions {
                    no_ack: false,
                    ..Default::default()
                },
            )
            .await
            .with_context(|| format!("failed to read from {queue_name}"))?;

        Ok(message.map(|msg| msg.delivery))
    }

    async fn select_speaker_with_messages(&self) -> Result<Option<u32>> {
        let mut max_count = 0u32;
        let mut candidates = Vec::new();

        for speaker_id in &self.speaker_ids {
            let queue_name = job_queue_name(*speaker_id);
            let queue = self
                .job_channel
                .queue_declare(
                    &queue_name,
                    QueueDeclareOptions {
                        passive: true,
                        ..Default::default()
                    },
                    FieldTable::default(),
                )
                .await
                .with_context(|| format!("failed to inspect queue {queue_name}"))?;
            let count = queue.message_count();

            if count == 0 {
                continue;
            }

            if count > max_count {
                max_count = count;
                candidates.clear();
                candidates.push(*speaker_id);
            } else if count == max_count {
                candidates.push(*speaker_id);
            }
        }

        if candidates.is_empty() {
            return Ok(None);
        }

        let mut rng = rand::thread_rng();
        Ok(candidates.choose(&mut rng).copied())
    }

    async fn wait_for_wake(&mut self) -> Result<()> {
        info!("waiting for wake message");
        while let Some(delivery_result) = self.wake_consumer.next().await {
            match delivery_result {
                Ok(delivery) => {
                    if let Err(err) = delivery.ack(BasicAckOptions::default()).await {
                        return Err(anyhow!("failed to ack wake message: {err}"));
                    }
                    return Ok(());
                }
                Err(err) => {
                    return Err(anyhow!("wake consumer error: {err}"));
                }
            }
        }

        Err(anyhow!("wake consumer closed"))
    }

    async fn process_delivery(&mut self, delivery: Delivery) -> Result<()> {
        let payload: JobPayload = match serde_json::from_slice(&delivery.data) {
            Ok(payload) => payload,
            Err(err) => {
                warn!(?err, "dropping invalid job payload");
                return self.ack_delivery(&delivery).await;
            }
        };

        let namespace = payload.qrpc_event_namespace.clone();
        let event_id = payload.qrpc_event_id.clone();
        if namespace.trim().is_empty() || event_id.trim().is_empty() {
            warn!("dropping job with invalid event identifiers");
            return self.ack_delivery(&delivery).await;
        }

        let prepared = match payload.prepare() {
            Ok(job) => job,
            Err(err) => {
                let message = format_failure_message(&err);
                self.ack_delivery(&delivery).await?;
                self.spawn_event(namespace, event_id, message);
                return Ok(());
            }
        };

        let model_path = match self.cfg.speaker_models.get(&prepared.speaker_id) {
            Some(path) => path.clone(),
            None => {
                let message = format_failure_message(&format!(
                    "unknown speaker_id {}",
                    prepared.speaker_id
                ));
                self.ack_delivery(&delivery).await?;
                self.spawn_event(namespace, event_id, message);
                return Ok(());
            }
        };

        if self
            .current_model
            .as_ref()
            .map(|model| model.speaker_id)
            != Some(prepared.speaker_id)
        {
            if let Err(err) = self.load_model(prepared.speaker_id, &model_path) {
                let message = format_failure_message(&format!("model load failed: {err}"));
                self.ack_delivery(&delivery).await?;
                self.spawn_event(namespace, event_id, message);
                return Ok(());
            }
        }

        let wav = match self.synth.synthesize(&prepared.text, prepared.speaker_id) {
            Ok(wav) => wav,
            Err(err) => {
                let message = format_failure_message(&format!("synthesis failed: {err}"));
                self.ack_delivery(&delivery).await?;
                self.spawn_event(namespace, event_id, message);
                return Ok(());
            }
        };

        self.ack_delivery(&delivery).await?;
        self.spawn_upload(prepared, wav);
        Ok(())
    }

    async fn ack_delivery(&self, delivery: &Delivery) -> Result<()> {
        delivery
            .ack(BasicAckOptions::default())
            .await
            .map_err(|err| anyhow!("failed to ack delivery: {err}"))
    }

    fn spawn_upload(&self, job: PreparedJob, wav: Vec<u8>) {
        let http = self.http.clone();
        let publisher = self.event_publisher.clone();
        tokio::spawn(async move {
            let message = match upload_audio(&http, &job.destination, wav).await {
                Ok(()) => MESSAGE_ON_COMPLETE.to_string(),
                Err(err) => format_failure_message(&err),
            };
            if let Err(err) = publisher.publish(&job.namespace, &job.event_id, &message).await {
                error!(?err, "failed to publish event");
            }
        });
    }

    fn spawn_event(&self, namespace: String, event_id: String, message: String) {
        let publisher = self.event_publisher.clone();
        tokio::spawn(async move {
            if let Err(err) = publisher.publish(&namespace, &event_id, &message).await {
                error!(?err, "failed to publish event");
            }
        });
    }

    fn load_model(&mut self, speaker_id: u32, model_path: &Path) -> Result<()> {
        if let Some(current) = self.current_model.take() {
            if let Err(err) = self.synth.synthesizer.unload_voice_model(current.model_id) {
                warn!(?err, "failed to unload previous model");
            }
        }

        let model_file = VoiceModelFile::open(model_path).with_context(|| {
            format!("failed to open voice model at {}", model_path.display())
        })?;
        self.synth
            .synthesizer
            .load_voice_model(&model_file)
            .context("failed to load voice model")?;
        let model_id = model_file.id();

        self.current_model = Some(LoadedModel {
            speaker_id,
            model_id,
            model_file,
        });
        Ok(())
    }
}

struct LoadedModel {
    speaker_id: u32,
    model_id: VoiceModelId,
    model_file: VoiceModelFile,
}

struct SynthResources {
    synthesizer: Synthesizer<OpenJtalk>,
}

impl SynthResources {
    fn new(cfg: &Config) -> Result<Self> {
        ensure_exists(&cfg.onnxruntime_path, "onnxruntime library")?;
        ensure_exists(&cfg.dict_path, "open_jtalk dictionary")?;

        let dict_path_str = cfg
            .dict_path
            .to_str()
            .ok_or_else(|| anyhow!("dictionary path must be valid UTF-8"))?;

        let ort = Onnxruntime::load_once()
            .filename(&cfg.onnxruntime_path)
            .perform()
            .context("failed to load onnxruntime")?;
        let text_analyzer =
            OpenJtalk::new(dict_path_str).context("failed to initialize OpenJTalk")?;
        let synthesizer = Synthesizer::builder(ort)
            .text_analyzer(text_analyzer)
            .build()
            .context("failed to build synthesizer")?;

        Ok(Self { synthesizer })
    }

    fn synthesize(&self, text: &str, speaker_id: u32) -> Result<Vec<u8>> {
        self.synthesizer
            .tts(text, StyleId::new(speaker_id))
            .perform()
            .map_err(|err| anyhow!("voice synthesis failed: {err}"))
    }
}

#[derive(Clone)]
struct EventPublisher {
    channel: Channel,
    declared_namespaces: std::sync::Arc<Mutex<HashSet<String>>>,
}

impl EventPublisher {
    fn new(channel: Channel) -> Self {
        Self {
            channel,
            declared_namespaces: std::sync::Arc::new(Mutex::new(HashSet::new())),
        }
    }

    async fn publish(&self, namespace: &str, event_id: &str, message: &str) -> Result<()> {
        self.ensure_namespace_bound(namespace).await?;
        let payload = json!({
            "namespace": namespace,
            "id": event_id,
            "message": message,
        });
        let payload = serde_json::to_vec(&payload)
            .map_err(|err| anyhow!("failed to encode event payload: {err}"))?;

        self.channel
            .basic_publish(
                EVENT_EXCHANGE,
                namespace,
                BasicPublishOptions::default(),
                &payload,
                BasicProperties::default()
                    .with_delivery_mode(2)
                    .with_content_type("application/json".into()),
            )
            .await
            .map_err(|err| anyhow!("failed to publish event: {err}"))?
            .await
            .map_err(|err| anyhow!("event publish not confirmed: {err:?}"))?;

        Ok(())
    }

    async fn ensure_namespace_bound(&self, namespace: &str) -> Result<()> {
        {
            let declared = self.declared_namespaces.lock().await;
            if declared.contains(namespace) {
                return Ok(());
            }
        }

        let queue_name = format!("qrpc_event_{namespace}");
        let mut queue_args = FieldTable::default();
        queue_args.insert("x-message-ttl".into(), EVENT_QUEUE_TTL_MS.into());
        queue_args.insert("x-expires".into(), EVENT_QUEUE_EXPIRES_MS.into());

        self.channel
            .queue_declare(
                &queue_name,
                QueueDeclareOptions {
                    durable: true,
                    ..Default::default()
                },
                queue_args,
            )
            .await
            .map_err(|err| anyhow!("failed to declare qrpc_event queue: {err}"))?;

        self.channel
            .queue_bind(
                &queue_name,
                EVENT_EXCHANGE,
                namespace,
                QueueBindOptions::default(),
                FieldTable::default(),
            )
            .await
            .map_err(|err| anyhow!("failed to bind qrpc_event queue: {err}"))?;

        let mut declared = self.declared_namespaces.lock().await;
        declared.insert(namespace.to_owned());
        Ok(())
    }
}

#[derive(Debug)]
struct Args {
    config_path: PathBuf,
}

impl Args {
    fn parse() -> Result<Self> {
        let mut config_path = None;
        let mut args = env::args().skip(1);
        while let Some(arg) = args.next() {
            if arg == CONFIG_ARG {
                let value = args
                    .next()
                    .ok_or_else(|| anyhow!("{CONFIG_ARG} requires a path"))?;
                config_path = Some(PathBuf::from(value));
                continue;
            }

            let prefix = format!("{CONFIG_ARG}=");
            if let Some(value) = arg.strip_prefix(&prefix) {
                config_path = Some(PathBuf::from(value));
                continue;
            }

            if arg == TLGRF_TAGS_ARG || arg == TLGRF_TAGS_ARG_ALT {
                args.next()
                    .ok_or_else(|| anyhow!("{arg} requires a JSON object"))?;
                continue;
            }

            let tags_prefix = format!("{TLGRF_TAGS_ARG}=");
            if arg.strip_prefix(&tags_prefix).is_some() {
                continue;
            }

            let tags_prefix_alt = format!("{TLGRF_TAGS_ARG_ALT}=");
            if arg.strip_prefix(&tags_prefix_alt).is_some() {
                continue;
            }
        }

        Ok(Self {
            config_path: config_path.unwrap_or_else(|| PathBuf::from(DEFAULT_CONFIG_PATH)),
        })
    }
}

#[derive(Debug, Deserialize)]
struct FileConfig {
    amqp_addr: Option<String>,
    assets_dir: Option<PathBuf>,
    dict_path: Option<PathBuf>,
    onnxruntime_path: Option<PathBuf>,
    http_timeout_secs: Option<u64>,
    speakers: Option<Vec<SpeakerConfig>>,
}

#[derive(Debug, Deserialize)]
struct SpeakerConfig {
    id: u32,
    model: PathBuf,
}

struct Config {
    amqp_addr: String,
    dict_path: PathBuf,
    onnxruntime_path: PathBuf,
    http_timeout_secs: u64,
    speaker_models: HashMap<u32, PathBuf>,
}

impl Config {
    fn load(path: &Path) -> Result<Self> {
        let raw = fs::read_to_string(path)
            .with_context(|| format!("failed to read config at {}", path.display()))?;
        let cfg: FileConfig = toml::from_str(&raw).context("failed to parse config")?;

        let assets_dir = cfg
            .assets_dir
            .unwrap_or_else(|| PathBuf::from(DEFAULT_ASSETS_DIR));
        let dict_path = resolve_path(&assets_dir, cfg.dict_path, Some(DEFAULT_DICT_DIR))?;
        let default_onnx = assets_dir
            .join("onnxruntime")
            .join("lib")
            .join(Onnxruntime::LIB_VERSIONED_FILENAME);
        let onnxruntime_path = match cfg.onnxruntime_path {
            Some(path) => resolve_path(&assets_dir, Some(path), None)?,
            None => default_onnx,
        };
        let http_timeout_secs = cfg
            .http_timeout_secs
            .unwrap_or(DEFAULT_HTTP_TIMEOUT_SECS);

        let mut speaker_models = HashMap::new();
        let speakers = cfg.speakers.unwrap_or_default();
        if speakers.is_empty() {
            bail!("config must include at least one speaker entry");
        }
        for speaker in speakers {
            if speaker_models.contains_key(&speaker.id) {
                bail!("duplicate speaker id {} in config", speaker.id);
            }
            let model_path = resolve_path(&assets_dir, Some(speaker.model), None)?;
            ensure_exists(&model_path, "voice model")?;
            speaker_models.insert(speaker.id, model_path);
        }

        Ok(Self {
            amqp_addr: cfg
                .amqp_addr
                .unwrap_or_else(|| DEFAULT_AMQP_ADDR.to_string()),
            dict_path,
            onnxruntime_path,
            http_timeout_secs,
            speaker_models,
        })
    }

    fn sorted_speaker_ids(&self) -> Vec<u32> {
        let mut ids: Vec<u32> = self.speaker_models.keys().copied().collect();
        ids.sort_unstable();
        ids
    }
}

#[derive(Debug, Deserialize)]
struct JobPayload {
    speaker_id: u32,
    text: String,
    destination_url: String,
    qrpc_event_namespace: String,
    qrpc_event_id: String,
}

impl JobPayload {
    fn prepare(self) -> Result<PreparedJob> {
        if self.text.trim().is_empty() {
            bail!("text must not be empty");
        }

        if self.text.len() >= MAX_TEXT_BYTES {
            bail!(
                "text too long ({} bytes, limit is {})",
                self.text.len(),
                MAX_TEXT_BYTES - 1
            );
        }

        let destination =
            Url::parse(&self.destination_url).context("invalid destination_url")?;

        Ok(PreparedJob {
            speaker_id: self.speaker_id,
            text: self.text,
            destination,
            namespace: self.qrpc_event_namespace,
            event_id: self.qrpc_event_id,
        })
    }
}

struct PreparedJob {
    speaker_id: u32,
    text: String,
    destination: Url,
    namespace: String,
    event_id: String,
}

async fn upload_audio(http: &Client, destination: &Url, wav: Vec<u8>) -> Result<()> {
    let response = http
        .put(destination.clone())
        .header(CONTENT_TYPE, "audio/wav")
        .body(wav)
        .send()
        .await
        .map_err(|err| {
            if err.is_timeout() {
                anyhow!("upload failed (timeout)")
            } else {
                anyhow!("upload failed")
            }
        })?;

    let status = response.status();
    if status.is_success() {
        Ok(())
    } else {
        Err(anyhow!("upload failed with status {}", status))
    }
}

fn job_queue_name(speaker_id: u32) -> String {
    format!("{JOB_QUEUE_PREFIX}{speaker_id}")
}

fn format_failure_message(err: impl std::fmt::Display) -> String {
    format!("{MESSAGE_ON_FAILED}: {err}")
}

fn resolve_path(
    assets_dir: &Path,
    value: Option<PathBuf>,
    default_rel: Option<&str>,
) -> Result<PathBuf> {
    if let Some(path) = value {
        if path.is_absolute() {
            return Ok(path);
        }
        return Ok(assets_dir.join(path));
    }

    match default_rel {
        Some(default) => Ok(assets_dir.join(default)),
        None => bail!("path is required"),
    }
}

fn ensure_exists(path: &Path, label: &str) -> Result<()> {
    if path.exists() {
        Ok(())
    } else {
        bail!("{label} not found at {}", path.display());
    }
}
