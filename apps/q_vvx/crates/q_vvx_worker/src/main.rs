use std::{
    collections::{HashMap, HashSet},
    env,
    fs,
    io,
    path::{Path, PathBuf},
    sync::Arc,
    time::{Duration, Instant},
};
use std::ffi::CString;

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
use tlgrf::{FieldValue, TelegrafClient};
use tokio::sync::Mutex;
use tracing::{error, info, warn};
use tracing_subscriber::EnvFilter;
use voicevox_core::{
    blocking::{Onnxruntime, OpenJtalk, Synthesizer, VoiceModelFile},
    AccelerationMode,
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
const MESSAGE_ON_SYNTH_START: &str = "q_vvx_worker:synth_start";
const MESSAGE_ON_SYNTH_END: &str = "q_vvx_worker:synth_end";
const MAX_TEXT_BYTES: usize = 1500;
const EVENT_QUEUE_TTL_MS: i64 = 3_600_000;
const EVENT_QUEUE_EXPIRES_MS: i64 = 3_900_000;
const CONFIG_ARG: &str = "--config";
const GPU_ARG: &str = "--gpu";
const TLGRF_TAGS_ARG: &str = "--tlgrf_tegs";
const TLGRF_TAGS_ARG_ALT: &str = "--tlgrf_tags";
const WORKER_ID_TAG: &str = "q_vvx_worker_index";
const DEFAULT_TELEGRAF_SOCKET: &str = "/tmp/telegraf.sock";
const METRICS_MEASUREMENT: &str = "q_vvx_worker";
const METRICS_KIND_TAG: &str = "metric_kind";
const METRICS_KIND_JOB: &str = "job";
const METRICS_KIND_STARTUP: &str = "startup";
const METRICS_KIND_COUNTS: &str = "counts";
const PACKAGE_VERSION: &str = env!("CARGO_PKG_VERSION");

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info"));
    tracing_subscriber::fmt().with_env_filter(filter).compact().init();

    let Args {
        config_path,
        tlgrf_tags,
        enable_gpu,
    } = Args::parse()?;
    let cfg = Config::load(&config_path)?;
    let (synth, startup) = SynthResources::new(&cfg, enable_gpu)?;
    let worker_id = tlgrf_tags.get(WORKER_ID_TAG).cloned();
    let metrics = Metrics::new(tlgrf_tags, &synth);
    metrics.record_startup(startup).await;
    let http = Client::builder()
        .timeout(Duration::from_secs(cfg.http_timeout_secs))
        .build()
        .context("failed to build HTTP client")?;

    Worker::new(cfg, synth, http, metrics, worker_id)
        .await?
        .run()
        .await
}

struct Worker {
    cfg: Config,
    synth: SynthResources,
    http: Client,
    _conn: Connection,
    job_channel: Channel,
    wake_consumer: lapin::Consumer,
    event_publisher: EventPublisher,
    metrics: Metrics,
    style_ids: Vec<u32>,
    current_model: Option<LoadedModel>,
}

impl Worker {
    async fn new(
        cfg: Config,
        synth: SynthResources,
        http: Client,
        metrics: Metrics,
        worker_id: Option<String>,
    ) -> Result<Self> {
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

        for style_id in cfg.style_models.keys() {
            let queue_name = job_queue_name(*style_id);
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
                    &style_id.to_string(),
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

        let style_ids = cfg.sorted_style_ids();

        Ok(Self {
            cfg,
            synth,
            http,
            _conn: conn,
            job_channel,
            wake_consumer,
            event_publisher: EventPublisher::new(event_channel, worker_id),
            metrics,
            style_ids,
            current_model: None,
        })
    }

    async fn run(mut self) -> Result<()> {
        info!(
            styles = self.style_ids.len(),
            "worker ready for jobs"
        );

        loop {
            if let Some(style_id) = self.current_model.as_ref().map(|model| model.style_id) {
                if let Some(delivery) = self.try_get_job(style_id).await? {
                    self.process_delivery(delivery).await?;
                    continue;
                }
            }

            match self.select_style_with_messages().await? {
                Some(style_id) => {
                    if let Some(delivery) = self.try_get_job(style_id).await? {
                        self.process_delivery(delivery).await?;
                    }
                }
                None => {
                    self.wait_for_wake().await?;
                }
            }
        }
    }

    async fn try_get_job(&self, style_id: u32) -> Result<Option<Delivery>> {
        let queue_name = job_queue_name(style_id);
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

    async fn select_style_with_messages(&self) -> Result<Option<u32>> {
        let mut max_count = 0u32;
        let mut candidates = Vec::new();

        for style_id in &self.style_ids {
            let queue_name = job_queue_name(*style_id);
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
                candidates.push(*style_id);
            } else if count == max_count {
                candidates.push(*style_id);
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
                self.metrics.record_drop().await;
                return self.ack_delivery(&delivery).await;
            }
        };

        let namespace = payload.qrpc_event_namespace.clone();
        let event_id = payload.qrpc_event_id.clone();
        if namespace.trim().is_empty() || event_id.trim().is_empty() {
            warn!("dropping job with invalid event identifiers");
            self.metrics.record_drop().await;
            return self.ack_delivery(&delivery).await;
        }

        let prepared = match payload.prepare() {
            Ok(job) => job,
            Err(err) => {
                let message = format_failure_message(&err);
                self.metrics.record_drop().await;
                self.ack_delivery(&delivery).await?;
                self.spawn_event(namespace, event_id, message);
                return Ok(());
            }
        };

        let job_timer = Instant::now();
        let text_chars = prepared.text.chars().count() as i64;
        let text_bytes = prepared.text.len() as i64;

        let model_path = match self.cfg.style_models.get(&prepared.style_id) {
            Some(path) => path.clone(),
            None => {
                let message = format_failure_message(&format!(
                    "unknown style_id {}",
                    prepared.style_id
                ));
                self.metrics.record_drop().await;
                self.ack_delivery(&delivery).await?;
                self.spawn_event(namespace, event_id, message);
                return Ok(());
            }
        };
        let model_tag = model_tag_value(&model_path);

        if self
            .current_model
            .as_ref()
            .map(|model| model.style_id)
            != Some(prepared.style_id)
        {
            if let Err(err) = self.load_model(prepared.style_id, &model_path) {
                warn!(
                    ?err,
                    style_id = prepared.style_id,
                    model_path = %model_path.display(),
                    "failed to load voice model"
                );
                let message = format_failure_message(&format!("model load failed: {err}"));
                self.metrics.record_drop().await;
                self.ack_delivery(&delivery).await?;
                self.spawn_event(namespace, event_id, message);
                return Ok(());
            }
        }

        self.publish_event(
            &namespace,
            &synth_event_id(&event_id, "synth_start"),
            MESSAGE_ON_SYNTH_START,
        )
        .await;
        let synth_timer = Instant::now();
        let wav = match self.synth.synthesize(&prepared.text, prepared.style_id) {
            Ok(wav) => wav,
            Err(err) => {
                let message = format_failure_message(&format!("synthesis failed: {err}"));
                self.metrics.record_drop().await;
                self.ack_delivery(&delivery).await?;
                self.spawn_event(namespace, event_id, message);
                return Ok(());
            }
        };
        self.publish_event(
            &namespace,
            &synth_event_id(&event_id, "synth_end"),
            MESSAGE_ON_SYNTH_END,
        )
        .await;
        let synth_duration_ms = elapsed_ms(synth_timer);
        let wav_bytes = wav.len() as i64;

        self.ack_delivery(&delivery).await?;
        self.spawn_upload(
            prepared,
            wav,
            JobTiming {
                job_start: job_timer,
                text_chars,
                text_bytes,
                synth_duration_ms,
                wav_bytes,
                model_tag,
            },
        );
        Ok(())
    }

    async fn ack_delivery(&self, delivery: &Delivery) -> Result<()> {
        delivery
            .ack(BasicAckOptions::default())
            .await
            .map_err(|err| anyhow!("failed to ack delivery: {err}"))
    }

    fn spawn_upload(&self, job: PreparedJob, wav: Vec<u8>, timing: JobTiming) {
        let http = self.http.clone();
        let publisher = self.event_publisher.clone();
        let metrics = self.metrics.clone();
        tokio::spawn(async move {
            let upload_timer = Instant::now();
            let upload_result = upload_audio(&http, &job.destination, wav).await;
            let upload_duration_ms = elapsed_ms(upload_timer);
            let message = match &upload_result {
                Ok(()) => MESSAGE_ON_COMPLETE.to_string(),
                Err(err) => format_failure_message(err),
            };
            if let Err(err) = publisher.publish(&job.namespace, &job.event_id, &message).await {
                error!(?err, "failed to publish event");
            }

            match upload_result {
                Ok(()) => {
                    let job_duration_ms = elapsed_ms(timing.job_start);
                    metrics
                        .record_job(
                            JobMetrics {
                                text_chars: timing.text_chars,
                                text_bytes: timing.text_bytes,
                                synth_duration_ms: timing.synth_duration_ms,
                                wav_bytes: timing.wav_bytes,
                                upload_duration_ms,
                                job_duration_ms,
                            },
                            timing.model_tag,
                        )
                        .await;
                }
                Err(_) => {
                    metrics.record_drop().await;
                }
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

    async fn publish_event(&self, namespace: &str, event_id: &str, message: &str) {
        if let Err(err) = self.event_publisher.publish(namespace, event_id, message).await {
            error!(?err, "failed to publish event");
        }
    }

    fn load_model(&mut self, style_id: u32, model_path: &Path) -> Result<()> {
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
            style_id,
            model_id,
        });
        Ok(())
    }
}

struct LoadedModel {
    style_id: u32,
    model_id: VoiceModelId,
}

struct SynthResources {
    synthesizer: Synthesizer<OpenJtalk>,
}

impl SynthResources {
    fn new(cfg: &Config, enable_gpu: bool) -> Result<(Self, StartupTimings)> {
        let startup_timer = Instant::now();
        ensure_exists(&cfg.onnxruntime_path, "onnxruntime library")?;
        ensure_exists(&cfg.dict_path, "open_jtalk dictionary")?;
        preload_additional_libraries(&cfg.assets_dir);

        let dict_path_str = cfg
            .dict_path
            .to_str()
            .ok_or_else(|| anyhow!("dictionary path must be valid UTF-8"))?;

        let onnx_timer = Instant::now();
        let ort = Onnxruntime::load_once()
            .filename(&cfg.onnxruntime_path)
            .perform()
            .context("failed to load onnxruntime")?;
        let onnx_load_ms = elapsed_ms(onnx_timer);
        let openjtalk_timer = Instant::now();
        let text_analyzer =
            OpenJtalk::new(dict_path_str).context("failed to initialize OpenJTalk")?;
        let openjtalk_init_ms = elapsed_ms(openjtalk_timer);
        let synthesizer_timer = Instant::now();
        let mut builder = Synthesizer::builder(ort).text_analyzer(text_analyzer);
        if !enable_gpu {
            builder = builder.acceleration_mode(AccelerationMode::Cpu);
        }
        let synthesizer = builder.build().context("failed to build synthesizer")?;
        let synthesizer_build_ms = elapsed_ms(synthesizer_timer);
        let startup_total_ms = elapsed_ms(startup_timer);

        Ok((
            Self { synthesizer },
            StartupTimings {
                startup_total_ms,
                onnx_load_ms,
                openjtalk_init_ms,
                synthesizer_build_ms,
                voice_model_open_ms: 0,
                voice_model_load_ms: 0,
            },
        ))
    }

    fn synthesize(&self, text: &str, style_id: u32) -> Result<Vec<u8>> {
        self.synthesizer
            .tts(text, StyleId::new(style_id))
            .perform()
            .map_err(|err| anyhow!("voice synthesis failed: {err}"))
    }

    fn is_gpu_mode(&self) -> bool {
        self.synthesizer.is_gpu_mode()
    }
}

struct StartupTimings {
    startup_total_ms: i64,
    onnx_load_ms: i64,
    openjtalk_init_ms: i64,
    synthesizer_build_ms: i64,
    voice_model_open_ms: i64,
    voice_model_load_ms: i64,
}

struct JobMetrics {
    text_chars: i64,
    text_bytes: i64,
    synth_duration_ms: i64,
    wav_bytes: i64,
    upload_duration_ms: i64,
    job_duration_ms: i64,
}

struct JobTiming {
    job_start: Instant,
    text_chars: i64,
    text_bytes: i64,
    synth_duration_ms: i64,
    wav_bytes: i64,
    model_tag: String,
}

#[derive(Clone)]
struct Metrics {
    state: Arc<Mutex<MetricsState>>,
}

struct MetricsState {
    client: Option<TelegrafClient>,
    tags: Vec<(String, String)>,
    jobs_retried_total: i64,
    jobs_dropped_total: i64,
}

impl Metrics {
    fn new(tlgrf_tags: HashMap<String, String>, synth: &SynthResources) -> Self {
        let tags = build_metric_tags(tlgrf_tags, synth);
        let client = if Path::new(DEFAULT_TELEGRAF_SOCKET).exists() {
            match TelegrafClient::new(DEFAULT_TELEGRAF_SOCKET) {
                Ok(client) => Some(client),
                Err(err) => {
                    warn!(
                        ?err,
                        socket = %DEFAULT_TELEGRAF_SOCKET,
                        "metrics disabled: failed to connect to telegraf socket"
                    );
                    None
                }
            }
        } else {
            info!(
                socket = %DEFAULT_TELEGRAF_SOCKET,
                "metrics disabled: telegraf socket not found"
            );
            None
        };

        Self {
            state: Arc::new(Mutex::new(MetricsState {
                client,
                tags,
                jobs_retried_total: 0,
                jobs_dropped_total: 0,
            })),
        }
    }

    async fn record_startup(&self, startup: StartupTimings) {
        let fields = [
            ("startup_total_ms", FieldValue::Integer(startup.startup_total_ms)),
            ("onnx_load_ms", FieldValue::Integer(startup.onnx_load_ms)),
            ("openjtalk_init_ms", FieldValue::Integer(startup.openjtalk_init_ms)),
            (
                "synthesizer_build_ms",
                FieldValue::Integer(startup.synthesizer_build_ms),
            ),
            (
                "voice_model_open_ms",
                FieldValue::Integer(startup.voice_model_open_ms),
            ),
            (
                "voice_model_load_ms",
                FieldValue::Integer(startup.voice_model_load_ms),
            ),
        ];
        self.send(METRICS_KIND_STARTUP, &fields, None).await;
    }

    async fn record_job(&self, metrics: JobMetrics, model_tag: String) {
        let fields = [
            ("text_chars", FieldValue::Integer(metrics.text_chars)),
            ("text_bytes", FieldValue::Integer(metrics.text_bytes)),
            (
                "synth_duration_ms",
                FieldValue::Integer(metrics.synth_duration_ms),
            ),
            ("wav_bytes", FieldValue::Integer(metrics.wav_bytes)),
            (
                "upload_duration_ms",
                FieldValue::Integer(metrics.upload_duration_ms),
            ),
            ("job_duration_ms", FieldValue::Integer(metrics.job_duration_ms)),
        ];
        self.send(
            METRICS_KIND_JOB,
            &fields,
            Some(vec![("model".to_string(), model_tag)]),
        )
        .await;
    }

    async fn record_drop(&self) {
        let mut state = self.state.lock().await;
        state.jobs_dropped_total += 1;
        let fields = [
            (
                "jobs_retried_total",
                FieldValue::Integer(state.jobs_retried_total),
            ),
            (
                "jobs_dropped_total",
                FieldValue::Integer(state.jobs_dropped_total),
            ),
        ];
        state.send(METRICS_KIND_COUNTS, &fields, None);
    }

    async fn send(
        &self,
        kind: &str,
        fields: &[(&str, FieldValue<'_>)],
        extra_tags: Option<Vec<(String, String)>>,
    ) {
        let mut state = self.state.lock().await;
        state.send(kind, fields, extra_tags);
    }
}

impl MetricsState {
    fn send(
        &mut self,
        kind: &str,
        fields: &[(&str, FieldValue<'_>)],
        extra_tags: Option<Vec<(String, String)>>,
    ) {
        let mut tags = self.tags.clone();
        tags.retain(|(key, _)| key != METRICS_KIND_TAG);
        if let Some(extra_tags) = extra_tags {
            for (key, _) in &extra_tags {
                tags.retain(|(existing, _)| existing != key);
            }
            tags.extend(extra_tags);
        }
        tags.push((METRICS_KIND_TAG.to_string(), kind.to_string()));

        let tag_refs: Vec<(&str, &str)> =
            tags.iter().map(|(key, value)| (key.as_str(), value.as_str())).collect();
        let result = match self.client.as_ref() {
            Some(client) => client.send_metric(METRICS_MEASUREMENT, &tag_refs, fields, None),
            None => return,
        };
        if let Err(err) = result {
            warn!(
                ?err,
                measurement = METRICS_MEASUREMENT,
                metric_kind = kind,
                "failed to send metric"
            );
            self.client = None;
        }
    }
}

#[derive(Clone)]
struct EventPublisher {
    channel: Channel,
    declared_namespaces: std::sync::Arc<Mutex<HashSet<String>>>,
    worker_id: Option<String>,
}

impl EventPublisher {
    fn new(channel: Channel, worker_id: Option<String>) -> Self {
        Self {
            channel,
            declared_namespaces: std::sync::Arc::new(Mutex::new(HashSet::new())),
            worker_id,
        }
    }

    async fn publish(&self, namespace: &str, event_id: &str, message: &str) -> Result<()> {
        self.ensure_namespace_bound(namespace).await?;
        let mut payload = json!({
            "namespace": namespace,
            "id": event_id,
            "message": message,
        });
        if let Some(worker_id) = self.worker_id.as_deref() {
            if let Some(obj) = payload.as_object_mut() {
                obj.insert("worker_id".to_string(), json!(worker_id));
            }
        }
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
    tlgrf_tags: HashMap<String, String>,
    enable_gpu: bool,
}

impl Args {
    fn parse() -> Result<Self> {
        let mut config_path = None;
        let mut tlgrf_tags = HashMap::new();
        let mut enable_gpu = false;
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
                let value = args.next().ok_or_else(|| {
                    anyhow!("{arg} requires a JSON object like '{{\"name\":\"prod\"}}'")
                })?;
                merge_tlgrf_tags_json(&mut tlgrf_tags, &value)?;
                continue;
            }

            let tags_prefix = format!("{TLGRF_TAGS_ARG}=");
            if let Some(value) = arg.strip_prefix(&tags_prefix) {
                merge_tlgrf_tags_json(&mut tlgrf_tags, value)?;
                continue;
            }

            let tags_prefix_alt = format!("{TLGRF_TAGS_ARG_ALT}=");
            if let Some(value) = arg.strip_prefix(&tags_prefix_alt) {
                merge_tlgrf_tags_json(&mut tlgrf_tags, value)?;
                continue;
            }

            if arg == GPU_ARG {
                enable_gpu = true;
                continue;
            }
        }

        Ok(Self {
            config_path: config_path.unwrap_or_else(|| PathBuf::from(DEFAULT_CONFIG_PATH)),
            tlgrf_tags,
            enable_gpu,
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
}

struct Config {
    amqp_addr: String,
    assets_dir: PathBuf,
    dict_path: PathBuf,
    onnxruntime_path: PathBuf,
    http_timeout_secs: u64,
    style_models: HashMap<u32, PathBuf>,
}

impl Config {
    fn load(path: &Path) -> Result<Self> {
        let cfg = match fs::read_to_string(path) {
            Ok(raw) => toml::from_str(&raw).context("failed to parse config")?,
            Err(err) if err.kind() == io::ErrorKind::NotFound => FileConfig {
                amqp_addr: None,
                assets_dir: None,
                dict_path: None,
                onnxruntime_path: None,
                http_timeout_secs: None,
            },
            Err(err) => {
                return Err(err)
                    .with_context(|| format!("failed to read config at {}", path.display()))
            }
        };

        let FileConfig {
            amqp_addr,
            assets_dir,
            dict_path,
            onnxruntime_path,
            http_timeout_secs,
        } = cfg;

        let assets_dir = assets_dir.unwrap_or_else(|| PathBuf::from(DEFAULT_ASSETS_DIR));
        let dict_path = resolve_path(&assets_dir, dict_path, Some(DEFAULT_DICT_DIR))?;
        let default_onnx = assets_dir
            .join("onnxruntime")
            .join("lib")
            .join(Onnxruntime::LIB_VERSIONED_FILENAME);
        let onnxruntime_path = match onnxruntime_path {
            Some(path) => resolve_path(&assets_dir, Some(path), None)?,
            None => default_onnx,
        };
        let http_timeout_secs = http_timeout_secs.unwrap_or(DEFAULT_HTTP_TIMEOUT_SECS);

        let style_models = discover_style_models(&assets_dir)?;

        Ok(Self {
            amqp_addr: amqp_addr.unwrap_or_else(|| DEFAULT_AMQP_ADDR.to_string()),
            assets_dir,
            dict_path,
            onnxruntime_path,
            http_timeout_secs,
            style_models,
        })
    }

    fn sorted_style_ids(&self) -> Vec<u32> {
        let mut ids: Vec<u32> = self.style_models.keys().copied().collect();
        ids.sort_unstable();
        ids
    }
}

#[derive(Debug, Deserialize)]
struct JobPayload {
    style_id: u32,
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
            style_id: self.style_id,
            text: self.text,
            destination,
            namespace: self.qrpc_event_namespace,
            event_id: self.qrpc_event_id,
        })
    }
}

struct PreparedJob {
    style_id: u32,
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

fn job_queue_name(style_id: u32) -> String {
    format!("{JOB_QUEUE_PREFIX}{style_id}")
}

fn synth_event_id(event_id: &str, suffix: &str) -> String {
    format!("{event_id}:{suffix}")
}

fn format_failure_message(err: impl std::fmt::Display) -> String {
    format!("{MESSAGE_ON_FAILED}: {err}")
}

fn build_metric_tags(
    tlgrf_tags: HashMap<String, String>,
    synth: &SynthResources,
) -> Vec<(String, String)> {
    let mut tags = HashMap::new();
    tags.insert("host".to_string(), host_tag_value());
    tags.insert("model".to_string(), "dynamic".to_string());
    tags.insert("version".to_string(), PACKAGE_VERSION.to_string());
    tags.insert(
        "gpu".to_string(),
        if synth.is_gpu_mode() {
            "true".to_string()
        } else {
            "false".to_string()
        },
    );
    for (key, value) in tlgrf_tags {
        tags.insert(key, value);
    }
    tags.into_iter().collect()
}

fn host_tag_value() -> String {
    env::var("HOSTNAME")
        .or_else(|_| env::var("HOST"))
        .unwrap_or_else(|_| "unknown".to_string())
}

fn model_tag_value(path: &Path) -> String {
    path.file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_else(|| "unknown".to_string())
}

fn merge_tlgrf_tags_json(tags: &mut HashMap<String, String>, input: &str) -> Result<()> {
    let raw: serde_json::Map<String, serde_json::Value> =
        serde_json::from_str(input).context("tlgrf tags must be a JSON object")?;
    for (key, value) in raw {
        let value = value
            .as_str()
            .ok_or_else(|| anyhow!("tlgrf tag value for '{key}' must be a JSON string"))?;
        tags.insert(key, value.to_string());
    }
    Ok(())
}

fn elapsed_ms(start: Instant) -> i64 {
    i64::try_from(start.elapsed().as_millis()).unwrap_or(i64::MAX)
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

fn preload_additional_libraries(assets_dir: &Path) {
    let additional_dir = assets_dir.join("additional_libraries");
    let entries = match fs::read_dir(&additional_dir) {
        Ok(entries) => entries,
        Err(err) => {
            if err.kind() != io::ErrorKind::NotFound {
                warn!(
                    ?err,
                    path = %additional_dir.display(),
                    "failed to read additional_libraries directory"
                );
            }
            return;
        }
    };

    let mut libs: Vec<PathBuf> = entries
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .filter(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .map(|name| name.contains(".so"))
                .unwrap_or(false)
        })
        .collect();
    libs.sort_by_key(|path| {
        let name = path.file_name().and_then(|name| name.to_str()).unwrap_or("");
        if name == "libcudnn_ops_infer.so.8" {
            0
        } else {
            1
        }
    });

    for path in libs {
        let c_path = match CString::new(path.to_string_lossy().as_ref()) {
            Ok(value) => value,
            Err(_) => {
                warn!(path = %path.display(), "skipping invalid library path");
                continue;
            }
        };
        unsafe {
            let handle = libc::dlopen(c_path.as_ptr(), libc::RTLD_NOW | libc::RTLD_GLOBAL);
            if handle.is_null() {
                let err = libc::dlerror();
                let detail = if err.is_null() {
                    "unknown error".to_string()
                } else {
                    let msg = std::ffi::CStr::from_ptr(err);
                    msg.to_string_lossy().into_owned()
                };
                warn!(
                    path = %path.display(),
                    error = %detail,
                    "failed to preload additional library"
                );
            } else {
                info!(path = %path.display(), "preloaded additional library");
            }
        }
    }
}


fn collect_style_ids(model_path: &Path) -> Result<HashSet<u32>> {
    let model_file = VoiceModelFile::open(model_path)
        .with_context(|| format!("failed to open voice model at {}", model_path.display()))?;
    let mut ids = HashSet::new();
    for character in model_file.metas() {
        for style in &character.styles {
            ids.insert(style.id.into());
        }
    }
    if ids.is_empty() {
        bail!(
            "no style ids found in voice model {}",
            model_path.display()
        );
    }
    Ok(ids)
}

fn discover_style_models(assets_dir: &Path) -> Result<HashMap<u32, PathBuf>> {
    let models_dir = assets_dir.join("models").join("vvms");
    let entries = fs::read_dir(&models_dir)
        .with_context(|| format!("failed to read voice model directory {}", models_dir.display()))?;
    let mut style_models: HashMap<u32, PathBuf> = HashMap::new();
    let mut found_model = false;

    for entry in entries {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(|ext| ext.to_str()) != Some("vvm") {
            continue;
        }
        found_model = true;
        let style_ids = collect_style_ids(&path)?;
        for style_id in style_ids {
            if let Some(existing) = style_models.get(&style_id) {
                bail!(
                    "style id {} found in multiple models: {} and {}",
                    style_id,
                    existing.display(),
                    path.display()
                );
            }
            style_models.insert(style_id, path.clone());
        }
    }

    if !found_model {
        bail!(
            "no voice models found in {}",
            models_dir.display()
        );
    }

    if style_models.is_empty() {
        bail!(
            "no style ids found in voice models under {}",
            models_dir.display()
        );
    }

    Ok(style_models)
}
