use std::{
    collections::HashSet,
    env,
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::{anyhow, bail, Context, Result};
use futures_util::StreamExt;
use lapin::{
    message::Delivery,
    options::{
        BasicAckOptions, BasicConsumeOptions, BasicNackOptions, BasicPublishOptions,
        BasicQosOptions, ExchangeDeclareOptions, QueueBindOptions, QueueDeclareOptions,
    },
    types::FieldTable,
    BasicProperties, Channel, Connection, ConnectionProperties, ExchangeKind,
};
use reqwest::{header::CONTENT_TYPE, Client, Url};
use serde::Deserialize;
use serde_json::json;
use tracing::{error, info, warn};
use tracing_subscriber::EnvFilter;
use voicevox_core::{
    blocking::{Onnxruntime, OpenJtalk, Synthesizer, VoiceModelFile},
    StyleId,
};

const DEFAULT_AMQP_ADDR: &str = "amqp://guest:guest@localhost:5672/%2f";
const DEFAULT_QUEUE: &str = "super_simple_tts";
const DEFAULT_ASSETS_DIR: &str = "/opt/qrpc/pkg/share/voicevox_core";
const DEFAULT_MODEL_FILE: &str = "models/vvms/0.vvm";
const DEFAULT_DICT_DIR: &str = "dict/open_jtalk_dic_utf_8-1.11";
const EVENT_EXCHANGE: &str = "qrpc_event_exchange";
const WORKER_EXCHANGE: &str = "super_simple_worker_exchange";
const MESSAGE_ON_COMPLETE: &str = "super_simple_worker:upload_complete";
const MAX_TEXT_BYTES: usize = 1500;
const EXPECTED_SPEAKER_ID: u32 = 3;
const EVENT_QUEUE_TTL_MS: i64 = 3_600_000;
const EVENT_QUEUE_EXPIRES_MS: i64 = 3_900_000;

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info"));
    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .compact()
        .init();

    let cfg = Config::from_env()?;
    let synth = SynthResources::new(&cfg)?;
    let http = Client::builder()
        .timeout(Duration::from_secs(30))
        .build()
        .context("failed to build HTTP client")?;

    Worker::new(cfg, synth, http).run().await
}

struct Worker {
    cfg: Config,
    synth: SynthResources,
    http: Client,
    declared_namespaces: HashSet<String>,
}

impl Worker {
    fn new(cfg: Config, synth: SynthResources, http: Client) -> Self {
        Self {
            cfg,
            synth,
            http,
            declared_namespaces: HashSet::new(),
        }
    }

    async fn run(mut self) -> Result<()> {
        info!(
            amqp = %self.cfg.amqp_addr,
            queue = %self.cfg.queue_name,
            "starting super_simple_worker"
        );

        let conn = Connection::connect(&self.cfg.amqp_addr, ConnectionProperties::default())
            .await
            .context("failed to connect to RabbitMQ")?;
        let channel = conn
            .create_channel()
            .await
            .context("failed to open channel")?;

        channel
            .basic_qos(
                1,
                BasicQosOptions {
                    ..Default::default()
                },
            )
            .await
            .context("failed to set qos")?;

        channel
            .queue_declare(
                &self.cfg.queue_name,
                QueueDeclareOptions {
                    durable: true,
                    ..Default::default()
                },
                FieldTable::default(),
            )
            .await
            .context("failed to declare job queue")?;
        info!("declared job queue");

        channel
            .exchange_declare(
                WORKER_EXCHANGE,
                ExchangeKind::Direct,
                ExchangeDeclareOptions {
                    durable: true,
                    ..Default::default()
                },
                FieldTable::default(),
            )
            .await
            .context("failed to declare worker exchange")?;

        channel
            .queue_bind(
                &self.cfg.queue_name,
                WORKER_EXCHANGE,
                &self.cfg.queue_name,
                QueueBindOptions::default(),
                FieldTable::default(),
            )
            .await
            .context("failed to bind worker queue")?;

        channel
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
        info!("declared qrpc_event exchange");

        let mut consumer = channel
            .basic_consume(
                &self.cfg.queue_name,
                "super_simple_worker",
                BasicConsumeOptions::default(),
                FieldTable::default(),
            )
            .await
            .context("failed to start consumer")?;
        info!("consumer started");

        info!("waiting for jobs");

        while let Some(delivery_result) = consumer.next().await {
            match delivery_result {
                Ok(delivery) => {
                    let decision = self.process_delivery(&channel, &delivery).await;
                    match decision {
                        DeliveryDecision::Ack => {
                            if let Err(err) = delivery.ack(BasicAckOptions::default()).await {
                                error!(?err, "failed to ack delivery");
                                break;
                            }
                        }
                        DeliveryDecision::Drop(err) => {
                            warn!(?err, "dropping job");
                            if let Err(ack_err) = delivery.ack(BasicAckOptions::default()).await {
                                error!(?ack_err, "failed to ack dropped delivery");
                                break;
                            }
                        }
                        DeliveryDecision::Retry(err) => {
                            error!(?err, "job failed, requeueing");
                            if let Err(nack_err) = delivery
                                .nack(BasicNackOptions {
                                    requeue: true,
                                    ..Default::default()
                                })
                                .await
                            {
                                error!(?nack_err, "failed to nack delivery");
                                break;
                            }
                        }
                    }
                }
                Err(err) => {
                    error!(?err, "consumer error");
                    break;
                }
            }
        }

        info!("worker stopped");
        Ok(())
    }

    async fn process_delivery(
        &mut self,
        channel: &Channel,
        delivery: &Delivery,
    ) -> DeliveryDecision {
        let payload: JobPayload = match serde_json::from_slice(&delivery.data) {
            Ok(payload) => payload,
            Err(err) => {
                return DeliveryDecision::Drop(anyhow!("invalid job payload: {err}"));
            }
        };

        let job = match payload.prepare(&self.cfg) {
            Ok(job) => job,
            Err(err) => return DeliveryDecision::from(err),
        };

        match self.run_job(channel, job).await {
            Ok(()) => DeliveryDecision::Ack,
            Err(err) => DeliveryDecision::from(err),
        }
    }

    async fn run_job(&mut self, channel: &Channel, job: PreparedJob) -> JobResult<()> {
        let PreparedJob {
            speaker_id,
            text,
            destination,
            namespace,
            event_id,
        } = job;

        info!(
            %namespace,
            %event_id,
            destination = %destination,
            "processing job"
        );
        let wav = self.synth.synthesize(&text, speaker_id)?;
        info!(
            %namespace,
            %event_id,
            "synthesis complete"
        );
        self.upload_audio(destination.clone(), wav).await?;
        info!(
            %namespace,
            %event_id,
            destination = %destination,
            "upload complete"
        );
        self.send_event(channel, &namespace, &event_id).await?;
        info!(%namespace, %event_id, "job completed");
        Ok(())
    }

    async fn upload_audio(&self, destination: Url, wav: Vec<u8>) -> JobResult<()> {
        let response = self
            .http
            .put(destination.clone())
            .header(CONTENT_TYPE, "audio/wav")
            .body(wav)
            .send()
            .await
            .map_err(|err| JobError::retryable(anyhow!("failed to upload audio: {err}")))?;

        let status = response.status();
        if status.is_success() {
            Ok(())
        } else if status.is_client_error() {
            Err(JobError::invalid(anyhow!(
                "destination {} rejected upload with status {}",
                destination,
                status
            )))
        } else {
            Err(JobError::retryable(anyhow!(
                "destination {} failed with status {}",
                destination,
                status
            )))
        }
    }

    async fn send_event(
        &mut self,
        channel: &Channel,
        namespace: &str,
        event_id: &str,
    ) -> JobResult<()> {
        self.ensure_namespace_bound(channel, namespace).await?;
        let payload = json!({
            "namespace": namespace,
            "id": event_id,
            "message": MESSAGE_ON_COMPLETE
        });
        let payload = serde_json::to_vec(&payload)
            .map_err(|err| JobError::retryable(anyhow!("failed to encode event payload: {err}")))?;

        channel
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
            .map_err(|err| JobError::retryable(anyhow!("failed to publish event: {err}")))?
            .await
            .map_err(|err| JobError::retryable(anyhow!("event publish not confirmed: {err:?}")))?;

        Ok(())
    }

    async fn ensure_namespace_bound(
        &mut self,
        channel: &Channel,
        namespace: &str,
    ) -> JobResult<()> {
        if self.declared_namespaces.contains(namespace) {
            return Ok(());
        }

        let queue_name = format!("qrpc_event_{}", namespace);
        let mut queue_args = FieldTable::default();
        queue_args.insert("x-message-ttl".into(), EVENT_QUEUE_TTL_MS.into());
        queue_args.insert("x-expires".into(), EVENT_QUEUE_EXPIRES_MS.into());

        channel
            .queue_declare(
                &queue_name,
                QueueDeclareOptions {
                    durable: true,
                    ..Default::default()
                },
                queue_args,
            )
            .await
            .map_err(|err| {
                JobError::retryable(anyhow!("failed to declare qrpc_event queue: {err}"))
            })?;

        channel
            .queue_bind(
                &queue_name,
                EVENT_EXCHANGE,
                namespace,
                QueueBindOptions::default(),
                FieldTable::default(),
            )
            .await
            .map_err(|err| {
                JobError::retryable(anyhow!("failed to bind qrpc_event queue: {err}"))
            })?;

        self.declared_namespaces.insert(namespace.to_owned());
        Ok(())
    }
}

#[derive(Debug)]
struct Config {
    amqp_addr: String,
    queue_name: String,
    expected_speaker_id: u32,
    dict_path: PathBuf,
    voice_model_path: PathBuf,
    ort_lib_path: PathBuf,
}

impl Config {
    fn from_env() -> Result<Self> {
        let amqp_addr = env::var("AMQP_ADDR").unwrap_or_else(|_| DEFAULT_AMQP_ADDR.to_string());
        let queue_name = env::var("QUEUE_NAME").unwrap_or_else(|_| DEFAULT_QUEUE.to_string());
        let assets_dir = PathBuf::from(
            env::var("VOICEVOX_ASSETS").unwrap_or_else(|_| DEFAULT_ASSETS_DIR.to_string()),
        );
        let dict_path = env::var("VOICEVOX_DICT")
            .map(PathBuf::from)
            .unwrap_or_else(|_| assets_dir.join(DEFAULT_DICT_DIR));
        let voice_model_path = env::var("VOICEVOX_MODEL")
            .map(PathBuf::from)
            .unwrap_or_else(|_| assets_dir.join(DEFAULT_MODEL_FILE));
        let ort_lib_path = env::var("VOICEVOX_ORT_LIB")
            .map(PathBuf::from)
            .unwrap_or_else(|_| {
                assets_dir
                    .join("onnxruntime")
                    .join("lib")
                    .join(Onnxruntime::LIB_VERSIONED_FILENAME)
            });
        let expected_speaker_id = env::var("EXPECTED_SPEAKER_ID")
            .ok()
            .map(|value| {
                value
                    .parse::<u32>()
                    .context("EXPECTED_SPEAKER_ID must be an unsigned integer")
            })
            .transpose()?
            .unwrap_or(EXPECTED_SPEAKER_ID);

        Ok(Self {
            amqp_addr,
            queue_name,
            expected_speaker_id,
            dict_path,
            voice_model_path,
            ort_lib_path,
        })
    }
}

struct SynthResources {
    synthesizer: Synthesizer<OpenJtalk>,
}

impl SynthResources {
    fn new(cfg: &Config) -> Result<Self> {
        ensure_exists(&cfg.ort_lib_path, "onnxruntime library")?;
        ensure_exists(&cfg.dict_path, "open_jtalk dictionary")?;
        ensure_exists(&cfg.voice_model_path, "voicevox model")?;

        let dict_path_str = cfg
            .dict_path
            .to_str()
            .ok_or_else(|| anyhow!("dictionary path must be valid UTF-8"))?;

        let ort = Onnxruntime::load_once()
            .filename(&cfg.ort_lib_path)
            .perform()
            .context("failed to load onnxruntime")?;
        let text_analyzer =
            OpenJtalk::new(dict_path_str).context("failed to initialize OpenJTalk")?;
        let synthesizer = Synthesizer::builder(ort)
            .text_analyzer(text_analyzer)
            .build()
            .context("failed to build synthesizer")?;
        let model_file = VoiceModelFile::open(&cfg.voice_model_path).with_context(|| {
            format!(
                "failed to open voice model at {}",
                cfg.voice_model_path.display()
            )
        })?;
        synthesizer
            .load_voice_model(&model_file)
            .context("failed to load voice model")?;

        Ok(Self { synthesizer })
    }

    fn synthesize(&self, text: &str, speaker_id: u32) -> JobResult<Vec<u8>> {
        self.synthesizer
            .tts(text, StyleId::new(speaker_id))
            .perform()
            .map_err(|err| JobError::retryable(anyhow!("voice synthesis failed: {err}")))
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
    fn prepare(self, cfg: &Config) -> JobResult<PreparedJob> {
        if self.speaker_id != cfg.expected_speaker_id {
            return Err(JobError::invalid(anyhow!(
                "unexpected speaker_id {} (expected {})",
                self.speaker_id,
                cfg.expected_speaker_id
            )));
        }

        if self.text.trim().is_empty() {
            return Err(JobError::invalid(anyhow!("text must not be empty")));
        }

        if self.text.len() >= MAX_TEXT_BYTES {
            return Err(JobError::invalid(anyhow!(
                "text too long ({} bytes, limit is {})",
                self.text.len(),
                MAX_TEXT_BYTES - 1
            )));
        }

        let destination = Url::parse(&self.destination_url)
            .map_err(|err| JobError::invalid(anyhow!("invalid destination_url: {err}")))?;

        if self.qrpc_event_namespace.trim().is_empty() {
            return Err(JobError::invalid(anyhow!(
                "qrpc_event_namespace must not be empty"
            )));
        }

        if self.qrpc_event_id.trim().is_empty() {
            return Err(JobError::invalid(anyhow!(
                "qrpc_event_id must not be empty"
            )));
        }

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

type JobResult<T> = std::result::Result<T, JobError>;

enum JobError {
    Invalid(anyhow::Error),
    Retryable(anyhow::Error),
}

impl JobError {
    fn invalid<E: Into<anyhow::Error>>(err: E) -> Self {
        JobError::Invalid(err.into())
    }

    fn retryable<E: Into<anyhow::Error>>(err: E) -> Self {
        JobError::Retryable(err.into())
    }
}

enum DeliveryDecision {
    Ack,
    Drop(anyhow::Error),
    Retry(anyhow::Error),
}

impl From<JobError> for DeliveryDecision {
    fn from(value: JobError) -> Self {
        match value {
            JobError::Invalid(err) => DeliveryDecision::Drop(err),
            JobError::Retryable(err) => DeliveryDecision::Retry(err),
        }
    }
}

fn ensure_exists(path: &Path, label: &str) -> Result<()> {
    if path.exists() {
        Ok(())
    } else {
        bail!("{label} not found at {}", path.display());
    }
}
