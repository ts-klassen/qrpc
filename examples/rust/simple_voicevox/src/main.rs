use std::{
    error::Error as StdError,
    fmt, fs, io,
    path::{Path, PathBuf},
};

use voicevox_core::{
    blocking::{Onnxruntime, OpenJtalk, Synthesizer, VoiceModelFile},
    CharacterMeta, StyleId,
};

// Minimal sample: synthesize fixed text to output.wav using the first style in 0.vvm.
// All files are resolved under this example directory; no external paths are used.
fn main() -> Result<()> {
    let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let assets = PathBuf::from("/opt/qrpc/pkg/share/voicevox_core");
    let text = "Hello from VOICEVOX Core!";
    let ort_path = assets
        .join("onnxruntime/lib")
        .join(Onnxruntime::LIB_VERSIONED_FILENAME);
    let dict_path = assets.join("dict/open_jtalk_dic_utf_8-1.11");
    let model_path = assets.join("models/vvms/0.vvm");
    let output_path = base.join("output.wav");
    let dict_path_str = dict_path
        .to_str()
        .ok_or_else(|| InvalidUtf8Path(dict_path.clone()))?;

    ensure_exists(&ort_path, "onnxruntime library")?;
    ensure_exists(&dict_path, "open_jtalk dictionary")?;
    ensure_exists(&model_path, "voice model (0.vvm)")?;

    let model_file = VoiceModelFile::open(&model_path)?;
    let style_id = pick_first_style(model_file.metas())?;

    let ort = Onnxruntime::load_once().filename(&ort_path).perform()?;
    let text_analyzer = OpenJtalk::new(dict_path_str)?;
    let synthesizer = Synthesizer::builder(ort)
        .text_analyzer(text_analyzer)
        .build()?;

    synthesizer.load_voice_model(&model_file)?;

    let wav = synthesizer.tts(text, style_id).perform()?;

    fs::write(&output_path, &wav)?;

    println!("Saved {}", output_path.display());
    Ok(())
}

fn ensure_exists(path: &Path, label: &str) -> Result<()> {
    if !path.exists() {
        return Err(missing(path, label).into());
    }
    Ok(())
}

fn pick_first_style(metas: &[CharacterMeta]) -> Result<StyleId> {
    if let Some(id) = metas
        .iter()
        .flat_map(|c| c.styles.iter())
        .map(|s| s.id)
        .next()
    {
        Ok(id)
    } else {
        Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "no styles found in the provided VVM",
        )
        .into())
    }
}

type Result<T> = std::result::Result<T, Box<dyn StdError>>;

fn missing(path: &Path, label: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::NotFound,
        format!(
            "{label} not found at {}. Fetch voicevox_core assets under this directory.",
            path.display()
        ),
    )
}

#[derive(Debug)]
struct InvalidUtf8Path(PathBuf);

impl fmt::Display for InvalidUtf8Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "dictionary path must be valid UTF-8: {}",
            self.0.display()
        )
    }
}

impl StdError for InvalidUtf8Path {}
