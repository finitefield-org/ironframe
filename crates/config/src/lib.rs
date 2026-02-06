use serde::Deserialize;
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
pub struct Config {
    #[serde(default)]
    pub theme: Theme,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct Theme {
    #[serde(default = "default_theme_name")]
    pub name: String,
    #[serde(default)]
    pub colors: BTreeMap<String, BTreeMap<String, String>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConfigError {
    pub message: String,
}

pub fn load(_path: &Path) -> Result<Config, ConfigError> {
    let text = fs::read_to_string(_path).map_err(|err| ConfigError {
        message: format!("failed to read config {}: {}", _path.display(), err),
    })?;
    toml::from_str(&text).map_err(|err| ConfigError {
        message: format!("failed to parse config {}: {}", _path.display(), err),
    })
}

pub fn resolve_theme(config: &Config) -> Theme {
    config.theme.clone()
}

fn default_theme_name() -> String {
    "default".to_string()
}

impl Default for Theme {
    fn default() -> Self {
        Self {
            name: default_theme_name(),
            colors: BTreeMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{load, Config};
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn loads_toml_config() {
        let path = temp_path("ironframe_config");
        let _ = fs::write(&path, "theme = { name = \"custom\" }");
        let config = load(&path).expect("config should parse");
        assert_eq!(config.theme.name, "custom");
    }

    #[test]
    fn defaults_when_missing_theme() {
        let path = temp_path("ironframe_config_default");
        let _ = fs::write(&path, "");
        let config = load(&path).expect("config should parse");
        assert_eq!(config.theme.name, "default");
        assert!(config.theme.colors.is_empty());
        assert_eq!(config, Config::default());
    }

    #[test]
    fn loads_theme_colors() {
        let path = temp_path("ironframe_config_colors");
        let _ = fs::write(
            &path,
            r##"
[theme.colors.gray]
100 = "#f3f4f6"
500 = "#6b7280"

[theme.colors.blue]
500 = "#3b82f6"
"##,
        );
        let config = load(&path).expect("config should parse");
        assert_eq!(config.theme.colors["gray"]["100"], "#f3f4f6");
        assert_eq!(config.theme.colors["gray"]["500"], "#6b7280");
        assert_eq!(config.theme.colors["blue"]["500"], "#3b82f6");
    }

    fn temp_path(prefix: &str) -> std::path::PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        std::env::temp_dir().join(format!("{}_{}.toml", prefix, nanos))
    }
}
