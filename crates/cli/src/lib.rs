use globset::{Glob, GlobSet, GlobSetBuilder};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::mpsc::channel;
use std::time::{Duration, Instant};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    Scan {
        inputs: Vec<String>,
        ignore: Vec<String>,
    },
    Build {
        inputs: Vec<String>,
        out: Option<String>,
        minify: bool,
        config: Option<String>,
        ignore: Vec<String>,
    },
    Watch {
        inputs: Vec<String>,
        out: Option<String>,
        minify: bool,
        config: Option<String>,
        ignore: Vec<String>,
        poll: bool,
        poll_interval_ms: u64,
    },
    Help,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CliError {
    pub message: String,
}

pub fn run(command: Command) -> Result<(), CliError> {
    match command {
        Command::Scan { inputs, ignore } => run_scan(inputs, ignore),
        Command::Build {
            inputs,
            out,
            minify,
            config,
            ignore,
        } => run_build(inputs, out, minify, config, ignore),
        Command::Watch {
            inputs,
            out,
            minify,
            config,
            ignore,
            poll,
            poll_interval_ms,
        } => run_watch(inputs, out, minify, config, ignore, poll, poll_interval_ms),
        Command::Help => {
            print_help();
            Ok(())
        }
    }
}

pub fn run_from_env() -> Result<(), CliError> {
    let command = parse_args(env::args().skip(1))?;
    run(command)
}

pub fn parse_args<I>(args: I) -> Result<Command, CliError>
where
    I: IntoIterator<Item = String>,
{
    let mut iter = args.into_iter();
    let Some(cmd) = iter.next() else {
        return Ok(Command::Help);
    };

    match cmd.as_str() {
        "scan" => parse_scan_args(iter.collect()),
        "build" => parse_build_args(iter.collect()),
        "watch" => parse_watch_args(iter.collect()),
        "-h" | "--help" | "help" => Ok(Command::Help),
        _ => Err(CliError {
            message: format!("unknown command: {}", cmd),
        }),
    }
}

fn parse_build_args(args: Vec<String>) -> Result<Command, CliError> {
    let mut inputs = Vec::new();
    let mut out = None;
    let mut minify = false;
    let mut config = None;
    let mut ignore = Vec::new();
    let mut idx = 0;

    while idx < args.len() {
        match args[idx].as_str() {
            "--out" | "-o" => {
                idx += 1;
                if idx >= args.len() {
                    return Err(CliError {
                        message: "build requires a value for --out".to_string(),
                    });
                }
                out = Some(args[idx].clone());
            }
            "--config" | "-c" => {
                idx += 1;
                if idx >= args.len() {
                    return Err(CliError {
                        message: "build requires a value for --config".to_string(),
                    });
                }
                config = Some(args[idx].clone());
            }
            "--ignore" | "-i" => {
                idx += 1;
                if idx >= args.len() {
                    return Err(CliError {
                        message: "build requires a value for --ignore".to_string(),
                    });
                }
                ignore.push(args[idx].clone());
            }
            "--minify" => {
                minify = true;
            }
            "--poll" => {
                return Err(CliError {
                    message: "--poll is only supported with watch".to_string(),
                });
            }
            value => {
                inputs.push(value.to_string());
            }
        }
        idx += 1;
    }

    if inputs.is_empty() {
        return Err(CliError {
            message: "build requires at least one path or glob pattern".to_string(),
        });
    }

    Ok(Command::Build {
        inputs,
        out,
        minify,
        config,
        ignore,
    })
}

fn parse_scan_args(args: Vec<String>) -> Result<Command, CliError> {
    let mut inputs = Vec::new();
    let mut ignore = Vec::new();
    let mut idx = 0;

    while idx < args.len() {
        match args[idx].as_str() {
            "--ignore" | "-i" => {
                idx += 1;
                if idx >= args.len() {
                    return Err(CliError {
                        message: "scan requires a value for --ignore".to_string(),
                    });
                }
                ignore.push(args[idx].clone());
            }
            value => {
                inputs.push(value.to_string());
            }
        }
        idx += 1;
    }

    if inputs.is_empty() {
        return Err(CliError {
            message: "scan requires at least one path or glob pattern".to_string(),
        });
    }

    Ok(Command::Scan { inputs, ignore })
}

fn parse_watch_args(args: Vec<String>) -> Result<Command, CliError> {
    let mut inputs = Vec::new();
    let mut out = None;
    let mut minify = false;
    let mut config = None;
    let mut ignore = Vec::new();
    let mut poll = false;
    let mut poll_interval_ms = 500;
    let mut idx = 0;

    while idx < args.len() {
        match args[idx].as_str() {
            "--out" | "-o" => {
                idx += 1;
                if idx >= args.len() {
                    return Err(CliError {
                        message: "watch requires a value for --out".to_string(),
                    });
                }
                out = Some(args[idx].clone());
            }
            "--config" | "-c" => {
                idx += 1;
                if idx >= args.len() {
                    return Err(CliError {
                        message: "watch requires a value for --config".to_string(),
                    });
                }
                config = Some(args[idx].clone());
            }
            "--ignore" | "-i" => {
                idx += 1;
                if idx >= args.len() {
                    return Err(CliError {
                        message: "watch requires a value for --ignore".to_string(),
                    });
                }
                ignore.push(args[idx].clone());
            }
            "--minify" => {
                minify = true;
            }
            "--poll" => {
                poll = true;
            }
            "--poll-interval" => {
                idx += 1;
                if idx >= args.len() {
                    return Err(CliError {
                        message: "watch requires a value for --poll-interval".to_string(),
                    });
                }
                poll = true;
                poll_interval_ms = parse_u64_arg(&args[idx], "--poll-interval")?;
            }
            value => {
                inputs.push(value.to_string());
            }
        }
        idx += 1;
    }

    if inputs.is_empty() {
        return Err(CliError {
            message: "watch requires at least one path or glob pattern".to_string(),
        });
    }

    Ok(Command::Watch {
        inputs,
        out,
        minify,
        config,
        ignore,
        poll,
        poll_interval_ms,
    })
}

fn run_scan(inputs: Vec<String>, ignore: Vec<String>) -> Result<(), CliError> {
    let mut result =
        ironframe_scanner::scan_globs_with_ignore(&inputs, &ignore).map_err(|err| CliError {
            message: err.message,
        })?;

    result.classes.sort();
    result.classes.dedup();

    for class in &result.classes {
        println!("{}", class);
    }

    eprintln!(
        "scanned {} files, found {} classes",
        result.files_scanned,
        result.classes.len()
    );

    Ok(())
}

fn run_build(
    inputs: Vec<String>,
    out: Option<String>,
    minify: bool,
    config_path: Option<String>,
    ignore: Vec<String>,
) -> Result<(), CliError> {
    let mut scan_result =
        ironframe_scanner::scan_globs_with_ignore(&inputs, &ignore).map_err(|err| CliError {
            message: err.message,
        })?;
    scan_result.classes.sort();
    scan_result.classes.dedup();

    let config = match config_path {
        Some(path) => ironframe_config::load(std::path::Path::new(&path)).map_err(|err| {
            CliError {
                message: err.message,
            }
        })?,
        None => ironframe_config::Config::default(),
    };
    let _theme = ironframe_config::resolve_theme(&config);

    let generator_config = ironframe_generator::GeneratorConfig {
        minify,
        colors: config.theme.colors.clone(),
    };
    let generation = ironframe_generator::generate(&scan_result.classes, &generator_config);
    let mut css = ironframe_generator::emit_css(&generation);
    let header = build_header(
        scan_result.files_scanned,
        scan_result.classes.len(),
        minify,
    );
    if minify {
        css = format!("{}{}", header, css);
    } else {
        css = format!("{}\n{}", header, css);
    }

    if let Some(out_path) = out {
        fs::write(&out_path, css).map_err(|err| CliError {
            message: format!("failed to write output {}: {}", out_path, err),
        })?;
    } else {
        print!("{}", css);
    }

    eprintln!(
        "scanned {} files, found {} classes",
        scan_result.files_scanned,
        scan_result.classes.len()
    );

    Ok(())
}

fn print_help() {
    println!("ironframe_cli");
    println!();
    println!("USAGE:");
    println!("  ironframe_cli scan [--ignore <glob>] <glob...>");
    println!("  ironframe_cli build [--minify] [--out <path>] [--config <path>] [--ignore <glob>] <glob...>");
    println!("  ironframe_cli watch [--minify] [--out <path>] [--config <path>] [--ignore <glob>] [--poll] [--poll-interval <ms>] <glob...>");
    println!();
    println!("EXAMPLES:");
    println!("  ironframe_cli scan \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe_cli scan -i \"**/generated/**\" \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe_cli build --out dist/tailwind.css \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe_cli build -c tailwind.toml \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe_cli watch -c tailwind.toml --out dist/tailwind.css \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe_cli build -i \"**/generated/**\" \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe_cli watch --poll --poll-interval 250 \"src/**/*.{{html,tsx}}\"");
}

fn build_header(files_scanned: usize, class_count: usize, minify: bool) -> String {
    if minify {
        return format!(
            "/* ironframe | files:{} | classes:{} */",
            files_scanned, class_count
        );
    }
    format!(
        "/*\n  ironframe\n  files: {}\n  classes: {}\n*/",
        files_scanned, class_count
    )
}

fn run_watch(
    inputs: Vec<String>,
    out: Option<String>,
    minify: bool,
    config: Option<String>,
    ignore: Vec<String>,
    poll: bool,
    poll_interval_ms: u64,
) -> Result<(), CliError> {
    run_build(inputs.clone(), out.clone(), minify, config.clone(), ignore.clone())?;
    let (tx, rx) = channel();
    let ignore_set = build_globset(&ignore).ok();
    let mut watcher: Box<dyn notify::Watcher> = if poll {
        Box::new(
            notify::PollWatcher::new(
                tx,
                notify::Config::default()
                    .with_poll_interval(Duration::from_millis(poll_interval_ms)),
            )
            .map_err(|err| CliError {
                message: format!("failed to start poll watcher: {}", err),
            })?,
        )
    } else {
        Box::new(notify::recommended_watcher(tx).map_err(|err| CliError {
            message: format!("failed to start watcher: {}", err),
        })?)
    };

    let roots = watch_roots(&inputs);
    if roots.is_empty() {
        watcher
            .watch(Path::new("."), notify::RecursiveMode::Recursive)
            .map_err(|err| CliError {
                message: format!("failed to watch current directory: {}", err),
            })?;
    } else {
        for root in roots {
            watcher
                .watch(&root, notify::RecursiveMode::Recursive)
                .map_err(|err| CliError {
                    message: format!("failed to watch {}: {}", root.display(), err),
                })?;
        }
    }

    if poll {
        eprintln!("watching for changes (polling, press Ctrl+C to stop)...");
    } else {
        eprintln!("watching for changes (press Ctrl+C to stop)...");
    }

    let mut last_event = Instant::now();
    loop {
        match rx.recv_timeout(Duration::from_millis(200)) {
            Ok(event_result) => {
                let event = match event_result {
                    Ok(event) => event,
                    Err(err) => {
                        eprintln!("watch error: {}", err);
                        continue;
                    }
                };
                if should_ignore_event(&event, ignore_set.as_ref()) {
                    continue;
                }
                if last_event.elapsed() < Duration::from_millis(200) {
                    continue;
                }
                last_event = Instant::now();
                eprintln!("change detected, rebuilding...");
                if let Err(err) =
                    run_build(inputs.clone(), out.clone(), minify, config.clone(), ignore.clone())
                {
                    eprintln!("build failed: {}", err.message);
                }
            }
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => continue,
            Err(_) => break,
        }
    }

    Ok(())
}

fn watch_roots(patterns: &[String]) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    let mut seen = std::collections::HashSet::new();

    for pattern in patterns {
        let root = glob_root(pattern);
        let normalized = if root.as_os_str().is_empty() {
            PathBuf::from(".")
        } else {
            root
        };
        if seen.insert(normalized.clone()) {
            roots.push(normalized);
        }
    }

    roots
}

fn glob_root(pattern: &str) -> PathBuf {
    let mut first_meta = None;
    for (idx, ch) in pattern.char_indices() {
        if matches!(ch, '*' | '?' | '[' | '{') {
            first_meta = Some(idx);
            break;
        }
    }

    if first_meta.is_none() {
        if pattern.ends_with('/') || pattern.ends_with('\\') {
            return PathBuf::from(pattern);
        }
        let path = Path::new(pattern);
        if path.extension().is_some() {
            return path.parent().unwrap_or(Path::new(".")).to_path_buf();
        }
        return path.to_path_buf();
    }

    let prefix = &pattern[..first_meta.unwrap()];
    let trimmed = prefix.trim_end_matches(&['/', '\\'][..]);
    if trimmed.is_empty() {
        return PathBuf::from(".");
    }
    let sep_idx = trimmed.rfind(|c| c == '/' || c == '\\');
    match sep_idx {
        Some(idx) => PathBuf::from(&trimmed[..=idx]),
        None => PathBuf::from("."),
    }
}

fn parse_u64_arg(value: &str, flag: &str) -> Result<u64, CliError> {
    value.parse::<u64>().map_err(|_| CliError {
        message: format!("{} requires a positive integer, got '{}'", flag, value),
    })
}

fn build_globset(patterns: &[String]) -> Result<GlobSet, CliError> {
    let mut builder = GlobSetBuilder::new();
    for pattern in patterns {
        let glob = Glob::new(pattern).map_err(|err| CliError {
            message: format!("invalid glob pattern '{}': {}", pattern, err),
        })?;
        builder.add(glob);
    }
    builder.build().map_err(|err| CliError {
        message: format!("failed to build ignore glob set: {}", err),
    })
}

fn should_ignore_event(event: &notify::Event, ignore_set: Option<&GlobSet>) -> bool {
    let Some(ignore_set) = ignore_set else {
        return false;
    };
    if event.paths.is_empty() {
        return false;
    }
    event.paths.iter().all(|path| ignore_set.is_match(path))
}
