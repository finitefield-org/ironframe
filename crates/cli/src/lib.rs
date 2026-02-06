use globset::{Glob, GlobSet, GlobSetBuilder};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::mpsc::channel;
use std::time::{Duration, Instant};

const PREFLIGHT_CSS: &str = include_str!("preflight.css");

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    Scan {
        inputs: Vec<String>,
        ignore: Vec<String>,
    },
    Build {
        inputs: Vec<String>,
        out: Option<String>,
        input_css: Option<String>,
        minify: bool,
        config: Option<String>,
        ignore: Vec<String>,
    },
    Watch {
        inputs: Vec<String>,
        out: Option<String>,
        input_css: Option<String>,
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
            input_css,
            minify,
            config,
            ignore,
        } => run_build(inputs, out, input_css, minify, config, ignore),
        Command::Watch {
            inputs,
            out,
            input_css,
            minify,
            config,
            ignore,
            poll,
            poll_interval_ms,
        } => run_watch(
            inputs,
            WatchOptions {
                out,
                input_css,
                minify,
                config,
                ignore,
                poll,
                poll_interval_ms,
            },
        ),
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
    let mut input_css = None;
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
            "--input-css" | "-s" => {
                idx += 1;
                if idx >= args.len() {
                    return Err(CliError {
                        message: "build requires a value for --input-css".to_string(),
                    });
                }
                input_css = Some(args[idx].clone());
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
        input_css,
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
    let mut input_css = None;
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
            "--input-css" | "-s" => {
                idx += 1;
                if idx >= args.len() {
                    return Err(CliError {
                        message: "watch requires a value for --input-css".to_string(),
                    });
                }
                input_css = Some(args[idx].clone());
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
        input_css,
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
    input_css_path: Option<String>,
    minify: bool,
    config_path: Option<String>,
    ignore: Vec<String>,
) -> Result<(), CliError> {
    let mut template_css = None;
    let mut stripped_template_css = None;
    let mut parsed_theme = None;
    let mut source_directives = SourceDirectives::default();
    let mut stylesheet_dir = None;
    if let Some(path) = input_css_path.as_ref() {
        let raw_template = fs::read_to_string(path).map_err(|err| CliError {
            message: format!("failed to read input css {}: {}", path, err),
        })?;
        let style_dir = resolve_stylesheet_dir(path);
        let template = inline_css_imports(&raw_template, &style_dir)?;
        source_directives = parse_source_directives(&template);
        stylesheet_dir = Some(style_dir);
        parsed_theme = Some(parse_theme(&template));
        let stripped = strip_tailwind_custom_directives(&template);
        let variant_overrides = parsed_theme
            .as_ref()
            .map(|theme| &theme.variant_overrides)
            .unwrap();
        stripped_template_css = Some(expand_variant_directives(&stripped, variant_overrides));
        template_css = Some(template);
    }

    let mut classes = std::collections::BTreeSet::<String>::new();
    let mut files_scanned = 0usize;

    if source_directives.auto_detection {
        let mut auto_options = ironframe_scanner::ScanGlobOptions::default();
        let mut auto_patterns = inputs.clone();
        if input_css_path.is_some() {
            auto_patterns = vec!["**/*".to_string()];
        }
        if let Some(base_path) = source_directives.base_path.as_deref() {
            if let Some(dir) = stylesheet_dir.as_deref() {
                auto_options.base_path = resolve_source_path(dir, base_path);
            }
        }
        let mut auto_ignore = ignore.clone();
        auto_ignore.extend(
            source_directives
                .exclude_paths
                .iter()
                .map(|path| {
                    if let Some(dir) = stylesheet_dir.as_deref() {
                        normalize_source_pattern(dir, path)
                    } else {
                        path.clone()
                    }
                })
                .collect::<Vec<_>>(),
        );
        let auto_scan =
            ironframe_scanner::scan_globs_with_options(&auto_patterns, &auto_ignore, &auto_options)
                .map_err(|err| CliError {
                    message: err.message,
                })?;
        files_scanned += auto_scan.files_scanned;
        for class in auto_scan.classes {
            classes.insert(class);
        }
    }

    if let Some(dir) = stylesheet_dir.as_deref() {
        let mut explicit_patterns = Vec::new();
        for path in &source_directives.include_paths {
            explicit_patterns.push(normalize_source_pattern(dir, path));
        }
        if !explicit_patterns.is_empty() {
            let explicit_options = ironframe_scanner::ScanGlobOptions {
                base_path: std::env::current_dir().map_err(|err| CliError {
                    message: format!("failed to resolve current directory: {}", err),
                })?,
                respect_gitignore: false,
                include_node_modules: true,
                ..ironframe_scanner::ScanGlobOptions::default()
            };

            let mut explicit_ignore = ignore.clone();
            explicit_ignore.extend(
                source_directives
                    .exclude_paths
                    .iter()
                    .map(|path| normalize_source_pattern(dir, path))
                    .collect::<Vec<_>>(),
            );
            let explicit_scan = ironframe_scanner::scan_globs_with_options(
                &explicit_patterns,
                &explicit_ignore,
                &explicit_options,
            )
            .map_err(|err| CliError {
                message: err.message,
            })?;
            files_scanned += explicit_scan.files_scanned;
            for class in explicit_scan.classes {
                classes.insert(class);
            }
        }
    }

    for class in &source_directives.inline_include {
        classes.insert(class.clone());
    }
    for class in &source_directives.inline_exclude {
        classes.remove(class);
    }
    let scan_result = ironframe_scanner::ScanResult {
        classes: classes.into_iter().collect(),
        files_scanned,
    };

    let config = match config_path {
        Some(path) => {
            ironframe_config::load(std::path::Path::new(&path)).map_err(|err| CliError {
                message: err.message,
            })?
        }
        None => ironframe_config::Config::default(),
    };
    let _theme = ironframe_config::resolve_theme(&config);

    let generator_config = ironframe_generator::GeneratorConfig {
        minify,
        colors: config.theme.colors.clone(),
    };
    if let Some(stripped) = stripped_template_css.take() {
        stripped_template_css = Some(expand_apply_directives(
            &stripped,
            &generator_config,
            parsed_theme.as_ref().map(|theme| &theme.variant_overrides),
        )?);
    }
    let generation = ironframe_generator::generate_with_overrides(
        &scan_result.classes,
        &generator_config,
        parsed_theme.as_ref().map(|theme| &theme.variant_overrides),
    );
    let mut utility_css = ironframe_generator::emit_css(&generation);
    let mut theme_css = String::new();
    if let Some(theme) = parsed_theme.as_ref() {
        if !theme.inline_variables.is_empty() {
            utility_css = apply_inline_theme_variables(&utility_css, &theme.inline_variables);
        }
        let usage_css = if let Some(template) = stripped_template_css.as_ref() {
            format!("{}\n{}", utility_css, template)
        } else {
            utility_css.clone()
        };
        theme_css = emit_parsed_theme_css(theme, &usage_css, minify);
    }
    let preflight_css = PREFLIGHT_CSS.to_string();
    let header = build_header(scan_result.files_scanned, scan_result.classes.len(), minify);
    let mut css = if minify {
        format!("{}{}{}", header, theme_css, utility_css)
    } else {
        let mut parts = vec![header.clone()];
        if !theme_css.is_empty() {
            parts.push(theme_css.clone());
        }
        if !utility_css.is_empty() {
            parts.push(utility_css.clone());
        }
        parts.join("\n")
    };

    if let Some(path) = input_css_path {
        let template = stripped_template_css
            .unwrap_or_else(|| strip_tailwind_custom_directives(&template_css.unwrap_or_default()));
        css = apply_framework_import_alias(
            &template,
            &header,
            &theme_css,
            &preflight_css,
            &utility_css,
            minify,
        )
        .ok_or_else(|| CliError {
            message: format!(
                "input css {} must include @import \"tailwindcss\" or @import \"ironframe\"",
                path
            ),
        })?;
    }
    css = expand_build_time_functions(&css);

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
    println!("ironframe");
    println!();
    println!("USAGE:");
    println!("  ironframe scan [--ignore <glob>] <glob...>");
    println!("  ironframe build [--minify] [--out <path>] [--input-css <path>] [--config <path>] [--ignore <glob>] <glob...>");
    println!("  ironframe watch [--minify] [--out <path>] [--input-css <path>] [--config <path>] [--ignore <glob>] [--poll] [--poll-interval <ms>] <glob...>");
    println!();
    println!("EXAMPLES:");
    println!("  ironframe scan \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe scan -i \"**/generated/**\" \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe build --out dist/tailwind.css \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe build --input-css src/app.css --out dist/app.css \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe build -c tailwind.toml \"src/**/*.{{html,tsx}}\"");
    println!(
        "  ironframe watch -c tailwind.toml --out dist/tailwind.css \"src/**/*.{{html,tsx}}\""
    );
    println!("  ironframe build -i \"**/generated/**\" \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe watch --poll --poll-interval 250 \"src/**/*.{{html,tsx}}\"");
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

#[derive(Debug, Clone)]
struct WatchOptions {
    out: Option<String>,
    input_css: Option<String>,
    minify: bool,
    config: Option<String>,
    ignore: Vec<String>,
    poll: bool,
    poll_interval_ms: u64,
}

fn run_watch(inputs: Vec<String>, options: WatchOptions) -> Result<(), CliError> {
    let WatchOptions {
        out,
        input_css,
        minify,
        config,
        ignore,
        poll,
        poll_interval_ms,
    } = options;

    run_build(
        inputs.clone(),
        out.clone(),
        input_css.clone(),
        minify,
        config.clone(),
        ignore.clone(),
    )?;
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

    let roots = watch_roots_for_build(&inputs, input_css.as_deref(), config.as_deref());
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
                if let Err(err) = run_build(
                    inputs.clone(),
                    out.clone(),
                    input_css.clone(),
                    minify,
                    config.clone(),
                    ignore.clone(),
                ) {
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

fn watch_roots_for_build(
    patterns: &[String],
    input_css: Option<&str>,
    config: Option<&str>,
) -> Vec<PathBuf> {
    let mut roots = watch_roots(patterns);
    let mut seen = roots
        .iter()
        .cloned()
        .collect::<std::collections::HashSet<_>>();

    for extra in [input_css, config].into_iter().flatten() {
        let root = glob_root(extra);
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
    let sep_idx = trimmed.rfind(['/', '\\']);
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

fn expand_build_time_functions(css: &str) -> String {
    let spacing_expanded = expand_spacing_function(css);
    expand_alpha_function(&spacing_expanded)
}

fn expand_spacing_function(css: &str) -> String {
    expand_named_function_calls(css, "--spacing(", &|args| {
        let value = args.trim();
        if value.is_empty() {
            return None;
        }
        Some(format!("calc(var(--spacing) * {})", value))
    })
}

fn expand_alpha_function(css: &str) -> String {
    expand_named_function_calls(css, "--alpha(", &|args| {
        let (color, alpha) = split_alpha_args(args)?;
        Some(format!(
            "color-mix(in oklab, {} {}, transparent)",
            color, alpha
        ))
    })
}

fn expand_named_function_calls(
    input: &str,
    marker: &str,
    transform: &dyn Fn(&str) -> Option<String>,
) -> String {
    let mut out = String::new();
    let mut cursor = 0usize;
    let marker_len = marker.len();
    let open_offset = marker_len.saturating_sub(1);

    while let Some(rel_start) = input[cursor..].find(marker) {
        let start = cursor + rel_start;
        let open_idx = start + open_offset;
        out.push_str(&input[cursor..start]);

        let Some(close_idx) = find_matching_paren(input, open_idx) else {
            out.push_str(&input[start..]);
            return out;
        };
        let args = &input[open_idx + 1..close_idx];
        let expanded_args = expand_build_time_functions(args);
        if let Some(replacement) = transform(&expanded_args) {
            out.push_str(&replacement);
        } else {
            out.push_str(&input[start..=close_idx]);
        }
        cursor = close_idx + 1;
    }
    out.push_str(&input[cursor..]);
    out
}

fn split_alpha_args(args: &str) -> Option<(String, String)> {
    let mut depth = 0usize;
    let mut quote: Option<char> = None;
    let mut escaped = false;
    let mut slash_idx = None;

    for (idx, ch) in args.char_indices() {
        if let Some(q) = quote {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == q {
                quote = None;
            }
            continue;
        }

        match ch {
            '\'' | '"' => quote = Some(ch),
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            '/' if depth == 0 => {
                slash_idx = Some(idx);
                break;
            }
            _ => {}
        }
    }

    let idx = slash_idx?;
    let color = args[..idx].trim();
    let alpha = args[idx + 1..].trim();
    if color.is_empty() || alpha.is_empty() {
        return None;
    }
    Some((color.to_string(), alpha.to_string()))
}

fn inline_css_imports(template_css: &str, base_dir: &Path) -> Result<String, CliError> {
    let mut visited = std::collections::BTreeSet::<PathBuf>::new();
    inline_css_imports_recursive(template_css, base_dir, &mut visited)
}

fn inline_css_imports_recursive(
    css: &str,
    base_dir: &Path,
    visited: &mut std::collections::BTreeSet<PathBuf>,
) -> Result<String, CliError> {
    let mut rendered_lines = Vec::new();
    for line in css.lines() {
        let trimmed = line.trim();
        if parse_framework_import_directives(trimmed).is_some() || !trimmed.starts_with("@import") {
            rendered_lines.push(line.to_string());
            continue;
        }

        let Some(path) = parse_plain_css_import_path(trimmed) else {
            rendered_lines.push(line.to_string());
            continue;
        };
        if path.starts_with("http://") || path.starts_with("https://") || path.starts_with("data:")
        {
            rendered_lines.push(line.to_string());
            continue;
        }
        let resolved = resolve_source_path(base_dir, &path);
        if visited.contains(&resolved) {
            continue;
        }
        let imported = fs::read_to_string(&resolved).map_err(|err| CliError {
            message: format!(
                "failed to read imported css {}: {}",
                resolved.display(),
                err
            ),
        })?;
        visited.insert(resolved.clone());
        let next_base = resolved.parent().unwrap_or(base_dir);
        let expanded = inline_css_imports_recursive(&imported, next_base, visited)?;
        rendered_lines.push(expanded);
    }

    let mut rendered = rendered_lines.join("\n");
    if css.ends_with('\n') && !rendered.ends_with('\n') {
        rendered.push('\n');
    }
    Ok(rendered)
}

fn parse_plain_css_import_path(trimmed_line: &str) -> Option<String> {
    let line = trimmed_line.trim_end_matches(';').trim();
    let body = line.strip_prefix("@import")?.trim();
    if let Some(rest) = body.strip_prefix('"') {
        let end = rest.find('"')?;
        if rest[end + 1..].trim().is_empty() {
            return Some(rest[..end].to_string());
        }
    }
    if let Some(rest) = body.strip_prefix('\'') {
        let end = rest.find('\'')?;
        if rest[end + 1..].trim().is_empty() {
            return Some(rest[..end].to_string());
        }
    }
    if let Some(rest) = body.strip_prefix("url(") {
        let close = rest.find(')')?;
        let inside = rest[..close].trim();
        let path = inside
            .strip_prefix('"')
            .and_then(|raw| raw.strip_suffix('"'))
            .or_else(|| {
                inside
                    .strip_prefix('\'')
                    .and_then(|raw| raw.strip_suffix('\''))
            })
            .unwrap_or(inside)
            .to_string();
        if rest[close + 1..].trim().is_empty() {
            return Some(path);
        }
    }
    None
}

fn expand_apply_directives(
    css: &str,
    config: &ironframe_generator::GeneratorConfig,
    overrides: Option<&ironframe_generator::VariantOverrides>,
) -> Result<String, CliError> {
    let mut out = String::new();
    let mut cursor = 0usize;
    while let Some(rel_start) = css[cursor..].find("@apply") {
        let apply_idx = cursor + rel_start;
        out.push_str(&css[cursor..apply_idx]);

        let mut pos = apply_idx + "@apply".len();
        while let Some(ch) = css[pos..].chars().next() {
            if !ch.is_whitespace() {
                break;
            }
            pos += ch.len_utf8();
        }
        let value_start = pos;
        while let Some(ch) = css[pos..].chars().next() {
            if ch == ';' {
                break;
            }
            pos += ch.len_utf8();
        }
        if pos >= css.len() {
            return Err(CliError {
                message: "@apply must end with ';'".to_string(),
            });
        }
        let class_list = css[value_start..pos].trim();
        if class_list.is_empty() {
            return Err(CliError {
                message: "@apply requires at least one utility class".to_string(),
            });
        }

        let mut declarations = Vec::new();
        for class in class_list.split_whitespace() {
            if class.contains(':') {
                return Err(CliError {
                    message: format!("@apply does not support variant classes: {}", class),
                });
            }
            let utility_css = ironframe_generator::generate_with_overrides(
                &[class.to_string()],
                config,
                overrides,
            );
            if utility_css.class_count == 0 {
                return Err(CliError {
                    message: format!("unknown utility in @apply: {}", class),
                });
            }
            let generated = ironframe_generator::emit_css(&utility_css);
            let Some(open_idx) = generated.find('{') else {
                return Err(CliError {
                    message: format!("failed to parse generated utility for @apply: {}", class),
                });
            };
            let Some(close_idx) = find_matching_brace(&generated, open_idx) else {
                return Err(CliError {
                    message: format!("failed to parse generated utility for @apply: {}", class),
                });
            };
            let body = generated[open_idx + 1..close_idx]
                .trim()
                .trim_end_matches(';');
            if body.is_empty() {
                return Err(CliError {
                    message: format!("utility in @apply has no declarations: {}", class),
                });
            }
            declarations.push(body.to_string());
        }

        let merged = declarations.join("; ");
        out.push_str(&merged);
        out.push(';');
        cursor = pos + 1;
    }

    out.push_str(&css[cursor..]);
    Ok(out)
}

fn apply_framework_import_alias(
    template_css: &str,
    header_css: &str,
    theme_css: &str,
    preflight_css: &str,
    utility_css: &str,
    minify: bool,
) -> Option<String> {
    let mut replaced = false;
    let mut emitted_header = false;
    let mut emitted_theme = false;
    let mut emitted_preflight = false;
    let mut emitted_utilities = false;
    let mut rendered_lines = Vec::new();

    for line in template_css.lines() {
        let Some(directives) = parse_framework_import_directives(line) else {
            rendered_lines.push(line.to_string());
            continue;
        };

        replaced = true;
        let mut blocks = Vec::<String>::new();
        if !emitted_header {
            blocks.push(header_css.to_string());
            emitted_header = true;
        }

        match directives.target {
            FrameworkImportTarget::Framework => {
                if !emitted_theme {
                    blocks.push(apply_import_transforms(
                        theme_css,
                        directives.prefix.as_deref(),
                        false,
                    ));
                    emitted_theme = true;
                }
                if !emitted_preflight {
                    blocks.push(preflight_css.to_string());
                    emitted_preflight = true;
                }
                if !emitted_utilities {
                    blocks.push(apply_import_transforms(
                        utility_css,
                        directives.prefix.as_deref(),
                        directives.important,
                    ));
                    emitted_utilities = true;
                }
            }
            FrameworkImportTarget::Theme => {
                if !emitted_theme {
                    blocks.push(apply_import_transforms(
                        theme_css,
                        directives.prefix.as_deref(),
                        false,
                    ));
                    emitted_theme = true;
                }
            }
            FrameworkImportTarget::Preflight => {
                if !emitted_preflight {
                    blocks.push(preflight_css.to_string());
                    emitted_preflight = true;
                }
            }
            FrameworkImportTarget::Utilities => {
                if !emitted_utilities {
                    blocks.push(apply_import_transforms(
                        utility_css,
                        directives.prefix.as_deref(),
                        directives.important,
                    ));
                    emitted_utilities = true;
                }
            }
        }

        let rendered_block = combine_css_blocks(&blocks, minify);
        if !rendered_block.is_empty() {
            rendered_lines.push(rendered_block);
        }
    }

    if !replaced {
        return None;
    }

    let mut rendered = rendered_lines.join("\n");
    if template_css.ends_with('\n') {
        rendered.push('\n');
    }
    Some(rendered)
}

fn apply_import_transforms(css: &str, prefix: Option<&str>, important: bool) -> String {
    let mut rendered = css.to_string();
    if let Some(prefix) = prefix {
        rendered = apply_prefix_to_css(&rendered, prefix);
    }
    if important {
        rendered = apply_important_to_css(&rendered);
    }
    rendered
}

fn combine_css_blocks(blocks: &[String], minify: bool) -> String {
    let filtered = blocks
        .iter()
        .filter(|block| !block.is_empty())
        .collect::<Vec<_>>();
    if filtered.is_empty() {
        return String::new();
    }
    if minify {
        filtered.into_iter().map(|block| block.as_str()).collect()
    } else {
        filtered
            .into_iter()
            .map(|block| block.as_str())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ParsedTheme {
    variant_overrides: ironframe_generator::VariantOverrides,
    root_variables: Vec<ThemeVariable>,
    keyframes: Vec<ThemeKeyframes>,
    inline_variables: Vec<(String, String)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ThemeVariable {
    name: String,
    value: String,
    always_emit: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ThemeKeyframes {
    name: String,
    css: String,
}

fn parse_theme(template_css: &str) -> ParsedTheme {
    let mut breakpoint_updates = Vec::<(String, String)>::new();
    let mut container_updates = Vec::<(String, String)>::new();
    let mut root_variables = Vec::<ThemeVariable>::new();
    let mut inline_variables = Vec::<(String, String)>::new();
    let mut keyframes = Vec::<ThemeKeyframes>::new();
    let mut global_theme_reset = false;
    let mut disabled_namespaces = std::collections::BTreeSet::<String>::new();
    let mut disabled_color_families = std::collections::BTreeSet::<String>::new();
    let mut declared_theme_vars = std::collections::BTreeSet::<String>::new();

    for block in extract_theme_blocks(template_css) {
        keyframes.extend(extract_keyframes(block.body));
        for (name, value) in extract_theme_variable_declarations(block.body) {
            if let Some(raw) = name.strip_prefix("--breakpoint-") {
                breakpoint_updates.push((raw.to_string(), value.to_string()));
            } else if let Some(raw) = name.strip_prefix("--container-") {
                container_updates.push((raw.to_string(), value.to_string()));
            }

            if name == "--*" && value == "initial" {
                global_theme_reset = true;
                continue;
            }
            if let Some(namespace) = name
                .strip_prefix("--color-")
                .and_then(|raw| raw.strip_suffix("-*"))
            {
                if value == "initial" {
                    if !namespace.is_empty() {
                        disabled_color_families.insert(namespace.to_string());
                    }
                    continue;
                }
            }
            if let Some(namespace) = name
                .strip_prefix("--")
                .and_then(|raw| raw.strip_suffix("-*"))
            {
                if value == "initial" {
                    disabled_namespaces.insert(namespace.to_string());
                    continue;
                }
            }

            if value != "initial" {
                declared_theme_vars.insert(name.to_string());
            }

            if block.inline {
                inline_variables.push((name.to_string(), value.to_string()));
            } else {
                root_variables.push(ThemeVariable {
                    name: name.to_string(),
                    value: value.to_string(),
                    always_emit: block.static_mode,
                });
            }
        }
    }

    let custom_variant_selectors = parse_custom_variant_selectors(template_css);
    let custom_utilities = parse_custom_utilities(template_css);
    let mut theme_variable_values = std::collections::BTreeMap::<String, String>::new();
    for variable in &root_variables {
        theme_variable_values.insert(variable.name.clone(), variable.value.clone());
    }
    for (name, value) in &inline_variables {
        theme_variable_values.insert(name.clone(), value.clone());
    }
    let dark_variant_selector = custom_variant_selectors
        .iter()
        .find(|(name, _)| name == "dark")
        .map(|(_, selector)| selector.clone());

    ParsedTheme {
        variant_overrides: ironframe_generator::VariantOverrides {
            responsive_breakpoints: resolve_named_sizes(
                &[
                    ("sm", "40rem"),
                    ("md", "48rem"),
                    ("lg", "64rem"),
                    ("xl", "80rem"),
                    ("2xl", "96rem"),
                ],
                &breakpoint_updates,
            ),
            container_breakpoints: resolve_named_sizes(
                &[
                    ("3xs", "16rem"),
                    ("2xs", "18rem"),
                    ("xs", "20rem"),
                    ("sm", "24rem"),
                    ("md", "28rem"),
                    ("lg", "32rem"),
                    ("xl", "36rem"),
                    ("2xl", "42rem"),
                    ("3xl", "48rem"),
                    ("4xl", "56rem"),
                    ("5xl", "64rem"),
                    ("6xl", "72rem"),
                    ("7xl", "80rem"),
                ],
                &container_updates,
            ),
            dark_variant_selector,
            custom_variant_selectors,
            custom_utilities,
            theme_variable_values: theme_variable_values.into_iter().collect(),
            global_theme_reset,
            disabled_namespaces: disabled_namespaces.into_iter().collect(),
            disabled_color_families: disabled_color_families.into_iter().collect(),
            declared_theme_vars: declared_theme_vars.into_iter().collect(),
        },
        root_variables,
        keyframes,
        inline_variables,
    }
}

fn parse_custom_variant_selectors(css: &str) -> Vec<(String, String)> {
    let mut variants = std::collections::BTreeMap::<String, String>::new();
    for def in extract_custom_variant_definitions(css) {
        if !def.template.is_empty() {
            variants.insert(def.name.to_string(), def.template.to_string());
        }
    }

    variants.into_iter().collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CustomVariantDefinition<'a> {
    start: usize,
    end: usize,
    name: &'a str,
    template: &'a str,
}

fn extract_custom_variant_definitions(css: &str) -> Vec<CustomVariantDefinition<'_>> {
    let mut definitions = Vec::new();
    let mut cursor = 0usize;

    while let Some(rel_start) = css[cursor..].find("@custom-variant") {
        let directive_idx = cursor + rel_start;
        if !is_top_level_position(css, directive_idx) {
            cursor = directive_idx + "@custom-variant".len();
            continue;
        }

        let mut pos = directive_idx + "@custom-variant".len();
        while let Some(ch) = css[pos..].chars().next() {
            if !ch.is_whitespace() {
                break;
            }
            pos += ch.len_utf8();
        }
        let name_start = pos;
        while let Some(ch) = css[pos..].chars().next() {
            if ch.is_whitespace() || ch == '(' || ch == '{' || ch == ';' {
                break;
            }
            pos += ch.len_utf8();
        }
        let name = css[name_start..pos].trim();
        if name.is_empty() {
            cursor = directive_idx + "@custom-variant".len();
            continue;
        }

        while let Some(ch) = css[pos..].chars().next() {
            if !ch.is_whitespace() {
                break;
            }
            pos += ch.len_utf8();
        }

        if css[pos..].starts_with('(') {
            let open_idx = pos;
            let Some(close_idx) = find_matching_paren(css, open_idx) else {
                cursor = directive_idx + "@custom-variant".len();
                continue;
            };
            let template = css[open_idx + 1..close_idx].trim();
            let mut end = close_idx + 1;
            while let Some(ch) = css[end..].chars().next() {
                if ch.is_whitespace() {
                    end += ch.len_utf8();
                    continue;
                }
                if ch == ';' {
                    end += ch.len_utf8();
                }
                break;
            }
            definitions.push(CustomVariantDefinition {
                start: directive_idx,
                end,
                name,
                template,
            });
            cursor = end;
            continue;
        }

        if css[pos..].starts_with('{') {
            let open_idx = pos;
            let Some(close_idx) = find_matching_brace(css, open_idx) else {
                cursor = directive_idx + "@custom-variant".len();
                continue;
            };
            definitions.push(CustomVariantDefinition {
                start: directive_idx,
                end: close_idx + 1,
                name,
                template: css[open_idx + 1..close_idx].trim(),
            });
            cursor = close_idx + 1;
            continue;
        }

        cursor = directive_idx + "@custom-variant".len();
    }

    definitions
}

fn find_matching_paren(css: &str, open_idx: usize) -> Option<usize> {
    if !css[open_idx..].starts_with('(') {
        return None;
    }

    let mut depth = 0usize;
    let mut in_string: Option<char> = None;
    let mut escaped = false;
    let chars = css[open_idx..].char_indices();
    for (rel_idx, ch) in chars {
        let idx = open_idx + rel_idx;
        if let Some(quote) = in_string {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == quote {
                in_string = None;
            }
            continue;
        }

        if ch == '\'' || ch == '"' {
            in_string = Some(ch);
            continue;
        }

        match ch {
            '(' => depth += 1,
            ')' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return Some(idx);
                }
            }
            _ => {}
        }
    }
    None
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct UtilityBlock<'a> {
    start: usize,
    end: usize,
    name: &'a str,
    body: &'a str,
}

fn extract_utility_blocks(css: &str) -> Vec<UtilityBlock<'_>> {
    let mut blocks = Vec::new();
    let mut cursor = 0usize;

    while let Some(rel_start) = css[cursor..].find("@utility") {
        let utility_idx = cursor + rel_start;
        if !is_top_level_position(css, utility_idx) {
            cursor = utility_idx + "@utility".len();
            continue;
        }
        let Some(open_rel) = css[utility_idx..].find('{') else {
            break;
        };
        let open_idx = utility_idx + open_rel;
        let Some(close_idx) = find_matching_brace(css, open_idx) else {
            break;
        };

        let header = css[utility_idx + "@utility".len()..open_idx].trim();
        let name = header.split_whitespace().next().unwrap_or("").trim();
        if !name.is_empty() {
            blocks.push(UtilityBlock {
                start: utility_idx,
                end: close_idx + 1,
                name,
                body: &css[open_idx + 1..close_idx],
            });
        }

        cursor = close_idx + 1;
    }

    blocks
}

fn parse_custom_utilities(css: &str) -> Vec<(String, String)> {
    let mut utilities = std::collections::BTreeMap::<String, String>::new();
    for block in extract_utility_blocks(css) {
        let body = block.body.trim();
        if !body.is_empty() {
            utilities.insert(block.name.to_string(), body.to_string());
        }
    }
    utilities.into_iter().collect()
}

fn resolve_named_sizes(
    defaults: &[(&str, &str)],
    updates: &[(String, String)],
) -> Vec<(String, String)> {
    let mut resolved = std::collections::BTreeMap::<String, String>::new();
    for (name, value) in defaults {
        resolved.insert((*name).to_string(), (*value).to_string());
    }

    for (name, value) in updates {
        let value = value.trim();
        if name == "*" && value == "initial" {
            resolved.clear();
            continue;
        }
        if value == "initial" {
            resolved.remove(name);
        } else {
            resolved.insert(name.clone(), value.to_string());
        }
    }

    resolved.into_iter().collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ThemeBlock<'a> {
    start: usize,
    end: usize,
    inline: bool,
    static_mode: bool,
    body: &'a str,
}

fn extract_theme_blocks(css: &str) -> Vec<ThemeBlock<'_>> {
    let mut blocks = Vec::new();
    let mut cursor = 0usize;

    while let Some(rel_start) = css[cursor..].find("@theme") {
        let theme_idx = cursor + rel_start;
        if !is_top_level_position(css, theme_idx) {
            cursor = theme_idx + "@theme".len();
            continue;
        }
        let Some(open_rel) = css[theme_idx..].find('{') else {
            break;
        };
        let open_idx = theme_idx + open_rel;
        let Some(close_idx) = find_matching_brace(css, open_idx) else {
            break;
        };
        let header = css[theme_idx..open_idx].trim();
        blocks.push(ThemeBlock {
            start: theme_idx,
            end: close_idx + 1,
            inline: header.split_whitespace().any(|token| token == "inline"),
            static_mode: header.split_whitespace().any(|token| token == "static"),
            body: &css[open_idx + 1..close_idx],
        });
        cursor = close_idx + 1;
    }

    blocks
}

fn is_top_level_position(css: &str, target_idx: usize) -> bool {
    let mut depth = 0usize;
    let mut in_comment = false;
    let mut in_string: Option<char> = None;
    let mut escaped = false;
    let mut chars = css.char_indices().peekable();

    while let Some((idx, ch)) = chars.next() {
        if idx >= target_idx {
            return depth == 0 && !in_comment && in_string.is_none();
        }

        if in_comment {
            if ch == '*' {
                if let Some((_, '/')) = chars.peek().copied() {
                    let _ = chars.next();
                    in_comment = false;
                }
            }
            continue;
        }

        if let Some(quote) = in_string {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == quote {
                in_string = None;
            }
            continue;
        }

        if ch == '/' {
            if let Some((_, '*')) = chars.peek().copied() {
                let _ = chars.next();
                in_comment = true;
                continue;
            }
        }

        if ch == '"' || ch == '\'' {
            in_string = Some(ch);
            continue;
        }

        match ch {
            '{' => depth += 1,
            '}' => depth = depth.saturating_sub(1),
            _ => {}
        }
    }

    depth == 0 && !in_comment && in_string.is_none()
}

fn extract_theme_variable_declarations(body: &str) -> Vec<(&str, &str)> {
    let mut declarations = Vec::new();
    let mut depth = 0usize;
    let mut segment_start = 0usize;

    for (idx, ch) in body.char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => depth = depth.saturating_sub(1),
            ';' if depth == 0 => {
                let segment = body[segment_start..idx].trim();
                segment_start = idx + 1;
                let Some((name, value)) = segment.split_once(':') else {
                    continue;
                };
                let name = name.trim();
                let value = value.trim();
                if !name.starts_with("--") || name.is_empty() || value.is_empty() {
                    continue;
                }
                declarations.push((name, value));
            }
            _ => {}
        }
    }

    declarations
}

fn extract_keyframes(body: &str) -> Vec<ThemeKeyframes> {
    let mut keyframes = Vec::new();
    let mut cursor = 0usize;

    while let Some(rel_start) = body[cursor..].find("@keyframes") {
        let start = cursor + rel_start;
        let Some(open_rel) = body[start..].find('{') else {
            break;
        };
        let open_idx = start + open_rel;
        let Some(close_idx) = find_matching_brace(body, open_idx) else {
            break;
        };
        let keyframes_css = body[start..=close_idx].trim().to_string();
        keyframes.push(ThemeKeyframes {
            name: extract_keyframe_name(&body[start..open_idx]),
            css: keyframes_css,
        });
        cursor = close_idx + 1;
    }

    keyframes
}

fn extract_keyframe_name(header: &str) -> String {
    header
        .trim()
        .trim_start_matches("@keyframes")
        .trim()
        .to_string()
}

fn emit_parsed_theme_css(theme: &ParsedTheme, generated_css: &str, minify: bool) -> String {
    let used_variables = extract_css_variables(generated_css);
    let emitted_root_vars = resolve_emitted_theme_variables(&theme.root_variables, &used_variables);
    let emitted_animate_names = emitted_root_vars
        .iter()
        .filter_map(|var| {
            if var.name.starts_with("--animate-") {
                Some(parse_animation_names(&var.value))
            } else {
                None
            }
        })
        .flatten()
        .collect::<std::collections::BTreeSet<_>>();
    let mut parts = Vec::<String>::new();

    if !emitted_root_vars.is_empty() {
        if minify {
            let mut body = String::new();
            for variable in &emitted_root_vars {
                body.push_str(&variable.name);
                body.push(':');
                body.push_str(&variable.value);
                body.push(';');
            }
            parts.push(format!(":root{{{}}}", body));
        } else {
            let mut block = String::from(":root {\n");
            for variable in &emitted_root_vars {
                block.push_str("  ");
                block.push_str(&variable.name);
                block.push_str(": ");
                block.push_str(&variable.value);
                block.push_str(";\n");
            }
            block.push('}');
            parts.push(block);
        }
    }

    parts.extend(
        theme
            .keyframes
            .iter()
            .filter(|keyframes| emitted_animate_names.contains(&keyframes.name))
            .map(|keyframes| keyframes.css.clone()),
    );
    if minify {
        parts.join("")
    } else {
        parts.join("\n")
    }
}

fn extract_css_variables(css: &str) -> std::collections::BTreeSet<String> {
    extract_css_variables_from_value(css).into_iter().collect()
}

fn resolve_emitted_theme_variables(
    root_variables: &[ThemeVariable],
    used_variables: &std::collections::BTreeSet<String>,
) -> Vec<ThemeVariable> {
    let mut included_indices = std::collections::BTreeSet::<usize>::new();
    let index_by_name = root_variables
        .iter()
        .enumerate()
        .map(|(idx, variable)| (variable.name.as_str(), idx))
        .collect::<std::collections::BTreeMap<_, _>>();

    for (idx, variable) in root_variables.iter().enumerate() {
        if variable.always_emit || used_variables.contains(&variable.name) {
            included_indices.insert(idx);
        }
    }

    loop {
        let mut changed = false;
        let current_indices = included_indices.iter().copied().collect::<Vec<_>>();
        for idx in current_indices {
            let variable = &root_variables[idx];
            for referenced in extract_css_variables_from_value(&variable.value) {
                if let Some(referenced_idx) = index_by_name.get(referenced.as_str()).copied() {
                    if included_indices.insert(referenced_idx) {
                        changed = true;
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    root_variables
        .iter()
        .enumerate()
        .filter(|(idx, _)| included_indices.contains(idx))
        .map(|(_, variable)| variable.clone())
        .collect()
}

fn extract_css_variables_from_value(value: &str) -> Vec<String> {
    let mut vars = Vec::new();
    let mut cursor = 0usize;
    while let Some(rel_start) = value[cursor..].find("var(--") {
        let start = cursor + rel_start + "var(".len();
        let Some(end_rel) = value[start..].find(')') else {
            break;
        };
        let end = start + end_rel;
        let raw = &value[start..end];
        let variable = raw.split(',').next().unwrap_or(raw).trim();
        if variable.starts_with("--") {
            vars.push(variable.to_string());
        }
        cursor = end + 1;
    }
    vars
}

fn parse_animation_names(value: &str) -> std::collections::BTreeSet<String> {
    let mut names = std::collections::BTreeSet::new();
    for entry in split_top_level_commas(value) {
        let trimmed = entry.trim();
        if trimmed.is_empty() {
            continue;
        }
        let Some(first) = trimmed.split_whitespace().next() else {
            continue;
        };
        if first != "none" && !first.contains('(') {
            names.insert(first.to_string());
        }
    }
    names
}

fn split_top_level_commas(value: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    for (idx, ch) in value.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                parts.push(value[start..idx].trim());
                start = idx + 1;
            }
            _ => {}
        }
    }
    if start < value.len() {
        parts.push(value[start..].trim());
    }
    parts
}

fn apply_inline_theme_variables(css: &str, inline_variables: &[(String, String)]) -> String {
    let mut out = css.to_string();
    for (name, value) in inline_variables {
        let target = format!("var({})", name);
        out = out.replace(&target, value);
    }
    out
}

fn strip_tailwind_custom_directives(template_css: &str) -> String {
    let theme_blocks = extract_theme_blocks(template_css);
    let utility_blocks = extract_utility_blocks(template_css);
    let custom_variant_defs = extract_custom_variant_definitions(template_css);
    let mut ranges = theme_blocks
        .iter()
        .map(|block| (block.start, block.end))
        .collect::<Vec<_>>();
    ranges.extend(utility_blocks.iter().map(|block| (block.start, block.end)));
    ranges.extend(custom_variant_defs.iter().map(|def| (def.start, def.end)));
    ranges.sort_by_key(|(start, _)| *start);

    let mut out = String::new();
    let mut cursor = 0usize;
    for (start, end) in ranges {
        if cursor < start {
            out.push_str(&template_css[cursor..start]);
        }
        cursor = end;
    }
    if cursor < template_css.len() {
        out.push_str(&template_css[cursor..]);
    }

    let filtered_lines = out
        .lines()
        .filter(|line| !line.trim_start().starts_with("@source"))
        .collect::<Vec<_>>();
    let mut rendered = filtered_lines.join("\n");
    if template_css.ends_with('\n') && !rendered.ends_with('\n') {
        rendered.push('\n');
    }
    rendered
}

fn expand_variant_directives(
    css: &str,
    overrides: &ironframe_generator::VariantOverrides,
) -> String {
    expand_variant_directives_in_block(css, overrides)
}

fn expand_variant_directives_in_block(
    content: &str,
    overrides: &ironframe_generator::VariantOverrides,
) -> String {
    let mut out = String::new();
    let mut cursor = 0usize;

    while let Some(rel_start) = content[cursor..].find("@variant") {
        let variant_idx = cursor + rel_start;
        out.push_str(&content[cursor..variant_idx]);

        let mut pos = variant_idx + "@variant".len();
        while let Some(ch) = content[pos..].chars().next() {
            if !ch.is_whitespace() {
                break;
            }
            pos += ch.len_utf8();
        }
        let name_start = pos;
        while let Some(ch) = content[pos..].chars().next() {
            if ch.is_whitespace() || ch == '{' {
                break;
            }
            pos += ch.len_utf8();
        }
        let variant_name = content[name_start..pos].trim();
        if variant_name.is_empty() {
            out.push_str("@variant");
            cursor = variant_idx + "@variant".len();
            continue;
        }
        while let Some(ch) = content[pos..].chars().next() {
            if !ch.is_whitespace() {
                break;
            }
            pos += ch.len_utf8();
        }
        if !content[pos..].starts_with('{') {
            out.push_str("@variant");
            cursor = variant_idx + "@variant".len();
            continue;
        }
        let open_idx = pos;
        let Some(close_idx) = find_matching_brace(content, open_idx) else {
            out.push_str(&content[variant_idx..]);
            return out;
        };
        let inner = &content[open_idx + 1..close_idx];
        let expanded_inner = expand_variant_directives_in_block(inner, overrides);
        let wrapped = wrap_variant_block_for_css(variant_name, &expanded_inner, overrides)
            .unwrap_or_else(|| format!("@variant {} {{ {} }}", variant_name, expanded_inner));
        out.push_str(&wrapped);
        cursor = close_idx + 1;
    }

    out.push_str(&content[cursor..]);
    out
}

fn wrap_variant_block_for_css(
    variant: &str,
    inner: &str,
    overrides: &ironframe_generator::VariantOverrides,
) -> Option<String> {
    if variant == "dark" {
        if let Some(template) = overrides.dark_variant_selector.as_ref() {
            return Some(format!(
                "{} {{ {} }}",
                normalize_variant_template(template),
                inner
            ));
        }
        return Some(format!(
            "@media (prefers-color-scheme: dark) {{ {} }}",
            inner
        ));
    }

    if let Some((_, template)) = overrides
        .custom_variant_selectors
        .iter()
        .find(|(name, _)| name == variant)
    {
        let template = normalize_variant_template(template);
        if template.contains("@slot") {
            return Some(template.replace("@slot", inner));
        }
        return Some(format!("{} {{ {} }}", template, inner));
    }

    if let Some((_, width)) = overrides
        .responsive_breakpoints
        .iter()
        .find(|(name, _)| name == variant)
    {
        return Some(format!("@media (width >= {}) {{ {} }}", width, inner));
    }

    match variant {
        "hover" => Some(format!(
            "&:hover {{ @media (hover: hover) {{ {} }} }}",
            inner
        )),
        "focus" => Some(format!("&:focus {{ {} }}", inner)),
        "focus-within" => Some(format!("&:focus-within {{ {} }}", inner)),
        "focus-visible" => Some(format!("&:focus-visible {{ {} }}", inner)),
        "active" => Some(format!("&:active {{ {} }}", inner)),
        "visited" => Some(format!("&:visited {{ {} }}", inner)),
        "disabled" => Some(format!("&:disabled {{ {} }}", inner)),
        _ => None,
    }
}

fn normalize_variant_template(template: &str) -> String {
    template.trim().replace('_', " ")
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ImportDirectives {
    target: FrameworkImportTarget,
    important: bool,
    prefix: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FrameworkImportTarget {
    Framework,
    Theme,
    Preflight,
    Utilities,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SourceDirectives {
    auto_detection: bool,
    base_path: Option<String>,
    include_paths: Vec<String>,
    exclude_paths: Vec<String>,
    inline_include: Vec<String>,
    inline_exclude: Vec<String>,
}

impl Default for SourceDirectives {
    fn default() -> Self {
        Self {
            auto_detection: true,
            base_path: None,
            include_paths: Vec::new(),
            exclude_paths: Vec::new(),
            inline_include: Vec::new(),
            inline_exclude: Vec::new(),
        }
    }
}

fn parse_framework_import_directives(line: &str) -> Option<ImportDirectives> {
    let trimmed = line.trim();
    if !trimmed.starts_with("@import") {
        return None;
    }

    let target = resolve_framework_import_target(trimmed)?;

    let important = trimmed.contains(" important") || trimmed.ends_with("important;");
    let prefix = extract_prefix_directive(trimmed);

    Some(ImportDirectives {
        target,
        important,
        prefix,
    })
}

fn resolve_framework_import_target(import_line: &str) -> Option<FrameworkImportTarget> {
    if contains_framework_import_target(import_line, "tailwindcss/preflight.css")
        || contains_framework_import_target(import_line, "ironframe/preflight.css")
    {
        return Some(FrameworkImportTarget::Preflight);
    }
    if contains_framework_import_target(import_line, "tailwindcss/theme.css")
        || contains_framework_import_target(import_line, "ironframe/theme.css")
    {
        return Some(FrameworkImportTarget::Theme);
    }
    if contains_framework_import_target(import_line, "tailwindcss/utilities.css")
        || contains_framework_import_target(import_line, "ironframe/utilities.css")
    {
        return Some(FrameworkImportTarget::Utilities);
    }
    if contains_framework_import_target(import_line, "tailwindcss")
        || contains_framework_import_target(import_line, "ironframe")
    {
        return Some(FrameworkImportTarget::Framework);
    }
    None
}

fn contains_framework_import_target(import_line: &str, target: &str) -> bool {
    let quoted_double = format!("\"{}\"", target);
    let quoted_single = format!("'{}'", target);
    let url_double = format!("url(\"{}\")", target);
    let url_single = format!("url('{}')", target);
    import_line.contains(&quoted_double)
        || import_line.contains(&quoted_single)
        || import_line.contains(&url_double)
        || import_line.contains(&url_single)
}

fn extract_prefix_directive(import_line: &str) -> Option<String> {
    let marker = "prefix(";
    let start = import_line.find(marker)?;
    let rest = &import_line[start + marker.len()..];
    let end = rest.find(')')?;
    let prefix = rest[..end].trim();
    if prefix.is_empty() {
        return None;
    }
    Some(prefix.to_string())
}

fn parse_source_directives(css: &str) -> SourceDirectives {
    let mut directives = SourceDirectives::default();

    for line in css.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("@import") {
            if let Some(source_value) = extract_import_source_directive(trimmed) {
                if source_value == "none" {
                    directives.auto_detection = false;
                } else {
                    directives.base_path = Some(source_value);
                }
            }
            continue;
        }

        if !trimmed.starts_with("@source") {
            continue;
        }
        let mut raw = trimmed.trim_start_matches("@source").trim();
        raw = raw.trim_end_matches(';').trim();
        let mut is_not = false;
        if let Some(rest) = raw.strip_prefix("not ") {
            is_not = true;
            raw = rest.trim();
        }

        if raw.starts_with("inline(") && raw.ends_with(')') {
            if let Some(value) = parse_parenthesized_quoted_value(raw, "inline") {
                let expanded = expand_braces(&value);
                if is_not {
                    directives.inline_exclude.extend(expanded);
                } else {
                    directives.inline_include.extend(expanded);
                }
            }
            continue;
        }

        if let Some(value) = parse_quoted_literal(raw) {
            if is_not {
                directives.exclude_paths.push(value);
            } else {
                directives.include_paths.push(value);
            }
        }
    }

    directives.inline_include.sort();
    directives.inline_include.dedup();
    directives.inline_exclude.sort();
    directives.inline_exclude.dedup();
    directives.include_paths.sort();
    directives.include_paths.dedup();
    directives.exclude_paths.sort();
    directives.exclude_paths.dedup();
    directives
}

fn extract_import_source_directive(import_line: &str) -> Option<String> {
    parse_framework_import_directives(import_line)?;
    parse_parenthesized_quoted_value(import_line, "source")
}

fn parse_parenthesized_quoted_value(input: &str, fn_name: &str) -> Option<String> {
    let value = parse_parenthesized_unquoted_or_quoted_value(input, fn_name)?;
    if value == "none" {
        return Some(value);
    }
    parse_quoted_literal(value.as_str()).or(Some(value))
}

fn parse_parenthesized_unquoted_or_quoted_value(input: &str, fn_name: &str) -> Option<String> {
    let marker = format!("{}(", fn_name);
    let start = input.find(&marker)? + marker.len();
    let mut depth = 1usize;
    let mut in_string: Option<char> = None;
    let mut escaped = false;

    for (offset, ch) in input[start..].char_indices() {
        if let Some(quote) = in_string {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == quote {
                in_string = None;
            }
            continue;
        }
        if ch == '"' || ch == '\'' {
            in_string = Some(ch);
            continue;
        }
        if ch == '(' {
            depth += 1;
            continue;
        }
        if ch == ')' {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                let raw = input[start..start + offset].trim().to_string();
                return Some(raw);
            }
        }
    }

    None
}

fn parse_quoted_literal(input: &str) -> Option<String> {
    let trimmed = input.trim();
    if trimmed.len() < 2 {
        return None;
    }
    let quote = trimmed.chars().next()?;
    if (quote != '"' && quote != '\'') || !trimmed.ends_with(quote) {
        return None;
    }
    Some(trimmed[1..trimmed.len() - 1].to_string())
}

fn resolve_stylesheet_dir(path: &str) -> PathBuf {
    let raw = Path::new(path);
    if raw.is_absolute() {
        return raw.parent().unwrap_or(Path::new("/")).to_path_buf();
    }
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    cwd.join(raw).parent().unwrap_or(&cwd).to_path_buf()
}

fn resolve_source_path(base_dir: &Path, source: &str) -> PathBuf {
    let raw = Path::new(source);
    if raw.is_absolute() {
        return raw.to_path_buf();
    }
    base_dir.join(raw)
}

fn normalize_source_pattern(base_dir: &Path, source: &str) -> String {
    if contains_glob_meta(source) {
        return resolve_source_path(base_dir, source)
            .to_string_lossy()
            .to_string();
    }

    let resolved = resolve_source_path(base_dir, source);
    if source.ends_with('/') || source.ends_with('\\') || resolved.is_dir() {
        return resolved.join("**/*").to_string_lossy().to_string();
    }

    resolved.to_string_lossy().to_string()
}

fn contains_glob_meta(input: &str) -> bool {
    input.chars().any(|ch| matches!(ch, '*' | '?' | '[' | '{'))
}

fn expand_braces(input: &str) -> Vec<String> {
    let Some((open, close)) = first_brace_pair(input) else {
        return vec![input.to_string()];
    };

    let prefix = &input[..open];
    let body = &input[open + 1..close];
    let suffix = &input[close + 1..];
    let mut expanded = Vec::new();

    for variant in split_top_level_commas_owned(body) {
        let options = expand_brace_variant(&variant);
        for option in options {
            for rest in expand_braces(suffix) {
                expanded.push(format!("{}{}{}", prefix, option, rest));
            }
        }
    }

    expanded
}

fn first_brace_pair(input: &str) -> Option<(usize, usize)> {
    let mut depth = 0usize;
    let mut open_idx = None;
    for (idx, ch) in input.char_indices() {
        if ch == '{' {
            if depth == 0 {
                open_idx = Some(idx);
            }
            depth += 1;
            continue;
        }
        if ch == '}' {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                return open_idx.map(|open| (open, idx));
            }
        }
    }
    None
}

fn split_top_level_commas_owned(value: &str) -> Vec<String> {
    let mut parts = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    for (idx, ch) in value.char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                parts.push(value[start..idx].trim().to_string());
                start = idx + 1;
            }
            _ => {}
        }
    }
    parts.push(value[start..].trim().to_string());
    parts
}

fn expand_brace_variant(variant: &str) -> Vec<String> {
    if let Some(values) = expand_numeric_range(variant) {
        return values;
    }
    expand_braces(variant)
}

fn expand_numeric_range(variant: &str) -> Option<Vec<String>> {
    let parts = variant.split("..").collect::<Vec<_>>();
    if parts.len() != 2 && parts.len() != 3 {
        return None;
    }
    let start = parts[0].trim().parse::<i32>().ok()?;
    let end = parts[1].trim().parse::<i32>().ok()?;
    let mut step = if parts.len() == 3 {
        parts[2].trim().parse::<i32>().ok()?
    } else if start <= end {
        1
    } else {
        -1
    };
    if step == 0 {
        return None;
    }
    if start < end && step < 0 {
        step = -step;
    }
    if start > end && step > 0 {
        step = -step;
    }

    let mut values = Vec::new();
    let mut current = start;
    loop {
        values.push(current.to_string());
        if current == end {
            break;
        }
        current += step;
        if (step > 0 && current > end) || (step < 0 && current < end) {
            break;
        }
    }

    Some(values)
}

fn apply_important_to_css(css: &str) -> String {
    transform_css_rules(css, &|header, body| {
        let important_body = add_important_to_declarations_block(body);
        format!("{}{{{}}}", header, important_body)
    })
}

fn append_important_to_decl(decl: &str) -> String {
    if !decl.contains(':') || decl.contains("!important") {
        return decl.to_string();
    }
    format!("{} !important", decl)
}

fn apply_prefix_to_css(css: &str, prefix: &str) -> String {
    let prefixed_selectors = transform_css_rules(css, &|header, body| {
        let prefixed_header = prefix_header_selectors(header, prefix);
        format!("{}{{{}}}", prefixed_header, body)
    });
    prefix_framework_variables(&prefixed_selectors, prefix)
}

fn transform_css_rules(css: &str, leaf_transform: &dyn Fn(&str, &str) -> String) -> String {
    let mut out = String::new();
    let mut cursor = 0usize;

    while cursor < css.len() {
        let Some(open_rel) = css[cursor..].find('{') else {
            out.push_str(&css[cursor..]);
            break;
        };
        let open = cursor + open_rel;
        let Some(close) = find_matching_brace(css, open) else {
            out.push_str(&css[cursor..]);
            break;
        };

        let header = &css[cursor..open];
        let body = &css[open + 1..close];

        if header.trim_start().starts_with("@media ") {
            let nested = transform_css_rules(body, leaf_transform);
            out.push_str(header);
            out.push('{');
            out.push_str(&nested);
            out.push('}');
        } else {
            out.push_str(&leaf_transform(header, body));
        }

        cursor = close + 1;
    }

    out
}

fn find_matching_brace(css: &str, open_idx: usize) -> Option<usize> {
    let mut depth = 0usize;
    let mut in_comment = false;
    let mut chars = css[open_idx..].char_indices().peekable();

    while let Some((rel_idx, ch)) = chars.next() {
        let idx = open_idx + rel_idx;
        if in_comment {
            if ch == '*' {
                if let Some((_, '/')) = chars.peek().copied() {
                    let _ = chars.next();
                    in_comment = false;
                }
            }
            continue;
        }

        if ch == '/' {
            if let Some((_, '*')) = chars.peek().copied() {
                let _ = chars.next();
                in_comment = true;
                continue;
            }
        }

        match ch {
            '{' => depth += 1,
            '}' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return Some(idx);
                }
            }
            _ => {}
        }
    }

    None
}

fn add_important_to_declarations_block(body: &str) -> String {
    let minified = !body.contains(": ");
    let mut parts = Vec::new();
    for decl in body.split(';') {
        let decl = decl.trim();
        if decl.is_empty() {
            continue;
        }
        parts.push(append_important_to_decl(decl));
    }
    if minified {
        parts.join(";")
    } else {
        let mut text = parts.join("; ");
        if !text.is_empty() {
            text.push(';');
        }
        text
    }
}

fn prefix_header_selectors(header: &str, prefix: &str) -> String {
    let mut out = String::with_capacity(header.len() + 16);
    let mut chars = header.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch != '.' {
            out.push(ch);
            continue;
        }

        let mut token = String::new();
        while let Some(next) = chars.peek().copied() {
            if next.is_ascii_alphanumeric() || next == '-' || next == '_' || next == '\\' {
                token.push(next);
                let _ = chars.next();
            } else {
                break;
            }
        }

        if token.is_empty() || token == "dark" {
            out.push('.');
            out.push_str(&token);
            continue;
        }

        let prefixed = format!("{}\\:", prefix);
        if token.starts_with(&prefixed) {
            out.push('.');
            out.push_str(&token);
            continue;
        }

        out.push('.');
        out.push_str(prefix);
        out.push_str("\\:");
        out.push_str(&token);
    }

    out
}

fn prefix_framework_variables(css: &str, prefix: &str) -> String {
    let mut out = String::with_capacity(css.len() + 64);
    let chars: Vec<char> = css.chars().collect();
    let mut i = 0usize;

    while i < chars.len() {
        if chars[i] == '-' && i + 1 < chars.len() && chars[i + 1] == '-' {
            let start = i;
            i += 2;
            while i < chars.len() {
                let ch = chars[i];
                if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                    i += 1;
                } else {
                    break;
                }
            }

            let var_name: String = chars[start..i].iter().collect();
            if should_prefix_variable(&var_name, prefix) {
                out.push_str(&prefixed_variable_name(&var_name, prefix));
            } else {
                out.push_str(&var_name);
            }
            continue;
        }

        out.push(chars[i]);
        i += 1;
    }

    out
}

fn should_prefix_variable(var_name: &str, prefix: &str) -> bool {
    if !var_name.starts_with("--") {
        return false;
    }
    if var_name.starts_with(&format!("--{}-", prefix)) {
        return false;
    }
    if var_name.starts_with("--my-") {
        return false;
    }

    let framework_prefixes = [
        "--tw-",
        "--color-",
        "--spacing",
        "--radius-",
        "--blur-",
        "--drop-shadow-",
        "--perspective-",
        "--ease-",
        "--animate-",
        "--font-",
        "--text-",
        "--shadow-",
        "--inset-shadow-",
        "--inset-ring-",
        "--breakpoint-",
        "--container-",
    ];

    framework_prefixes
        .iter()
        .any(|candidate| var_name.starts_with(candidate))
}

fn prefixed_variable_name(var_name: &str, prefix: &str) -> String {
    let raw = &var_name[2..];
    format!("--{}-{}", prefix, raw)
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

#[cfg(test)]
mod tests {
    use super::{
        apply_framework_import_alias, apply_important_to_css, apply_prefix_to_css,
        emit_parsed_theme_css, expand_apply_directives, expand_braces, expand_build_time_functions,
        expand_variant_directives, inline_css_imports, normalize_source_pattern, parse_args,
        parse_framework_import_directives, parse_source_directives, parse_theme,
        strip_tailwind_custom_directives, watch_roots_for_build, Command, PREFLIGHT_CSS,
    };
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn render_framework_import(template_css: &str, generated_css: &str) -> Option<String> {
        apply_framework_import_alias(
            template_css,
            "/* header */",
            "",
            PREFLIGHT_CSS,
            generated_css,
            false,
        )
    }

    #[test]
    fn parse_build_supports_input_css_flag() {
        let command = parse_args(vec![
            "build".to_string(),
            "--input-css".to_string(),
            "src/app.css".to_string(),
            "--out".to_string(),
            "dist/app.css".to_string(),
            "src/**/*.html".to_string(),
        ])
        .expect("build args should parse");

        assert_eq!(
            command,
            Command::Build {
                inputs: vec!["src/**/*.html".to_string()],
                out: Some("dist/app.css".to_string()),
                input_css: Some("src/app.css".to_string()),
                minify: false,
                config: None,
                ignore: vec![],
            }
        );
    }

    #[test]
    fn parse_watch_supports_input_css_flag() {
        let command = parse_args(vec![
            "watch".to_string(),
            "-s".to_string(),
            "src/app.css".to_string(),
            "src/**/*.html".to_string(),
        ])
        .expect("watch args should parse");

        assert_eq!(
            command,
            Command::Watch {
                inputs: vec!["src/**/*.html".to_string()],
                out: None,
                input_css: Some("src/app.css".to_string()),
                minify: false,
                config: None,
                ignore: vec![],
                poll: false,
                poll_interval_ms: 500,
            }
        );
    }

    #[test]
    fn watch_roots_include_input_css_and_config_paths() {
        let roots = watch_roots_for_build(
            &["packages/app/src/**/*.html".to_string()],
            Some("styles/app.css"),
            Some("config/tailwind.toml"),
        );
        assert!(roots.contains(&PathBuf::from("packages/app")));
        assert!(roots.contains(&PathBuf::from("styles/")));
        assert!(roots.contains(&PathBuf::from("config/")));
    }

    #[test]
    fn accepts_tailwindcss_and_ironframe_import_aliases() {
        assert!(parse_framework_import_directives(r#"@import "tailwindcss";"#).is_some());
        assert!(parse_framework_import_directives(r#"@import "tailwindcss" important;"#).is_some());
        assert!(parse_framework_import_directives(r#"@import "ironframe";"#).is_some());
        assert!(parse_framework_import_directives(r#"@import 'ironframe';"#).is_some());
        assert!(parse_framework_import_directives(r#"@import "tailwindcss/theme.css";"#).is_some());
        assert!(
            parse_framework_import_directives(r#"@import "tailwindcss/preflight.css";"#).is_some()
        );
        assert!(
            parse_framework_import_directives(r#"@import "tailwindcss/utilities.css";"#).is_some()
        );
    }

    #[test]
    fn can_disable_preflight_by_omitting_preflight_import() {
        let template = r#"@import "tailwindcss/theme.css";
@import "tailwindcss/utilities.css";
"#;
        let rendered = apply_framework_import_alias(
            template,
            "/* header */",
            ":root { --color-red-500: #ef4444; }",
            PREFLIGHT_CSS,
            ".p-2 { padding: 0.5rem; }",
            false,
        )
        .expect("framework imports should be replaced");

        assert!(rendered.contains("--color-red-500"));
        assert!(rendered.contains(".p-2 { padding: 0.5rem; }"));
        assert!(!rendered.contains("::file-selector-button {\n  box-sizing: border-box;"));
    }

    #[test]
    fn supports_split_preflight_imports() {
        let template = r#"@import "tailwindcss/theme.css";
@import "tailwindcss/preflight.css";
@import "tailwindcss/utilities.css";
"#;
        let rendered = apply_framework_import_alias(
            template,
            "/* header */",
            ":root { --color-red-500: #ef4444; }",
            PREFLIGHT_CSS,
            ".p-2 { padding: 0.5rem; }",
            false,
        )
        .expect("framework imports should be replaced");

        assert!(rendered.contains(":root { --color-red-500: #ef4444; }"));
        assert!(rendered.contains("::file-selector-button {\n  box-sizing: border-box;"));
        assert!(rendered.contains(".p-2 { padding: 0.5rem; }"));
    }

    #[test]
    fn replaces_framework_import_line_with_generated_css() {
        let generated = "/* generated */\n.p-2 { padding: 0.5rem; }";

        let rendered =
            render_framework_import("@import \"tailwindcss\";\nbody { margin: 0; }\n", generated)
                .expect("tailwindcss import should be replaced");
        assert!(rendered.contains("/* generated */"));
        assert!(rendered.contains("body { margin: 0; }"));
        assert!(rendered.contains("*,\n::after,\n::before,\n::backdrop,\n::file-selector-button"));
        assert!(!rendered.contains("@import \"tailwindcss\";"));

        let rendered = render_framework_import(
            "@import \"ironframe\";\nmain { display: block; }\n",
            generated,
        )
        .expect("ironframe import should be replaced");
        assert!(rendered.contains("/* generated */"));
        assert!(rendered.contains("main { display: block; }"));
        assert!(!rendered.contains("@import \"ironframe\";"));
    }

    #[test]
    fn supports_import_important_directive() {
        let generated = ".p-2 { padding: 0.5rem; color: red; }";
        let rendered = render_framework_import(
            "@import \"tailwindcss\" important;\nbody { margin: 0; }\n",
            generated,
        )
        .expect("important import should be replaced");
        assert!(rendered.contains("padding: 0.5rem !important;"));
        assert!(rendered.contains("color: red !important;"));
    }

    #[test]
    fn supports_import_prefix_directive() {
        let generated = ".p-2 { padding: 0.5rem; }\n.dark\\:bg-gray-900 { background-color: var(--color-gray-900); }";
        let rendered = render_framework_import(
            "@import \"ironframe\" prefix(tw);\nbody { margin: 0; }\n",
            generated,
        )
        .expect("prefix import should be replaced");
        assert!(rendered.contains(".tw\\:p-2 {"));
        assert!(rendered.contains(".tw\\:dark\\:bg-gray-900 {"));
        assert!(rendered.contains("var(--tw-color-gray-900)"));
    }

    #[test]
    fn can_apply_prefix_and_important_helpers() {
        let prefixed = apply_prefix_to_css(".p-2 { padding: 0.5rem; }", "tw");
        assert!(prefixed.contains(".tw\\:p-2"));
        let important = apply_important_to_css(".p-2 { padding: 0.5rem; }");
        assert!(important.contains("padding: 0.5rem !important;"));
    }

    #[test]
    fn applies_prefix_and_important_inside_media_queries() {
        let css = "@media (min-width: 640px) { .sm\\:p-2 { padding: 0.5rem; } }";
        let prefixed = apply_prefix_to_css(css, "tw");
        assert!(prefixed.contains(".tw\\:sm\\:p-2"));
        let important = apply_important_to_css(css);
        assert!(important.contains("padding: 0.5rem !important;"));
    }

    #[test]
    fn supports_import_prefix_and_important_together() {
        let generated = ".p-2 { padding: 0.5rem; }\n@media (min-width: 640px) { .sm\\:p-2 { padding: 0.5rem; } }";
        let rendered =
            render_framework_import("@import \"tailwindcss\" prefix(tw) important;\n", generated)
                .expect("combined directives should be applied");
        assert!(rendered.contains(".tw\\:p-2"));
        assert!(rendered.contains("padding: 0.5rem !important;"));
        assert!(rendered.contains(".tw\\:sm\\:p-2"));
    }

    #[test]
    fn supports_url_import_form() {
        let generated = ".p-2 { padding: 0.5rem; }";
        let rendered = render_framework_import("@import url('ironframe') prefix(tw);\n", generated)
            .expect("url import should be recognized");
        assert!(rendered.contains(".tw\\:p-2"));
    }

    #[test]
    fn inlines_regular_css_imports() {
        let base = temp_dir("cli_css_import");
        let nested = base.join("nested.css");
        let imported = base.join("imported.css");
        let _ = fs::create_dir_all(&base);
        fs::write(&nested, ".nested { color: red; }\n").expect("should write nested.css");
        fs::write(
            &imported,
            "@import \"./nested.css\";\n.imported { color: blue; }\n",
        )
        .expect("should write imported.css");
        let css =
            "@import \"tailwindcss\";\n@import \"./imported.css\";\n.main { color: black; }\n";
        let inlined = inline_css_imports(css, &base).expect("should inline imports");

        assert!(inlined.contains("@import \"tailwindcss\";"));
        assert!(inlined.contains(".nested { color: red; }"));
        assert!(inlined.contains(".imported { color: blue; }"));
        assert!(inlined.contains(".main { color: black; }"));
        let _ = fs::remove_dir_all(base);
    }

    #[test]
    fn prefixes_framework_variable_definitions_and_references() {
        let generated = ":root { --color-red-500: #ef4444; --my-brand: #123; }\n.text-red-500 { color: var(--color-red-500); }\n.bg-brand { background: var(--my-brand); }";
        let rendered = render_framework_import("@import \"tailwindcss\" prefix(tw);\n", generated)
            .expect("prefix import should be replaced");
        assert!(rendered.contains("--tw-color-red-500: #ef4444"));
        assert!(rendered.contains("color: var(--tw-color-red-500)"));
        assert!(rendered.contains("--my-brand: #123"));
        assert!(rendered.contains("var(--my-brand)"));
    }

    #[test]
    fn returns_none_when_framework_import_is_missing() {
        assert!(render_framework_import("body { color: black; }", ".p-2{}").is_none());
    }

    #[test]
    fn parses_theme_breakpoint_and_container_overrides() {
        let css = r#"
@import "tailwindcss";
@theme {
  --breakpoint-xs: 30rem;
  --breakpoint-2xl: initial;
  --container-8xl: 96rem;
  --container-*: initial;
  --container-sm: 24rem;
  --color-lime-*: initial;
  --color-brand-500: #10b981;
}
@custom-variant dark (&:where(.dark, .dark *));
@custom-variant theme-midnight (&:where([data-theme="midnight"] *));
@custom-variant any-hover {
  @media (any-hover: hover) {
    &:hover {
      @slot;
    }
  }
}
@utility content-auto {
  content-visibility: auto;
}
@utility scrollbar-hidden {
  &::-webkit-scrollbar {
    display: none;
  }
}
"#;
        let overrides = parse_theme(css).variant_overrides;
        assert!(overrides
            .responsive_breakpoints
            .contains(&("xs".to_string(), "30rem".to_string())));
        assert!(!overrides
            .responsive_breakpoints
            .iter()
            .any(|(name, _)| name == "2xl"));
        assert_eq!(
            overrides.container_breakpoints,
            vec![("sm".to_string(), "24rem".to_string())]
        );
        assert_eq!(
            overrides.dark_variant_selector,
            Some("&:where(.dark, .dark *)".to_string())
        );
        assert!(overrides.custom_variant_selectors.contains(&(
            "theme-midnight".to_string(),
            "&:where([data-theme=\"midnight\"] *)".to_string()
        )));
        assert!(overrides
            .custom_variant_selectors
            .iter()
            .any(|(name, template)| name == "any-hover" && template.contains("@slot")));
        assert!(overrides.custom_utilities.contains(&(
            "content-auto".to_string(),
            "content-visibility: auto;".to_string()
        )));
        assert!(overrides
            .theme_variable_values
            .contains(&("--color-brand-500".to_string(), "#10b981".to_string())));
        assert!(overrides.custom_utilities.iter().any(
            |(name, body)| name == "scrollbar-hidden" && body.contains("&::-webkit-scrollbar")
        ));
        assert!(!overrides.global_theme_reset);
        assert!(overrides
            .disabled_namespaces
            .contains(&"container".to_string()));
        assert!(overrides
            .disabled_color_families
            .contains(&"lime".to_string()));
        assert!(overrides
            .declared_theme_vars
            .contains(&"--color-brand-500".to_string()));
        assert!(overrides
            .declared_theme_vars
            .contains(&"--breakpoint-xs".to_string()));
    }

    #[test]
    fn parses_theme_variables_keyframes_and_inline_mode() {
        let css = r#"
@import "tailwindcss";
@theme {
  --color-brand: #123456;
  --animate-wiggle: wiggle 1s ease-in-out infinite;
  @keyframes wiggle {
    0% { transform: rotate(-3deg); }
    100% { transform: rotate(3deg); }
  }
}
@theme inline {
  --font-sans: var(--font-inter);
}
"#;
        let parsed = parse_theme(css);
        assert!(parsed
            .root_variables
            .iter()
            .any(|entry| entry.name == "--color-brand" && entry.value == "#123456"));
        assert!(parsed
            .inline_variables
            .contains(&("--font-sans".to_string(), "var(--font-inter)".to_string())));
        assert!(parsed
            .keyframes
            .iter()
            .any(|block| block.css.contains("@keyframes wiggle")));
        let emitted = emit_parsed_theme_css(
            &parsed,
            ".animate-wiggle { animation: var(--animate-wiggle); }",
            false,
        );
        assert!(emitted.contains(":root {"));
        assert!(!emitted.contains("--color-brand: #123456;"));
        assert!(emitted.contains("--animate-wiggle: wiggle 1s ease-in-out infinite;"));
        assert!(emitted.contains("@keyframes wiggle"));
    }

    #[test]
    fn theme_static_option_keeps_unused_variables() {
        let css = r#"
@theme static {
  --color-brand: #123456;
}
"#;
        let parsed = parse_theme(css);
        let emitted = emit_parsed_theme_css(&parsed, ".p-4 { padding: 1rem; }", false);
        assert!(emitted.contains("--color-brand: #123456;"));
    }

    #[test]
    fn theme_variables_default_to_usage_based_output() {
        let css = r#"
@theme {
  --color-brand: #123456;
  --color-unused: #abcdef;
  --color-dependent: var(--color-brand);
}
"#;
        let parsed = parse_theme(css);
        let emitted = emit_parsed_theme_css(
            &parsed,
            ".bg-brand { background-color: var(--color-dependent); }",
            false,
        );
        assert!(emitted.contains("--color-dependent: var(--color-brand);"));
        assert!(emitted.contains("--color-brand: #123456;"));
        assert!(!emitted.contains("--color-unused: #abcdef;"));
    }

    #[test]
    fn theme_variables_used_by_template_css_are_emitted() {
        let css = r#"
@theme {
  --color-brand: #123456;
}
"#;
        let parsed = parse_theme(css);
        let emitted = emit_parsed_theme_css(
            &parsed,
            ".p-4 { padding: 1rem; }\n.typography { color: var(--color-brand); }",
            false,
        );
        assert!(emitted.contains("--color-brand: #123456;"));
    }

    #[test]
    fn ignores_nested_theme_blocks() {
        let css = r#"
@media (width >= 40rem) {
  @theme {
    --color-nested: #111111;
  }
}
@theme {
  --color-top: #222222;
}
"#;
        let parsed = parse_theme(css);
        assert!(parsed
            .root_variables
            .iter()
            .any(|entry| entry.name == "--color-top"));
        assert!(!parsed
            .root_variables
            .iter()
            .any(|entry| entry.name == "--color-nested"));
    }

    #[test]
    fn ignores_theme_markers_inside_comments() {
        let css = r#"
/* @theme { --color-comment: #111111; } */
@theme { --color-top: #222222; }
"#;
        let parsed = parse_theme(css);
        assert!(parsed
            .root_variables
            .iter()
            .any(|entry| entry.name == "--color-top"));
        assert!(!parsed
            .root_variables
            .iter()
            .any(|entry| entry.name == "--color-comment"));
    }

    #[test]
    fn strips_custom_theme_and_variant_directives_from_template() {
        let css = r#"
@import "tailwindcss";
@theme { --color-brand: #123456; }
@custom-variant dark (&:where(.dark, .dark *));
@custom-variant any-hover {
  @media (any-hover: hover) {
    &:hover {
      @slot;
    }
  }
}
@utility content-auto { content-visibility: auto; }
body { margin: 0; }
"#;
        let stripped = strip_tailwind_custom_directives(css);
        assert!(stripped.contains("@import \"tailwindcss\";"));
        assert!(stripped.contains("body { margin: 0; }"));
        assert!(!stripped.contains("@theme"));
        assert!(!stripped.contains("@custom-variant"));
        assert!(!stripped.contains("@utility"));
        assert!(stripped.contains("body { margin: 0; }"));
    }

    #[test]
    fn strips_source_directives_from_template() {
        let css = r#"
@import "tailwindcss";
@source "../node_modules/acme-ui";
@source not "../legacy";
body { margin: 0; }
"#;
        let stripped = strip_tailwind_custom_directives(css);
        assert!(stripped.contains("@import \"tailwindcss\";"));
        assert!(stripped.contains("body { margin: 0; }"));
        assert!(!stripped.contains("@source"));
    }

    #[test]
    fn parses_source_directives_from_import_and_source_rules() {
        let css = r#"
@import "tailwindcss" source("../src");
@source "../node_modules/@acmecorp/ui-lib";
@source not "../src/components/legacy";
@source inline("{hover:,focus:,}underline");
@source not inline("{hover:,focus:,}bg-red-{50,{100..900..100},950}");
"#;
        let directives = parse_source_directives(css);
        assert!(directives.auto_detection);
        assert_eq!(directives.base_path, Some("../src".to_string()));
        assert!(directives
            .include_paths
            .contains(&"../node_modules/@acmecorp/ui-lib".to_string()));
        assert!(directives
            .exclude_paths
            .contains(&"../src/components/legacy".to_string()));
        assert!(directives
            .inline_include
            .contains(&"hover:underline".to_string()));
        assert!(directives
            .inline_include
            .contains(&"focus:underline".to_string()));
        assert!(directives.inline_exclude.contains(&"bg-red-50".to_string()));
        assert!(directives
            .inline_exclude
            .contains(&"hover:bg-red-500".to_string()));
        assert!(directives
            .inline_exclude
            .contains(&"focus:bg-red-950".to_string()));
    }

    #[test]
    fn parses_source_none_on_import() {
        let css = r#"@import "tailwindcss" source(none);"#;
        let directives = parse_source_directives(css);
        assert!(!directives.auto_detection);
        assert!(directives.base_path.is_none());
    }

    #[test]
    fn expands_braces_with_nested_lists_and_ranges() {
        let expanded = expand_braces("{hover:,}bg-red-{50,{100..300..100},950}");
        assert!(expanded.contains(&"bg-red-50".to_string()));
        assert!(expanded.contains(&"hover:bg-red-50".to_string()));
        assert!(expanded.contains(&"bg-red-100".to_string()));
        assert!(expanded.contains(&"hover:bg-red-300".to_string()));
        assert!(expanded.contains(&"bg-red-950".to_string()));
    }

    #[test]
    fn normalizes_source_directory_to_recursive_glob() {
        let base = temp_dir("cli_source_pattern");
        let dir = base.join("node_modules/acme-ui");
        let _ = fs::create_dir_all(&dir);
        let pattern = normalize_source_pattern(&base, "node_modules/acme-ui");
        assert!(pattern.ends_with("node_modules/acme-ui/**/*"));
        let _ = fs::remove_dir_all(base);
    }

    fn temp_dir(prefix: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        std::env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .join(format!("{}_{}", prefix, nanos))
    }

    #[test]
    fn expands_variant_directive_in_custom_css() {
        let css = r#"
.my-element {
  background: white;
  @variant dark {
    background: black;
  }
}
"#;
        let overrides = ironframe_generator::VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let expanded = expand_variant_directives(css, &overrides);
        assert!(expanded.contains("@media (prefers-color-scheme: dark)"));
        assert!(expanded.contains("background: black;"));
        assert!(!expanded.contains("@variant dark"));
    }

    #[test]
    fn expands_apply_directive_with_known_utilities() {
        let css = r#"
.card {
  @apply rounded-b-lg shadow-md;
}
"#;
        let config = ironframe_generator::GeneratorConfig {
            minify: false,
            colors: std::collections::BTreeMap::new(),
        };
        let expanded = expand_apply_directives(css, &config, None).expect("apply should expand");
        assert!(!expanded.contains("@apply"));
        assert!(expanded.contains("border-bottom-right-radius"));
        assert!(expanded.contains("var(--radius-lg)"));
        assert!(expanded.contains("box-shadow"));
    }

    #[test]
    fn apply_rejects_unknown_utility() {
        let css = ".x { @apply definitely-not-a-real-utility; }";
        let config = ironframe_generator::GeneratorConfig {
            minify: false,
            colors: std::collections::BTreeMap::new(),
        };
        let err = expand_apply_directives(css, &config, None).expect_err("should fail");
        assert!(err.message.contains("unknown utility in @apply"));
    }

    #[test]
    fn expands_spacing_function_in_css() {
        let css = ".my-element { margin: --spacing(4); }";
        let compiled = expand_build_time_functions(css);
        assert!(compiled.contains("margin: calc(var(--spacing) * 4);"));
    }

    #[test]
    fn expands_alpha_function_in_css() {
        let css = ".my-element { color: --alpha(var(--color-lime-300) / 50%); }";
        let compiled = expand_build_time_functions(css);
        assert!(compiled
            .contains("color: color-mix(in oklab, var(--color-lime-300) 50%, transparent);"));
    }

    #[test]
    fn expands_spacing_function_inside_calc_arbitrary_value() {
        let css = ".x { padding-top: calc(--spacing(4)-1px); }";
        let compiled = expand_build_time_functions(css);
        assert!(compiled.contains("padding-top: calc(calc(var(--spacing) * 4)-1px);"));
    }

    #[test]
    fn expands_nested_variant_directives_in_custom_css() {
        let css = r#"
.my-element {
  @variant dark {
    @variant hover {
      background: black;
    }
  }
}
"#;
        let overrides = ironframe_generator::VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let expanded = expand_variant_directives(css, &overrides);
        assert!(expanded.contains("@media (prefers-color-scheme: dark)"));
        assert!(expanded.contains("&:hover"));
        assert!(expanded.contains("@media (hover: hover)"));
        assert!(!expanded.contains("@variant hover"));
    }

    #[test]
    fn preserves_nested_theme_directives_when_stripping_template() {
        let css = r#"
@media (width >= 40rem) {
  @theme { --color-nested: #111111; }
}
@theme { --color-top: #222222; }
"#;
        let stripped = strip_tailwind_custom_directives(css);
        assert!(stripped.contains("@media (width >= 40rem)"));
        assert!(stripped.contains("@theme { --color-nested: #111111; }"));
        assert!(!stripped.contains("--color-top: #222222"));
    }
}
