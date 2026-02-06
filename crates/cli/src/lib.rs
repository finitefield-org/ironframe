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
            out,
            input_css,
            minify,
            config,
            ignore,
            poll,
            poll_interval_ms,
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
    let mut variant_overrides = None;
    if let Some(path) = input_css_path.as_ref() {
        let template = fs::read_to_string(path).map_err(|err| CliError {
            message: format!("failed to read input css {}: {}", path, err),
        })?;
        variant_overrides = Some(parse_variant_overrides_from_theme(&template));
        template_css = Some(template);
    }

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
    let generation = ironframe_generator::generate_with_overrides(
        &scan_result.classes,
        &generator_config,
        variant_overrides.as_ref(),
    );
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

    if let Some(path) = input_css_path {
        let template = template_css.unwrap_or_default();
        css = apply_framework_import_alias(&template, &css).ok_or_else(|| CliError {
            message: format!(
                "input css {} must include @import \"tailwindcss\" or @import \"ironframe\"",
                path
            ),
        })?;
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
    println!("  ironframe_cli build [--minify] [--out <path>] [--input-css <path>] [--config <path>] [--ignore <glob>] <glob...>");
    println!("  ironframe_cli watch [--minify] [--out <path>] [--input-css <path>] [--config <path>] [--ignore <glob>] [--poll] [--poll-interval <ms>] <glob...>");
    println!();
    println!("EXAMPLES:");
    println!("  ironframe_cli scan \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe_cli scan -i \"**/generated/**\" \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe_cli build --out dist/tailwind.css \"src/**/*.{{html,tsx}}\"");
    println!("  ironframe_cli build --input-css src/app.css --out dist/app.css \"src/**/*.{{html,tsx}}\"");
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
    input_css: Option<String>,
    minify: bool,
    config: Option<String>,
    ignore: Vec<String>,
    poll: bool,
    poll_interval_ms: u64,
) -> Result<(), CliError> {
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
                    run_build(
                        inputs.clone(),
                        out.clone(),
                        input_css.clone(),
                        minify,
                        config.clone(),
                        ignore.clone(),
                    )
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

fn apply_framework_import_alias(template_css: &str, generated_css: &str) -> Option<String> {
    let mut replaced = false;
    let mut rendered_lines = Vec::new();
    let mut generated_bundle = generated_css.to_string();

    for line in template_css.lines() {
        if let Some(directives) = parse_framework_import_directives(line) {
            if !replaced {
                if directives.prefix.is_some() {
                    generated_bundle = apply_prefix_to_css(&generated_bundle, directives.prefix.as_deref()?);
                }
                if directives.important {
                    generated_bundle = apply_important_to_css(&generated_bundle);
                }
                rendered_lines.push(generated_bundle.clone());
                replaced = true;
            }
            continue;
        }
        rendered_lines.push(line.to_string());
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

fn parse_variant_overrides_from_theme(template_css: &str) -> ironframe_generator::VariantOverrides {
    let mut breakpoint_updates = Vec::<(String, String)>::new();
    let mut container_updates = Vec::<(String, String)>::new();

    for block in extract_theme_blocks(template_css) {
        for declaration in block.split(';') {
            let Some((name, value)) = declaration.split_once(':') else {
                continue;
            };
            let name = name.trim();
            let value = value.trim();
            if name.is_empty() || value.is_empty() {
                continue;
            }
            if let Some(raw) = name.strip_prefix("--breakpoint-") {
                breakpoint_updates.push((raw.to_string(), value.to_string()));
            } else if let Some(raw) = name.strip_prefix("--container-") {
                container_updates.push((raw.to_string(), value.to_string()));
            }
        }
    }

    ironframe_generator::VariantOverrides {
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
    }
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

fn extract_theme_blocks(css: &str) -> Vec<&str> {
    let mut blocks = Vec::new();
    let mut cursor = 0usize;

    while let Some(rel_start) = css[cursor..].find("@theme") {
        let theme_idx = cursor + rel_start;
        let Some(open_rel) = css[theme_idx..].find('{') else {
            break;
        };
        let open_idx = theme_idx + open_rel;
        let Some(close_idx) = find_matching_brace(css, open_idx) else {
            break;
        };
        blocks.push(&css[open_idx + 1..close_idx]);
        cursor = close_idx + 1;
    }

    blocks
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ImportDirectives {
    important: bool,
    prefix: Option<String>,
}

fn parse_framework_import_directives(line: &str) -> Option<ImportDirectives> {
    let trimmed = line.trim();
    if !trimmed.starts_with("@import") {
        return None;
    }

    const TARGETS: [&str; 8] = [
        "\"tailwindcss\"",
        "'tailwindcss'",
        "url(\"tailwindcss\")",
        "url('tailwindcss')",
        "\"ironframe\"",
        "'ironframe'",
        "url(\"ironframe\")",
        "url('ironframe')",
    ];

    if !TARGETS.iter().any(|target| trimmed.contains(target)) {
        return None;
    }

    let important = trimmed.contains(" important") || trimmed.ends_with("important;");
    let prefix = extract_prefix_directive(trimmed);

    Some(ImportDirectives { important, prefix })
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
        parse_args, parse_framework_import_directives, parse_variant_overrides_from_theme, Command,
    };

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
    fn accepts_tailwindcss_and_ironframe_import_aliases() {
        assert!(parse_framework_import_directives(r#"@import "tailwindcss";"#).is_some());
        assert!(parse_framework_import_directives(r#"@import "tailwindcss" important;"#).is_some());
        assert!(parse_framework_import_directives(r#"@import "ironframe";"#).is_some());
        assert!(parse_framework_import_directives(r#"@import 'ironframe';"#).is_some());
    }

    #[test]
    fn replaces_framework_import_line_with_generated_css() {
        let generated = "/* generated */\n.p-2 { padding: 0.5rem; }";

        let rendered = apply_framework_import_alias(
            "@import \"tailwindcss\";\nbody { margin: 0; }\n",
            generated,
        )
        .expect("tailwindcss import should be replaced");
        assert!(rendered.contains("/* generated */"));
        assert!(rendered.contains("body { margin: 0; }"));
        assert!(!rendered.contains("@import \"tailwindcss\";"));

        let rendered = apply_framework_import_alias(
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
        let rendered = apply_framework_import_alias(
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
        let rendered = apply_framework_import_alias(
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
        let rendered = apply_framework_import_alias(
            "@import \"tailwindcss\" prefix(tw) important;\n",
            generated,
        )
        .expect("combined directives should be applied");
        assert!(rendered.contains(".tw\\:p-2"));
        assert!(rendered.contains("padding: 0.5rem !important;"));
        assert!(rendered.contains(".tw\\:sm\\:p-2"));
    }

    #[test]
    fn supports_url_import_form() {
        let generated = ".p-2 { padding: 0.5rem; }";
        let rendered = apply_framework_import_alias(
            "@import url('ironframe') prefix(tw);\n",
            generated,
        )
        .expect("url import should be recognized");
        assert!(rendered.contains(".tw\\:p-2"));
    }

    #[test]
    fn prefixes_framework_variable_definitions_and_references() {
        let generated = ":root { --color-red-500: #ef4444; --my-brand: #123; }\n.text-red-500 { color: var(--color-red-500); }\n.bg-brand { background: var(--my-brand); }";
        let rendered = apply_framework_import_alias(
            "@import \"tailwindcss\" prefix(tw);\n",
            generated,
        )
        .expect("prefix import should be replaced");
        assert!(rendered.contains("--tw-color-red-500: #ef4444"));
        assert!(rendered.contains("color: var(--tw-color-red-500)"));
        assert!(rendered.contains("--my-brand: #123"));
        assert!(rendered.contains("var(--my-brand)"));
    }

    #[test]
    fn returns_none_when_framework_import_is_missing() {
        assert!(apply_framework_import_alias("body { color: black; }", ".p-2{}").is_none());
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
}
"#;
        let overrides = parse_variant_overrides_from_theme(css);
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
    }
}
