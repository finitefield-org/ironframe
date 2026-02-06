use globset::{Glob, GlobSet};
use ignore::WalkBuilder;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScanTarget {
    pub path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScanResult {
    pub classes: Vec<String>,
    pub files_scanned: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScanError {
    pub message: String,
}

pub fn scan(_paths: &[PathBuf]) -> Result<ScanResult, ScanError> {
    let mut classes = Vec::new();
    let mut seen = HashSet::new();
    let mut files_scanned = 0;

    for path in _paths {
        scan_path(path, &mut classes, &mut seen, &mut files_scanned)?;
    }

    Ok(ScanResult {
        classes,
        files_scanned,
    })
}

pub fn scan_globs(patterns: &[String]) -> Result<ScanResult, ScanError> {
    scan_globs_with_ignore(patterns, &[])
}

pub fn scan_globs_with_ignore(
    patterns: &[String],
    ignore_patterns: &[String],
) -> Result<ScanResult, ScanError> {
    if patterns.is_empty() {
        return Err(ScanError {
            message: "scan_globs requires at least one pattern".to_string(),
        });
    }

    let globset = build_globset(patterns)?;
    let ignore_set = build_globset(ignore_patterns)?;
    let mut paths = Vec::new();
    let mut seen = HashSet::new();

    let mut builder = WalkBuilder::new(".");
    builder
        .hidden(false)
        .git_ignore(true)
        .git_global(true)
        .git_exclude(true);
    for pattern in ignore_patterns {
        builder.add_ignore(pattern);
    }
    let walker = builder.build();

    for entry in walker {
        let entry = match entry {
            Ok(entry) => entry,
            Err(_) => continue,
        };
        if !entry.file_type().map(|ft| ft.is_file()).unwrap_or(false) {
            continue;
        }
        let path = entry.path();
        if !globset.is_match(path) {
            continue;
        }
        if ignore_set.is_match(path) {
            continue;
        }
        if seen.insert(path.to_path_buf()) {
            paths.push(path.to_path_buf());
        }
    }

    scan(&paths)
}

pub fn extract_classes(_text: &str) -> Vec<String> {
    extract_classes_by_extension(_text, None)
}

fn extract_classes_by_extension(text: &str, ext: Option<&str>) -> Vec<String> {
    let extractor = match ext {
        Some("html") | Some("htm") | Some("vue") | Some("svelte") | Some("astro") => {
            Extractor::Markup
        }
        Some("js") | Some("jsx") | Some("ts") | Some("tsx") | Some("mjs") | Some("cjs") => {
            Extractor::Script
        }
        Some("md") | Some("mdx") => Extractor::Markdown,
        _ => Extractor::Fallback,
    };

    let candidates = extract_candidates(text, extractor);

    let mut results = Vec::new();
    let mut seen = HashSet::new();

    for candidate in candidates {
        for token in tokenize_class_list(candidate.trim()) {
            if is_valid_candidate(&token) && seen.insert(token.clone()) {
                results.push(token);
            }
        }
    }

    results
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Extractor {
    Markup,
    Script,
    Markdown,
    Fallback,
}

fn extract_candidates(text: &str, extractor: Extractor) -> Vec<String> {
    match extractor {
        Extractor::Markup => extract_class_attributes(text),
        Extractor::Script => {
            let mut candidates = extract_string_literals(text);
            candidates.extend(extract_class_helpers(text));
            candidates
        }
        Extractor::Markdown => {
            let mut candidates = extract_class_attributes(text);
            candidates.extend(extract_string_literals(text));
            candidates
        }
        Extractor::Fallback => {
            let mut candidates = extract_class_attributes(text);
            candidates.extend(extract_string_literals(text));
            candidates
        }
    }
}

fn scan_path(
    path: &Path,
    classes: &mut Vec<String>,
    seen: &mut HashSet<String>,
    files_scanned: &mut usize,
) -> Result<(), ScanError> {
    if !path.exists() {
        return Err(ScanError {
            message: format!("path not found: {}", path.display()),
        });
    }

    if path.is_dir() {
        let entries = fs::read_dir(path).map_err(|err| ScanError {
            message: format!("failed to read directory {}: {}", path.display(), err),
        })?;
        for entry in entries {
            let entry = match entry {
                Ok(entry) => entry,
                Err(_) => continue,
            };
            scan_path(&entry.path(), classes, seen, files_scanned)?;
        }
        return Ok(());
    }

    if path.is_file() {
        let text = match fs::read_to_string(path) {
            Ok(text) => text,
            Err(_) => return Ok(()),
        };
        *files_scanned += 1;
        let ext = path
            .extension()
            .and_then(|value| value.to_str())
            .map(|value| value.to_ascii_lowercase());
        let classes_for_file = extract_classes_by_extension(&text, ext.as_deref());
        for class in classes_for_file {
            if seen.insert(class.clone()) {
                classes.push(class);
            }
        }
    }

    Ok(())
}

fn extract_class_attributes(text: &str) -> Vec<String> {
    const ATTRS: [&str; 5] = ["class", "className", "class:list", ":class", "v-bind:class"];
    let mut out = Vec::new();

    for attr in ATTRS {
        for (idx, _) in text.match_indices(attr) {
            if !is_attr_boundary(text, idx, attr.len()) {
                continue;
            }
            let mut pos = idx + attr.len();
            pos = skip_whitespace(text, pos);
            if !text[pos..].starts_with('=') {
                continue;
            }
            pos += 1;
            pos = skip_whitespace(text, pos);
            if pos >= text.len() {
                continue;
            }
            let (values, _) = parse_attribute_value(text, pos);
            out.extend(values);
        }
    }

    out
}

fn extract_class_helpers(text: &str) -> Vec<String> {
    const HELPERS: [&str; 4] = ["clsx", "classnames", "tw", "cva"];
    let mut out = Vec::new();

    for helper in HELPERS {
        for (idx, _) in text.match_indices(helper) {
            if !is_identifier_boundary(text, idx, helper.len()) {
                continue;
            }
            let mut pos = idx + helper.len();
            pos = skip_whitespace(text, pos);
            if pos >= text.len() || !text[pos..].starts_with('(') {
                continue;
            }
            let (args, end) = extract_parenthesized(text, pos);
            if !args.is_empty() {
                out.extend(extract_string_literals(args));
                out.extend(extract_object_keys(args));
            }
            if end <= pos {
                break;
            }
        }
    }

    out
}

fn build_globset(patterns: &[String]) -> Result<GlobSet, ScanError> {
    let mut builder = globset::GlobSetBuilder::new();
    for pattern in patterns {
        let glob = Glob::new(pattern).map_err(|err| ScanError {
            message: format!("invalid glob pattern '{}': {}", pattern, err),
        })?;
        builder.add(glob);
    }
    builder.build().map_err(|err| ScanError {
        message: format!("failed to build glob set: {}", err),
    })
}

fn is_attr_boundary(text: &str, idx: usize, len: usize) -> bool {
    let prev = if idx == 0 {
        None
    } else {
        text[..idx].chars().last()
    };
    let next = text[idx + len..].chars().next();

    let prev_ok = prev.is_none_or(is_boundary_char);
    let next_ok = next.is_none_or(|c| is_boundary_char(c) || c == '=');

    prev_ok && next_ok
}

fn is_identifier_boundary(text: &str, idx: usize, len: usize) -> bool {
    let prev = if idx == 0 {
        None
    } else {
        text[..idx].chars().last()
    };
    let next = text[idx + len..].chars().next();

    let prev_ok = prev.is_none_or(|c| !is_identifier_char(c));
    let next_ok = next.is_none_or(|c| !is_identifier_char(c));

    prev_ok && next_ok
}

fn is_boundary_char(c: char) -> bool {
    !(c.is_ascii_alphanumeric() || c == '_' || c == '-')
}

fn is_identifier_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '$'
}

fn skip_whitespace(text: &str, mut idx: usize) -> usize {
    while idx < text.len() {
        let Some((ch, size)) = next_char(text, idx) else {
            break;
        };
        if !ch.is_whitespace() {
            break;
        }
        idx += size;
    }
    idx
}

fn extract_parenthesized(text: &str, idx: usize) -> (&str, usize) {
    let mut depth: usize = 0;
    let mut pos = idx;
    let mut start = None;

    while pos < text.len() {
        let Some((ch, size)) = next_char(text, pos) else {
            break;
        };
        match ch {
            '(' => {
                depth += 1;
                if depth == 1 {
                    start = Some(pos + size);
                }
                pos += size;
            }
            ')' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    let begin = start.unwrap_or(pos);
                    return (&text[begin..pos], pos + size);
                }
                pos += size;
            }
            '"' | '\'' => {
                let (_, new_pos) = parse_string_literal(text, pos + size, ch);
                pos = new_pos;
            }
            '`' => {
                let (_, new_pos) = parse_template_literal(text, pos + size);
                pos = new_pos;
            }
            _ => pos += size,
        }
    }

    let begin = start.unwrap_or(idx + 1);
    (&text[begin..], text.len())
}

fn parse_attribute_value(text: &str, idx: usize) -> (Vec<String>, usize) {
    let Some((ch, size)) = next_char(text, idx) else {
        return (Vec::new(), idx);
    };

    match ch {
        '"' | '\'' => parse_quoted_value(text, idx + size, ch),
        '{' => parse_braced_value(text, idx),
        _ => parse_unquoted_value(text, idx),
    }
}

fn parse_quoted_value(text: &str, mut idx: usize, quote: char) -> (Vec<String>, usize) {
    let mut value = String::new();
    while idx < text.len() {
        let Some((ch, size)) = next_char(text, idx) else {
            break;
        };
        if ch == '\\' {
            let next_idx = idx + size;
            if let Some((next, next_size)) = next_char(text, next_idx) {
                value.push(next);
                idx = next_idx + next_size;
                continue;
            }
            break;
        }
        if ch == quote {
            idx += size;
            break;
        }
        value.push(ch);
        idx += size;
    }
    (vec![value], idx)
}

fn parse_unquoted_value(text: &str, mut idx: usize) -> (Vec<String>, usize) {
    let mut value = String::new();
    while idx < text.len() {
        let Some((ch, size)) = next_char(text, idx) else {
            break;
        };
        if ch.is_whitespace() || ch == '>' {
            break;
        }
        value.push(ch);
        idx += size;
    }
    (vec![value], idx)
}

fn parse_braced_value(text: &str, idx: usize) -> (Vec<String>, usize) {
    let mut depth: usize = 0;
    let mut pos = idx;
    let mut start = None;
    while pos < text.len() {
        let Some((ch, size)) = next_char(text, pos) else {
            break;
        };
        if ch == '{' {
            depth += 1;
            if depth == 1 {
                start = Some(pos + size);
            }
        } else if ch == '}' {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                let end = pos;
                let inner = start.map(|s| &text[s..end]).unwrap_or("");
                let values = extract_string_literals(inner);
                return (values, pos + size);
            }
        }
        pos += size;
    }
    (Vec::new(), pos)
}

fn extract_string_literals(text: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < text.len() {
        let Some((ch, size)) = next_char(text, idx) else {
            break;
        };
        match ch {
            '"' | '\'' => {
                let (value, new_idx) = parse_string_literal(text, idx + size, ch);
                if !value.is_empty() {
                    out.push(value);
                }
                idx = new_idx;
            }
            '`' => {
                let (values, new_idx) = parse_template_literal(text, idx + size);
                out.extend(values);
                idx = new_idx;
            }
            _ => idx += size,
        }
    }
    out
}

fn parse_string_literal(text: &str, mut idx: usize, quote: char) -> (String, usize) {
    let mut value = String::new();
    while idx < text.len() {
        let Some((ch, size)) = next_char(text, idx) else {
            break;
        };
        if ch == '\\' {
            let next_idx = idx + size;
            if let Some((next, next_size)) = next_char(text, next_idx) {
                value.push(next);
                idx = next_idx + next_size;
                continue;
            }
            break;
        }
        if ch == quote {
            idx += size;
            break;
        }
        value.push(ch);
        idx += size;
    }
    (value, idx)
}

fn parse_template_literal(text: &str, mut idx: usize) -> (Vec<String>, usize) {
    let mut values = Vec::new();
    let mut current = String::new();

    while idx < text.len() {
        let Some((ch, size)) = next_char(text, idx) else {
            break;
        };
        if ch == '`' {
            if !current.is_empty() {
                values.push(current);
            }
            idx += size;
            break;
        }
        if ch == '\\' {
            let next_idx = idx + size;
            if let Some((next, next_size)) = next_char(text, next_idx) {
                current.push(next);
                idx = next_idx + next_size;
                continue;
            }
            break;
        }
        if ch == '$' {
            let next_idx = idx + size;
            if let Some((next, next_size)) = next_char(text, next_idx) {
                if next == '{' {
                    if !current.is_empty() {
                        values.push(current.clone());
                        current.clear();
                    }
                    idx = next_idx + next_size;
                    idx = skip_braced_expression(text, idx);
                    continue;
                }
            }
        }
        current.push(ch);
        idx += size;
    }

    (values, idx)
}

fn skip_braced_expression(text: &str, mut idx: usize) -> usize {
    let mut depth = 1;
    while idx < text.len() && depth > 0 {
        let Some((ch, size)) = next_char(text, idx) else {
            break;
        };
        if ch == '{' {
            depth += 1;
        } else if ch == '}' {
            depth -= 1;
        }
        idx += size;
    }
    idx
}

fn extract_object_keys(text: &str) -> Vec<String> {
    let mut keys = Vec::new();
    let mut idx = 0;

    while idx < text.len() {
        let Some((ch, size)) = next_char(text, idx) else {
            break;
        };
        if ch == '"' || ch == '\'' {
            let (_, new_idx) = parse_string_literal(text, idx + size, ch);
            idx = new_idx;
            continue;
        }
        if ch == '`' {
            let (_, new_idx) = parse_template_literal(text, idx + size);
            idx = new_idx;
            continue;
        }
        if ch == '{' {
            let (body, new_idx) = extract_braced_body(text, idx);
            keys.extend(parse_object_keys_in_body(body));
            idx = new_idx;
            continue;
        }
        idx += size;
    }

    keys
}

fn extract_braced_body(text: &str, idx: usize) -> (&str, usize) {
    let mut depth: usize = 0;
    let mut pos = idx;
    let mut start = None;

    while pos < text.len() {
        let Some((ch, size)) = next_char(text, pos) else {
            break;
        };
        match ch {
            '{' => {
                depth += 1;
                if depth == 1 {
                    start = Some(pos + size);
                }
                pos += size;
            }
            '}' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    let begin = start.unwrap_or(pos);
                    return (&text[begin..pos], pos + size);
                }
                pos += size;
            }
            '"' | '\'' => {
                let (_, new_pos) = parse_string_literal(text, pos + size, ch);
                pos = new_pos;
            }
            '`' => {
                let (_, new_pos) = parse_template_literal(text, pos + size);
                pos = new_pos;
            }
            _ => pos += size,
        }
    }

    let begin = start.unwrap_or(idx + 1);
    (&text[begin..], text.len())
}

fn parse_object_keys_in_body(body: &str) -> Vec<String> {
    let mut keys = Vec::new();
    let mut segments = Vec::new();
    let mut current = String::new();

    let mut depth = 0;
    let mut idx = 0;

    while idx < body.len() {
        let Some((ch, size)) = next_char(body, idx) else {
            break;
        };
        match ch {
            '"' | '\'' => {
                let (value, new_idx) = parse_string_literal(body, idx + size, ch);
                current.push(ch);
                current.push_str(&value);
                current.push(ch);
                idx = new_idx;
                continue;
            }
            '`' => {
                let (values, new_idx) = parse_template_literal(body, idx + size);
                current.push('`');
                for value in values {
                    current.push_str(&value);
                }
                current.push('`');
                idx = new_idx;
                continue;
            }
            '{' | '[' | '(' => {
                depth += 1;
                current.push(ch);
                idx += size;
                continue;
            }
            '}' | ']' | ')' => {
                if depth > 0 {
                    depth -= 1;
                }
                current.push(ch);
                idx += size;
                continue;
            }
            ',' if depth == 0 => {
                segments.push(current.trim().to_string());
                current.clear();
                idx += size;
                continue;
            }
            _ => {
                current.push(ch);
                idx += size;
            }
        }
    }

    if !current.trim().is_empty() {
        segments.push(current.trim().to_string());
    }

    for segment in segments {
        if let Some(key) = parse_object_key(&segment) {
            keys.push(key);
        }
    }

    keys
}

fn parse_object_key(segment: &str) -> Option<String> {
    let trimmed = segment.trim();
    if trimmed.is_empty() {
        return None;
    }
    if trimmed.starts_with("...") {
        return None;
    }

    let mut idx = 0;
    let (ch, size) = next_char(trimmed, idx)?;

    if ch == '"' || ch == '\'' {
        let (value, new_idx) = parse_string_literal(trimmed, idx + size, ch);
        let rest = trimmed[new_idx..].trim_start();
        if rest.starts_with(':') {
            return Some(value);
        }
        return None;
    }

    if !is_identifier_char(ch) {
        return None;
    }

    let mut ident = String::new();
    ident.push(ch);
    idx += size;
    while idx < trimmed.len() {
        let Some((next, next_size)) = next_char(trimmed, idx) else {
            break;
        };
        if !is_identifier_char(next) {
            break;
        }
        ident.push(next);
        idx += next_size;
    }

    let rest = trimmed[idx..].trim_start();
    if rest.starts_with(':') || rest.is_empty() {
        return Some(ident);
    }

    if rest.starts_with(',') || rest.starts_with('}') {
        return Some(ident);
    }

    None
}

fn tokenize_class_list(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut bracket_depth: usize = 0;
    let mut paren_depth: usize = 0;
    let mut idx = 0;

    while idx < input.len() {
        let Some((ch, size)) = next_char(input, idx) else {
            break;
        };
        if ch == '\\' {
            let next_idx = idx + size;
            if let Some((next, next_size)) = next_char(input, next_idx) {
                current.push(next);
                idx = next_idx + next_size;
                continue;
            }
        }

        if ch == '[' {
            bracket_depth += 1;
        } else if ch == ']' {
            bracket_depth = bracket_depth.saturating_sub(1);
        } else if ch == '(' {
            paren_depth += 1;
        } else if ch == ')' {
            paren_depth = paren_depth.saturating_sub(1);
        }

        if ch.is_whitespace() && bracket_depth == 0 && paren_depth == 0 {
            if !current.is_empty() {
                tokens.push(current.clone());
                current.clear();
            }
            idx += size;
            continue;
        }

        current.push(ch);
        idx += size;
    }

    if !current.is_empty() {
        tokens.push(current);
    }

    tokens
}

fn is_valid_candidate(token: &str) -> bool {
    if token.is_empty() {
        return false;
    }
    let mut has_letter_or_bracket = false;
    for ch in token.chars() {
        if ch.is_ascii_alphabetic() || ch == '[' {
            has_letter_or_bracket = true;
        }
        if !is_allowed_char(ch) {
            return false;
        }
    }
    has_letter_or_bracket
}

fn is_allowed_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric()
        || matches!(
            ch,
            '-' | '_' | '/' | ':' | '.' | '%' | '#' | '[' | ']' | '(' | ')' | '!' | '&' | '>' | '+'
        )
}

fn next_char(text: &str, idx: usize) -> Option<(char, usize)> {
    text[idx..].chars().next().map(|ch| (ch, ch.len_utf8()))
}

#[cfg(test)]
mod tests {
    use super::extract_classes;
    use super::extract_classes_by_extension;
    use super::scan_globs;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn extracts_from_class_attribute() {
        let classes = extract_classes(r#"<div class="text-sm bg-red-500"></div>"#);
        assert!(classes.contains(&"text-sm".to_string()));
        assert!(classes.contains(&"bg-red-500".to_string()));
    }

    #[test]
    fn keeps_arbitrary_values_intact() {
        let classes = extract_classes(r#"<div class="bg-[color:var(--brand)]"></div>"#);
        assert!(classes.contains(&"bg-[color:var(--brand)]".to_string()));
    }

    #[test]
    fn handles_template_literals() {
        let classes = extract_classes(r#"const cls = `p-4 ${foo}`;"#);
        assert!(classes.contains(&"p-4".to_string()));
    }

    #[test]
    fn uses_script_extractor_for_ts() {
        let classes = extract_classes_by_extension(r#"const cls = "p-4";"#, Some("ts"));
        assert!(classes.contains(&"p-4".to_string()));
    }

    #[test]
    fn uses_markup_extractor_for_html() {
        let classes = extract_classes_by_extension(r#"<div class="p-2"></div>"#, Some("html"));
        assert!(classes.contains(&"p-2".to_string()));
    }

    #[test]
    fn extracts_from_class_helpers() {
        let source = r#"
            const cls = clsx("p-2", { "bg-red-500": ok, flex: ok });
            const other = classnames({ 'text-sm': ok, underline: ok });
        "#;
        let classes = extract_classes_by_extension(source, Some("ts"));
        assert!(classes.contains(&"p-2".to_string()));
        assert!(classes.contains(&"bg-red-500".to_string()));
        assert!(classes.contains(&"flex".to_string()));
        assert!(classes.contains(&"text-sm".to_string()));
        assert!(classes.contains(&"underline".to_string()));
    }

    #[test]
    fn scans_glob_patterns() {
        let base = temp_dir("scanner_glob");
        let _ = fs::create_dir_all(&base);
        let nested = base.join("nested");
        let _ = fs::create_dir_all(&nested);
        let file_path = nested.join("example.html");
        let _ = fs::write(&file_path, r#"<div class="p-2"></div>"#);

        let result = scan_globs(&["**/*.html".to_string()]).expect("scan_globs should succeed");

        assert!(result.classes.contains(&"p-2".to_string()));
        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn keeps_complex_variants_and_important_tokens_intact() {
        let classes = extract_classes(
            r#"<button class="dark:lg:data-current:hover:bg-indigo-600 [&>[data-active]+span]:text-blue-600 !text-sm"></button>"#,
        );
        assert!(classes.contains(&"dark:lg:data-current:hover:bg-indigo-600".to_string()));
        assert!(classes.contains(&"[&>[data-active]+span]:text-blue-600".to_string()));
        assert!(classes.contains(&"!text-sm".to_string()));
    }

    #[test]
    fn keeps_arbitrary_property_classes_intact() {
        let classes = extract_classes(
            r#"<div class="[--gutter-width:1rem] lg:[--gutter-width:2rem]"></div>"#,
        );
        assert!(classes.contains(&"[--gutter-width:1rem]".to_string()));
        assert!(classes.contains(&"lg:[--gutter-width:2rem]".to_string()));
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
}
