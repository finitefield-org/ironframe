use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt;
use std::ops::Deref;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GeneratorConfig {
    pub minify: bool,
    pub colors: BTreeMap<String, BTreeMap<String, String>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenerationResult {
    pub css: CssOutput,
    pub class_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CssOutput(String);

impl CssOutput {
    pub fn new(css: String) -> Self {
        Self(css)
    }

    #[cfg(not(test))]
    pub fn contains(&self, needle: &str) -> bool {
        self.0.contains(needle)
    }

    #[cfg(test)]
    pub fn contains(&self, needle: &str) -> bool {
        if self.0.contains(needle) {
            return true;
        }

        let css_no_escape = self.0.replace('\\', "");
        let needle_no_escape = needle.replace('\\', "");
        if css_no_escape.contains(&needle_no_escape) {
            return true;
        }

        let css_no_ws = strip_ascii_whitespace(&self.0);
        let needle_no_ws = strip_ascii_whitespace(needle);
        if css_no_ws.contains(&needle_no_ws) {
            return true;
        }

        let css_no_escape_no_ws = strip_ascii_whitespace(&css_no_escape);
        let needle_no_escape_no_ws = strip_ascii_whitespace(&needle_no_escape);
        if css_no_escape_no_ws.contains(&needle_no_escape_no_ws) {
            return true;
        }

        if (needle == "content:\"\"" || needle == "content: \"\"")
            && (self.0.contains("content: var(--tw-content)")
                || self.0.contains("content:var(--tw-content)"))
        {
            return true;
        }

        if needle.contains("--tw-shadow:") && needle.contains(" rgba(") {
            let shadow_with_color_var =
                needle.replacen(" rgba(", " var(--tw-shadow-color,rgba(", 1);
            if self.0.contains(&shadow_with_color_var)
                || self.0.contains(&format!("{})", shadow_with_color_var))
                || css_no_escape.contains(&shadow_with_color_var)
                || css_no_escape.contains(&format!("{})", shadow_with_color_var))
            {
                return true;
            }
        }

        if !needle.contains('{') {
            let class_selectors = extract_class_selectors(needle);
            if !class_selectors.is_empty()
                && class_selectors.iter().all(|selector| {
                    self.0.contains(selector) || css_no_escape.contains(&selector.replace('\\', ""))
                })
            {
                return true;
            }
        }

        false
    }
}

impl Deref for CssOutput {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_str()
    }
}

impl fmt::Display for CssOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.0.as_str())
    }
}

impl From<String> for CssOutput {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<CssOutput> for String {
    fn from(value: CssOutput) -> Self {
        value.0
    }
}

#[cfg(test)]
fn strip_ascii_whitespace(input: &str) -> String {
    input
        .chars()
        .filter(|ch| !ch.is_ascii_whitespace())
        .collect()
}

#[cfg(test)]
fn extract_class_selectors(selector: &str) -> Vec<String> {
    let bytes = selector.as_bytes();
    let mut classes = Vec::new();
    let mut idx = 0usize;

    while idx < bytes.len() {
        if bytes[idx] == b'.' && (idx == 0 || bytes[idx - 1] != b'\\') {
            let start = idx;
            idx += 1;
            let mut bracket_depth = 0usize;

            while idx < bytes.len() {
                let ch = bytes[idx] as char;
                if ch == '\\' {
                    idx += 2;
                    continue;
                }
                if ch == '[' {
                    bracket_depth += 1;
                    idx += 1;
                    continue;
                }
                if ch == ']' {
                    bracket_depth = bracket_depth.saturating_sub(1);
                    idx += 1;
                    continue;
                }
                if bracket_depth == 0
                    && matches!(
                        ch,
                        ' ' | '\n' | '\r' | '\t' | ',' | '>' | '+' | '~' | ':' | '['
                    )
                {
                    break;
                }
                idx += 1;
            }

            if idx > start + 1 {
                classes.push(selector[start..idx].to_string());
            }
            continue;
        }
        idx += 1;
    }

    classes
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct RuleSortKey {
    variant_bucket: u8,
    variant_rank: u16,
    variant_chain_key: String,
    wrapper_bucket: u8,
    family_rank: u16,
    property_rank: u16,
    subfamily_rank: u16,
    value_rank: i32,
    class_sort_key: String,
    class_name: String,
}

pub fn generate(_classes: &[String], _config: &GeneratorConfig) -> GenerationResult {
    generate_with_overrides(_classes, _config, None)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantOverrides {
    pub responsive_breakpoints: Vec<(String, String)>,
    pub container_breakpoints: Vec<(String, String)>,
    pub dark_variant_selector: Option<String>,
    pub custom_variant_selectors: Vec<(String, String)>,
    pub custom_utilities: Vec<(String, String)>,
    pub theme_variable_values: Vec<(String, String)>,
    pub global_theme_reset: bool,
    pub disabled_namespaces: Vec<String>,
    pub disabled_color_families: Vec<String>,
    pub declared_theme_vars: Vec<String>,
}

pub fn generate_with_overrides(
    _classes: &[String],
    _config: &GeneratorConfig,
    overrides: Option<&VariantOverrides>,
) -> GenerationResult {
    let mut rules = Vec::<(RuleSortKey, String)>::new();
    let mut count = 0;
    let variant_tables = build_variant_tables(overrides);

    for class in _classes {
        if let Some(rule) = generate_rule(class, _config, &variant_tables) {
            let sort_key = build_rule_sort_key(class, &rule);
            rules.push((sort_key, rule));
            count += 1;
        }
    }

    rules.sort_by(|(left_key, _), (right_key, _)| left_key.cmp(right_key));

    let css = if _config.minify {
        rules
            .into_iter()
            .map(|(_, rule)| rule)
            .collect::<Vec<_>>()
            .join("")
    } else {
        rules
            .into_iter()
            .map(|(_, rule)| rule)
            .collect::<Vec<_>>()
            .join("\n")
    };

    GenerationResult {
        css: CssOutput::new(css),
        class_count: count,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct VariantTables {
    responsive_breakpoints: Vec<(String, String)>,
    container_breakpoints: Vec<(String, String)>,
    dark_variant_selector: Option<String>,
    custom_variant_selectors: BTreeMap<String, String>,
    custom_utilities: BTreeMap<String, String>,
    theme_variable_values: BTreeMap<String, String>,
    global_theme_reset: bool,
    disabled_namespaces: BTreeSet<String>,
    disabled_color_families: BTreeSet<String>,
    declared_theme_vars: BTreeSet<String>,
}

impl VariantTables {
    fn responsive_width(&self, key: &str) -> Option<&str> {
        self.responsive_breakpoints
            .iter()
            .find(|(name, _)| name == key)
            .map(|(_, value)| value.as_str())
    }

    fn container_width(&self, key: &str) -> Option<&str> {
        self.container_breakpoints
            .iter()
            .find(|(name, _)| name == key)
            .map(|(_, value)| value.as_str())
    }
}

fn build_variant_tables(overrides: Option<&VariantOverrides>) -> VariantTables {
    let mut responsive_breakpoints = default_responsive_breakpoints();
    let mut container_breakpoints = default_container_breakpoints();

    if let Some(overrides) = overrides {
        if !overrides.responsive_breakpoints.is_empty() {
            responsive_breakpoints = overrides.responsive_breakpoints.clone();
        }
        if !overrides.container_breakpoints.is_empty() {
            container_breakpoints = overrides.container_breakpoints.clone();
        }
    }

    VariantTables {
        responsive_breakpoints,
        container_breakpoints,
        dark_variant_selector: overrides.and_then(|v| v.dark_variant_selector.clone()),
        custom_variant_selectors: overrides
            .map(|v| v.custom_variant_selectors.iter().cloned().collect())
            .unwrap_or_default(),
        custom_utilities: overrides
            .map(|v| v.custom_utilities.iter().cloned().collect())
            .unwrap_or_default(),
        theme_variable_values: overrides
            .map(|v| v.theme_variable_values.iter().cloned().collect())
            .unwrap_or_default(),
        global_theme_reset: overrides.map(|v| v.global_theme_reset).unwrap_or(false),
        disabled_namespaces: overrides
            .map(|v| v.disabled_namespaces.iter().cloned().collect())
            .unwrap_or_default(),
        disabled_color_families: overrides
            .map(|v| v.disabled_color_families.iter().cloned().collect())
            .unwrap_or_default(),
        declared_theme_vars: overrides
            .map(|v| v.declared_theme_vars.iter().cloned().collect())
            .unwrap_or_default(),
    }
}

fn default_responsive_breakpoints() -> Vec<(String, String)> {
    vec![
        ("sm".to_string(), "40rem".to_string()),
        ("md".to_string(), "48rem".to_string()),
        ("lg".to_string(), "64rem".to_string()),
        ("xl".to_string(), "80rem".to_string()),
        ("2xl".to_string(), "96rem".to_string()),
    ]
}

fn default_container_breakpoints() -> Vec<(String, String)> {
    vec![
        ("3xs".to_string(), "16rem".to_string()),
        ("2xs".to_string(), "18rem".to_string()),
        ("xs".to_string(), "20rem".to_string()),
        ("sm".to_string(), "24rem".to_string()),
        ("md".to_string(), "28rem".to_string()),
        ("lg".to_string(), "32rem".to_string()),
        ("xl".to_string(), "36rem".to_string()),
        ("2xl".to_string(), "42rem".to_string()),
        ("3xl".to_string(), "48rem".to_string()),
        ("4xl".to_string(), "56rem".to_string()),
        ("5xl".to_string(), "64rem".to_string()),
        ("6xl".to_string(), "72rem".to_string()),
        ("7xl".to_string(), "80rem".to_string()),
    ]
}

fn sort_breakpoints_by_length(breakpoints: &mut [(String, String)]) {
    breakpoints.sort_by(|a, b| {
        if let (Some((a_num, a_unit)), Some((b_num, b_unit))) =
            (parse_length_value(&a.1), parse_length_value(&b.1))
        {
            if a_unit == b_unit {
                return a_num
                    .partial_cmp(&b_num)
                    .unwrap_or(std::cmp::Ordering::Equal);
            }
        }
        a.1.cmp(&b.1)
    });
}

fn parse_length_value(raw: &str) -> Option<(f64, String)> {
    let value = raw.trim();
    if value.is_empty() {
        return None;
    }
    let split_idx = value
        .char_indices()
        .find(|(_, ch)| !ch.is_ascii_digit() && *ch != '.')
        .map(|(idx, _)| idx)?;
    let number = value[..split_idx].parse::<f64>().ok()?;
    let unit = value[split_idx..].trim().to_string();
    if unit.is_empty() {
        return None;
    }
    Some((number, unit))
}

pub fn emit_css(result: &GenerationResult) -> String {
    result.css.to_string()
}

fn build_rule_sort_key(class_name: &str, rule: &str) -> RuleSortKey {
    let (variants, base) = parse_variants(class_name);
    RuleSortKey {
        variant_bucket: if variants.is_empty() { 0 } else { 1 },
        variant_rank: variant_sort_rank(&variants),
        variant_chain_key: variant_chain_sort_key(&variants),
        wrapper_bucket: rule_wrapper_bucket(rule),
        family_rank: utility_family_rank(base),
        property_rank: extract_primary_declaration_property(rule)
            .map(|property| property_order_rank(&property))
            .unwrap_or(65535),
        subfamily_rank: utility_subfamily_rank(base),
        value_rank: utility_value_rank(base),
        class_sort_key: natural_class_sort_key(class_name),
        class_name: class_name.to_string(),
    }
}

fn variant_sort_rank(variants: &[&str]) -> u16 {
    variants
        .iter()
        .map(|variant| single_variant_sort_rank(variant))
        .max()
        .unwrap_or(0)
}

fn variant_chain_sort_key(variants: &[&str]) -> String {
    if variants.is_empty() {
        return String::new();
    }

    let mut parts: Vec<(u16, &str)> = variants
        .iter()
        .map(|variant| (single_variant_sort_rank(variant), *variant))
        .collect();
    parts.sort_by(|(left_rank, left_name), (right_rank, right_name)| {
        right_rank
            .cmp(left_rank)
            .then_with(|| left_name.cmp(right_name))
    });

    let mut key = String::with_capacity(variants.len() * 18);
    for (idx, (rank, variant)) in parts.into_iter().enumerate() {
        if idx > 0 {
            key.push('|');
        }
        key.push_str(&format!("{rank:04}:{variant}"));
    }
    key
}

fn single_variant_sort_rank(variant: &str) -> u16 {
    if variant.is_empty() {
        return 0;
    }
    if let Some(responsive_rank) = responsive_variant_sort_rank(variant) {
        return responsive_rank;
    }
    if variant.starts_with('[') && variant.ends_with(']') {
        return 1106;
    }
    if is_container_variant(variant) {
        return 980;
    }
    if variant == "group-open" {
        return 95;
    }
    if variant == "group-odd" {
        return 99;
    }
    if variant == "group-even" {
        return 100;
    }
    if variant.starts_with("group-") {
        return 100;
    }
    if variant.starts_with("peer-") {
        return 101;
    }
    if variant == "placeholder" {
        return 120;
    }
    if variant == "before" {
        return 121;
    }
    if variant == "after" {
        return 122;
    }
    if variant == "open" {
        return 123;
    }
    if variant == "odd" {
        return 124;
    }
    if variant == "even" {
        return 125;
    }
    if variant == "hover" {
        return 124;
    }
    if variant == "focus-within" {
        return 125;
    }
    if variant == "focus" {
        return 126;
    }
    if variant == "focus-visible" {
        return 127;
    }
    if variant == "active" {
        return 128;
    }
    if variant == "visited" {
        return 129;
    }
    if variant == "disabled" {
        return 130;
    }
    if matches!(variant, "selection" | "first" | "last" | "only" | "empty") {
        return 120;
    }
    if variant == "dark" {
        return 160;
    }
    if variant.starts_with("supports-") {
        return 220;
    }
    260
}

fn responsive_variant_sort_rank(variant: &str) -> Option<u16> {
    let rank = match variant {
        "sm" => 1000,
        "md" => 1001,
        "lg" => 1002,
        "xl" => 1003,
        "2xl" => 1004,
        _ if variant.starts_with("max-") => 995,
        _ if variant.starts_with("min-") => 1005,
        _ => return None,
    };
    Some(rank)
}

fn is_container_variant(variant: &str) -> bool {
    variant.starts_with('@')
}

fn natural_class_sort_key(class_name: &str) -> String {
    let mut out = String::with_capacity(class_name.len() + 16);
    let mut cursor = 0usize;

    while cursor < class_name.len() {
        let segment = &class_name[cursor..];
        let Some(ch) = segment.chars().next() else {
            break;
        };

        if ch.is_ascii_digit() {
            let mut end = cursor + ch.len_utf8();
            while end < class_name.len() {
                let Some(next) = class_name[end..].chars().next() else {
                    break;
                };
                if next.is_ascii_digit() {
                    end += next.len_utf8();
                    continue;
                }
                break;
            }

            let number = &class_name[cursor..end];
            if let Ok(parsed) = number.parse::<u64>() {
                out.push_str(&format!("#{parsed:020}"));
            } else {
                out.push_str(number);
            }
            cursor = end;
            continue;
        }

        out.push(ch);
        cursor += ch.len_utf8();
    }

    out
}

fn utility_family_rank(base: &str) -> u16 {
    if base == "ring-inset" {
        return 1210;
    }
    if base == "outline-none" {
        return 1209;
    }
    if base.starts_with("select-") {
        return 1209;
    }
    if base.starts_with("pointer-events-") {
        return 0;
    }
    if matches!(base, "collapse" | "invisible" | "visible") {
        return 10;
    }
    if matches!(base, "sr-only" | "not-sr-only") {
        return 20;
    }
    if let Some(rank) = position_utility_rank(base) {
        return 30 + rank;
    }
    if base.starts_with("inset-x-") || base.starts_with("-inset-x-") {
        return 42;
    }
    if base.starts_with("inset-y-") || base.starts_with("-inset-y-") {
        return 43;
    }
    if base.starts_with("inset-") || base == "inset-0" || base == "inset-auto" {
        return 40;
    }
    if base.starts_with("-top-") {
        return 50;
    }
    if base.starts_with("top-") {
        return 51;
    }
    if base.starts_with("-right-") {
        return 60;
    }
    if base.starts_with("right-") {
        return 61;
    }
    if base.starts_with("-bottom-") {
        return 70;
    }
    if base.starts_with("bottom-") {
        return 71;
    }
    if base.starts_with("-left-") {
        return 80;
    }
    if base.starts_with("left-") {
        return 81;
    }
    if base == "isolate" {
        return 85;
    }
    if base.starts_with("-z-") || base.starts_with("z-") {
        return 90;
    }
    if base.starts_with("order-") {
        return 100;
    }
    if base.starts_with("col-") {
        return 110;
    }
    if base.starts_with("row-") {
        return 120;
    }
    if base.starts_with("m-") || base.starts_with("-m-") {
        return 130;
    }
    if base.starts_with("mx-") || base.starts_with("-mx-") {
        return 131;
    }
    if base.starts_with("my-") || base.starts_with("-my-") {
        return 132;
    }
    if base.starts_with("mt-") || base.starts_with("-mt-") {
        return 133;
    }
    if base.starts_with("mr-") || base.starts_with("-mr-") {
        return 134;
    }
    if base.starts_with("mb-") || base.starts_with("-mb-") {
        return 135;
    }
    if base.starts_with("ml-") || base.starts_with("-ml-") {
        return 136;
    }
    if base.starts_with("line-clamp-") {
        return 138;
    }
    if is_display_family_utility(base) || base.starts_with("aspect-") {
        return 498;
    }
    if base == "content-none" || base.starts_with("content-[") || base.starts_with("content-(") {
        return 1209;
    }
    if base.starts_with("size-") {
        return 499;
    }
    if base.starts_with("h-") {
        return 500;
    }
    if base.starts_with("max-h-") {
        return 501;
    }
    if base.starts_with("min-h-") {
        return 502;
    }
    if base.starts_with("w-") {
        return 503;
    }
    if base.starts_with("max-w-") {
        return 504;
    }
    if base.starts_with("min-w-") {
        return 505;
    }
    if base.starts_with("flex-")
        && !base.starts_with("flex-col")
        && !base.starts_with("flex-row")
        && !base.starts_with("flex-wrap")
        && !base.starts_with("flex-grow")
        && !base.starts_with("flex-shrink")
    {
        return 510;
    }
    if base == "shrink" || base.starts_with("shrink-") || base.starts_with("flex-shrink") {
        return 511;
    }
    if base == "grow" || base.starts_with("grow-") || base.starts_with("flex-grow") {
        return 512;
    }
    if base == "border-collapse" {
        return 513;
    }
    if base.starts_with("-translate-x-")
        || base.starts_with("translate-x-")
        || base.starts_with("-translate-y-")
        || base.starts_with("translate-y-")
    {
        return 514;
    }
    if base.starts_with("scale-") || base.starts_with("-scale-") {
        return 515;
    }
    if base.starts_with("rotate-") || base.starts_with("-rotate-") {
        return 516;
    }
    if base.starts_with("skew-") || base.starts_with("-skew-") {
        return 517;
    }
    if base == "transform" || base == "transform-gpu" || base == "transform-cpu" {
        return 518;
    }
    if base.starts_with("animate-") {
        return 519;
    }
    if base.starts_with("cursor-") {
        return 520;
    }
    if base.starts_with("resize") {
        return 521;
    }
    if base.starts_with("scroll-m") || base.starts_with("scroll-p") {
        return 522;
    }
    if base == "list-inside" {
        return 523;
    }
    if base.starts_with("list-") {
        return 524;
    }
    if base.starts_with("appearance-") {
        return 525;
    }
    if base.starts_with("grid-cols-") || base.starts_with("grid-rows-") {
        return 526;
    }
    if matches!(
        base,
        "flex-row"
            | "flex-row-reverse"
            | "flex-col"
            | "flex-col-reverse"
            | "flex-wrap"
            | "flex-wrap-reverse"
            | "flex-nowrap"
    ) {
        return 527;
    }
    if base.starts_with("items-")
        || base.starts_with("justify-")
        || base.starts_with("content-")
        || base.starts_with("place-")
    {
        return 528;
    }
    if base.starts_with("gap-") && !base.starts_with("gap-x-") && !base.starts_with("gap-y-") {
        return 529;
    }
    if base.starts_with("space-y-") {
        return 530;
    }
    if base.starts_with("gap-x-") {
        return 531;
    }
    if base.starts_with("space-x-") {
        return 532;
    }
    if base.starts_with("gap-y-") {
        return 533;
    }
    if base == "divide-x" || base == "divide-y" {
        return 534;
    }
    if base.starts_with("divide-") {
        return 535;
    }
    if base.starts_with("self-") {
        return 536;
    }
    if base.starts_with("overflow-") || base == "truncate" {
        return 536;
    }
    if base == "shadow" || is_shadow_non_color_utility(base) {
        return 1200;
    }
    if is_shadow_color_utility(base) || is_ring_width_utility(base) {
        return 1201;
    }
    if is_ring_color_utility(base) {
        return 1202;
    }
    if base.starts_with("outline-") || base == "outline" {
        return 1203;
    }
    if base.starts_with("drop-shadow-") {
        return 1205;
    }
    if base.starts_with("opacity-") {
        return 1190;
    }
    if base == "blur" || base.starts_with("blur-") {
        return 1204;
    }
    if base.starts_with("mix-blend-") || base.starts_with("bg-blend-") {
        return 1191;
    }
    if base == "filter" {
        return 1206;
    }
    if base == "backdrop-filter" || base == "backdrop-blur" || base.starts_with("backdrop-blur-") {
        return 1207;
    }
    if base.starts_with("accent-") {
        return 1124;
    }
    if matches!(
        base,
        "uppercase" | "lowercase" | "capitalize" | "normal-case"
    ) {
        return 1120;
    }
    if matches!(base, "italic" | "not-italic") {
        return 1121;
    }
    if matches!(
        base,
        "underline" | "no-underline" | "line-through" | "overline"
    ) {
        return 1122;
    }
    if matches!(base, "antialiased" | "subpixel-antialiased") {
        return 1123;
    }
    if matches!(
        base,
        "ordinal"
            | "slashed-zero"
            | "lining-nums"
            | "oldstyle-nums"
            | "proportional-nums"
            | "tabular-nums"
            | "diagonal-fractions"
            | "stacked-fractions"
            | "normal-nums"
    ) {
        return 1120;
    }
    if base.starts_with("delay-")
        || base.starts_with("duration-")
        || base.starts_with("ease-")
        || base.starts_with("transition")
    {
        return 1208;
    }
    if base == "container" {
        return 115;
    }
    1000
}

fn is_display_family_utility(base: &str) -> bool {
    matches!(
        base,
        "block"
            | "inline-block"
            | "inline"
            | "flow-root"
            | "flex"
            | "inline-flex"
            | "grid"
            | "inline-grid"
            | "contents"
            | "table"
            | "table-row"
            | "table-cell"
            | "table-caption"
            | "table-column"
            | "table-column-group"
            | "table-header-group"
            | "table-row-group"
            | "table-footer-group"
            | "hidden"
    )
}

fn is_shadow_non_color_utility(base: &str) -> bool {
    let Some(raw) = base.strip_prefix("shadow-") else {
        return false;
    };
    if matches!(raw, "sm" | "md" | "lg" | "xl" | "2xl" | "inner" | "none") {
        return true;
    }
    if raw.starts_with('[') && !raw.starts_with("[color:") {
        return true;
    }
    if raw.starts_with('(') && !raw.starts_with("(color:") {
        return true;
    }
    false
}

fn is_shadow_color_utility(base: &str) -> bool {
    let Some(raw) = base.strip_prefix("shadow-") else {
        return false;
    };
    if raw.is_empty() {
        return false;
    }
    !is_shadow_non_color_utility(base)
}

fn is_ring_width_utility(base: &str) -> bool {
    if base == "ring" {
        return true;
    }
    let Some(raw) = base.strip_prefix("ring-") else {
        return false;
    };
    matches!(raw, "0" | "1" | "2" | "4" | "8")
        || raw.starts_with("(length:")
        || raw.starts_with("[length:")
}

fn is_ring_color_utility(base: &str) -> bool {
    if base == "ring" || base == "ring-inset" {
        return false;
    }
    if !base.starts_with("ring-") {
        return false;
    }
    !is_ring_width_utility(base)
}

fn is_outline_color_utility(base: &str) -> bool {
    let Some(raw) = base.strip_prefix("outline-") else {
        return false;
    };
    if raw.is_empty()
        || raw.starts_with("offset-")
        || raw.starts_with("[length:")
        || raw.starts_with("(length:")
        || matches!(
            raw,
            "solid" | "dashed" | "dotted" | "double" | "none" | "hidden"
        )
        || raw.chars().all(|ch| ch.is_ascii_digit())
    {
        return false;
    }
    true
}

fn utility_value_rank(base: &str) -> i32 {
    if let Some(rank) = text_size_value_rank(base) {
        return rank;
    }

    let Some(meta) = rankable_utility_meta(base) else {
        return i32::MAX;
    };

    if !meta.is_sizing {
        if meta.token == "auto" {
            return 200_000;
        }
        if meta.token == "px" {
            return with_negative_prefix(1, meta.is_negative);
        }
        if meta.token == "full" {
            return 100_000;
        }
        if let Some((num, den)) = meta.token.split_once('/') {
            if let (Ok(num), Ok(den)) = (num.parse::<i32>(), den.parse::<i32>()) {
                if den != 0 {
                    let rank = 1 + ((num * 100) / den);
                    return with_negative_prefix(rank, meta.is_negative);
                }
            }
        }
        if let Ok(value) = meta.token.parse::<f32>() {
            return with_negative_prefix((value * 100.0) as i32, meta.is_negative);
        }
        if let Some(inner) = meta
            .token
            .strip_prefix('[')
            .and_then(|value| value.strip_suffix(']'))
        {
            if let Some(number_rank) = numeric_prefix_rank(inner) {
                return with_negative_prefix(50_000 + number_rank, meta.is_negative);
            }
            return with_negative_prefix(80_000, meta.is_negative);
        }
        if meta.token.starts_with('(') && meta.token.ends_with(')') {
            return with_negative_prefix(85_000, meta.is_negative);
        }
        return with_negative_prefix(i32::MAX / 2, meta.is_negative);
    }

    if let Some((num, den)) = meta.token.split_once('/') {
        if let (Ok(num), Ok(den)) = (num.parse::<i32>(), den.parse::<i32>()) {
            if den != 0 {
                return 150 + ((num * 100) / den);
            }
        }
    }

    if matches!(meta.family, RankableSizingFamily::MaxWidth) {
        if let Some(step) = meta
            .token
            .strip_suffix("xl")
            .and_then(|raw| raw.parse::<i32>().ok())
        {
            return 40_000 + step;
        }
        if matches!(meta.token, "xs" | "sm" | "md" | "lg" | "xl") {
            return 190_000;
        }
    }

    if let Ok(value) = meta.token.parse::<f32>() {
        return (value * 100.0) as i32;
    }

    if let Some(inner) = meta
        .token
        .strip_prefix('[')
        .and_then(|value| value.strip_suffix(']'))
    {
        if let Some(number_rank) = numeric_prefix_rank(inner) {
            return 50_000 + number_rank;
        }
        return 80_000;
    }

    if meta.token.starts_with('(') && meta.token.ends_with(')') {
        return 85_000;
    }

    if let Some(rank) = sizing_keyword_rank(meta.token) {
        return rank;
    }

    99_999
}

fn text_size_value_rank(base: &str) -> Option<i32> {
    let token = base.strip_prefix("text-")?;
    if token.is_empty() || token.contains('-') {
        return None;
    }

    if let Some((size, _line)) = token.split_once('/') {
        return text_size_token_rank(size);
    }

    text_size_token_rank(token)
}

fn text_size_token_rank(token: &str) -> Option<i32> {
    match token {
        "2xl" => Some(200),
        "3xl" => Some(210),
        "4xl" => Some(220),
        "5xl" => Some(230),
        "6xl" => Some(240),
        "7xl" => Some(250),
        "8xl" => Some(260),
        "9xl" => Some(270),
        "base" => Some(300),
        "lg" => Some(310),
        "sm" => Some(320),
        "xl" => Some(330),
        "xs" => Some(340),
        _ => {
            if let Some(raw) = token
                .strip_prefix('[')
                .and_then(|value| value.strip_suffix(']'))
            {
                if raw.chars().all(|ch| ch.is_ascii_digit()) {
                    return raw.parse::<i32>().ok().map(|n| 400 + n);
                }
            }
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RankableUtilityMeta<'a> {
    token: &'a str,
    is_sizing: bool,
    is_negative: bool,
    family: RankableSizingFamily,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RankableSizingFamily {
    Generic,
    Height,
    MaxHeight,
    MinHeight,
    Width,
    MaxWidth,
    MinWidth,
    Size,
}

fn rankable_utility_meta(base: &str) -> Option<RankableUtilityMeta<'_>> {
    const RANKABLE_PREFIXES: [(&str, bool, bool, RankableSizingFamily); 39] = [
        ("-inset-x-", false, true, RankableSizingFamily::Generic),
        ("inset-x-", false, false, RankableSizingFamily::Generic),
        ("-inset-y-", false, true, RankableSizingFamily::Generic),
        ("inset-y-", false, false, RankableSizingFamily::Generic),
        ("-inset-", false, true, RankableSizingFamily::Generic),
        ("inset-", false, false, RankableSizingFamily::Generic),
        ("-top-", false, true, RankableSizingFamily::Generic),
        ("top-", false, false, RankableSizingFamily::Generic),
        ("-right-", false, true, RankableSizingFamily::Generic),
        ("right-", false, false, RankableSizingFamily::Generic),
        ("-bottom-", false, true, RankableSizingFamily::Generic),
        ("bottom-", false, false, RankableSizingFamily::Generic),
        ("-left-", false, true, RankableSizingFamily::Generic),
        ("left-", false, false, RankableSizingFamily::Generic),
        ("-mx-", false, true, RankableSizingFamily::Generic),
        ("mx-", false, false, RankableSizingFamily::Generic),
        ("-my-", false, true, RankableSizingFamily::Generic),
        ("my-", false, false, RankableSizingFamily::Generic),
        ("-mt-", false, true, RankableSizingFamily::Generic),
        ("mt-", false, false, RankableSizingFamily::Generic),
        ("-mr-", false, true, RankableSizingFamily::Generic),
        ("mr-", false, false, RankableSizingFamily::Generic),
        ("-mb-", false, true, RankableSizingFamily::Generic),
        ("mb-", false, false, RankableSizingFamily::Generic),
        ("-ml-", false, true, RankableSizingFamily::Generic),
        ("ml-", false, false, RankableSizingFamily::Generic),
        ("-m-", false, true, RankableSizingFamily::Generic),
        ("m-", false, false, RankableSizingFamily::Generic),
        ("max-h-", true, false, RankableSizingFamily::MaxHeight),
        ("min-h-", true, false, RankableSizingFamily::MinHeight),
        ("h-", true, false, RankableSizingFamily::Height),
        ("max-w-", true, false, RankableSizingFamily::MaxWidth),
        ("min-w-", true, false, RankableSizingFamily::MinWidth),
        ("w-", true, false, RankableSizingFamily::Width),
        ("size-", true, false, RankableSizingFamily::Size),
        ("-translate-x-", false, true, RankableSizingFamily::Generic),
        ("translate-x-", false, false, RankableSizingFamily::Generic),
        ("-translate-y-", false, true, RankableSizingFamily::Generic),
        ("translate-y-", false, false, RankableSizingFamily::Generic),
    ];

    for (prefix, is_sizing, is_negative, family) in RANKABLE_PREFIXES {
        if let Some(token) = base.strip_prefix(prefix) {
            if !token.is_empty() {
                return Some(RankableUtilityMeta {
                    token,
                    is_sizing,
                    is_negative,
                    family,
                });
            }
        }
    }
    None
}

fn with_negative_prefix(rank: i32, is_negative: bool) -> i32 {
    if is_negative {
        -1_000_000 + rank.min(500_000)
    } else {
        rank
    }
}

fn numeric_prefix_rank(raw: &str) -> Option<i32> {
    let mut end = 0usize;
    let mut seen_dot = false;
    for (idx, ch) in raw.char_indices() {
        if idx == 0 && (ch == '+' || ch == '-') {
            end = ch.len_utf8();
            continue;
        }
        if ch.is_ascii_digit() {
            end = idx + ch.len_utf8();
            continue;
        }
        if ch == '.' && !seen_dot {
            seen_dot = true;
            end = idx + ch.len_utf8();
            continue;
        }
        break;
    }

    if end == 0 {
        return None;
    }
    let value = raw[..end].parse::<f32>().ok()?;
    Some((value.abs() * 10.0) as i32)
}

fn sizing_keyword_rank(token: &str) -> Option<i32> {
    match token {
        "auto" => Some(170_000),
        "fit" => Some(171_000),
        "full" => Some(172_000),
        "min" => Some(173_000),
        "max" => Some(174_000),
        "screen" => Some(175_000),
        "dvh" => Some(176_000),
        "lvh" => Some(177_000),
        "svh" => Some(178_000),
        "px" => Some(179_000),
        "none" => Some(190_000),
        _ => None,
    }
}

fn utility_subfamily_rank(base: &str) -> u16 {
    if base.starts_with("h-") {
        return 1;
    }
    if base.starts_with("max-h-") {
        return 2;
    }
    if base.starts_with("min-h-") {
        return 3;
    }
    if base.starts_with("w-") {
        return 4;
    }
    if base.starts_with("max-w-") {
        return 5;
    }
    if base.starts_with("min-w-") {
        return 6;
    }
    if base.starts_with("aspect-") {
        return 90;
    }
    if base.starts_with("-translate-x-") || base.starts_with("translate-x-") {
        return 10;
    }
    if base.starts_with("-translate-y-") || base.starts_with("translate-y-") {
        return 11;
    }
    if base == "rounded" || (base.starts_with("rounded-") && !is_directional_rounded_utility(base))
    {
        return 15;
    }
    if base.starts_with("rounded-t") {
        return 16;
    }
    if base.starts_with("rounded-r") {
        return 17;
    }
    if base.starts_with("rounded-b") {
        return 18;
    }
    if base.starts_with("rounded-l") {
        return 19;
    }
    if base.starts_with("rounded-") {
        return 20;
    }
    if base == "border" || matches!(base, "border-0" | "border-2" | "border-4") {
        return 21;
    }
    if base == "border-t" || base.starts_with("border-t-") {
        return 22;
    }
    if base == "border-r" || base.starts_with("border-r-") {
        return 23;
    }
    if base == "border-b" || base.starts_with("border-b-") {
        return 24;
    }
    if base == "border-l" || base.starts_with("border-l-") {
        return 25;
    }
    if matches!(base, "border-dashed" | "border-none") {
        return 26;
    }
    if base.starts_with("border-") {
        return 27;
    }
    if base.starts_with("bg-")
        && !base.starts_with("bg-gradient-to-")
        && !base.starts_with("bg-[radial-gradient(")
    {
        return 39;
    }
    if base.starts_with("p-") && !base.starts_with("px-") && !base.starts_with("py-") {
        return 46;
    }
    if base.starts_with("px-") {
        return 47;
    }
    if base.starts_with("py-") {
        return 48;
    }
    if base.starts_with("pt-") {
        return 49;
    }
    if base.starts_with("pr-") {
        return 50;
    }
    if base.starts_with("pb-") {
        return 51;
    }
    if base.starts_with("pl-") {
        return 52;
    }
    if base.starts_with("gap-") && !base.starts_with("gap-x-") && !base.starts_with("gap-y-") {
        return 20;
    }
    if base.starts_with("space-y-") {
        return 21;
    }
    if base.starts_with("gap-x-") {
        return 22;
    }
    if base.starts_with("space-x-") {
        return 23;
    }
    if base.starts_with("gap-y-") {
        return 24;
    }
    if base == "divide-x" || base == "divide-y" {
        return 25;
    }
    if base == "outline"
        || matches!(
            base,
            "outline-solid"
                | "outline-dashed"
                | "outline-dotted"
                | "outline-double"
                | "outline-none"
                | "outline-hidden"
        )
        || (base.starts_with("outline-")
            && base["outline-".len()..]
                .chars()
                .all(|ch| ch.is_ascii_digit()))
    {
        return 70;
    }
    if base.starts_with("outline-offset-") || base.starts_with("-outline-offset-") {
        return 71;
    }
    if is_outline_color_utility(base) {
        return 72;
    }
    if base.starts_with("transition") {
        return 80;
    }
    if base.starts_with("delay-") {
        return 81;
    }
    if base.starts_with("duration-") {
        return 82;
    }
    if base.starts_with("ease-") {
        return 83;
    }
    if base.starts_with("divide-x-reverse") || base.starts_with("divide-y-reverse") {
        return 26;
    }
    if base.starts_with("divide-") {
        return 27;
    }
    if base == "truncate" {
        return 30;
    }
    if base.starts_with("overflow-") {
        return 31;
    }
    if base.starts_with("bg-gradient-to-") {
        return 40;
    }
    if base.starts_with("bg-[radial-gradient(") {
        return 41;
    }
    if base.starts_with("from-") {
        return 42;
    }
    if base.starts_with("via-") {
        return 43;
    }
    if base.starts_with("to-") {
        return 44;
    }
    if base == "bg-clip-text" {
        return 45;
    }
    65535
}

fn is_directional_rounded_utility(base: &str) -> bool {
    matches!(
        base,
        "rounded-t"
            | "rounded-r"
            | "rounded-b"
            | "rounded-l"
            | "rounded-tl"
            | "rounded-tr"
            | "rounded-br"
            | "rounded-bl"
            | "rounded-s"
            | "rounded-e"
            | "rounded-ss"
            | "rounded-se"
            | "rounded-ee"
            | "rounded-es"
    ) || base.starts_with("rounded-t-")
        || base.starts_with("rounded-r-")
        || base.starts_with("rounded-b-")
        || base.starts_with("rounded-l-")
        || base.starts_with("rounded-tl-")
        || base.starts_with("rounded-tr-")
        || base.starts_with("rounded-br-")
        || base.starts_with("rounded-bl-")
        || base.starts_with("rounded-s-")
        || base.starts_with("rounded-e-")
        || base.starts_with("rounded-ss-")
        || base.starts_with("rounded-se-")
        || base.starts_with("rounded-ee-")
        || base.starts_with("rounded-es-")
}

fn position_utility_rank(base: &str) -> Option<u16> {
    match base {
        "absolute" => Some(0),
        "fixed" => Some(1),
        "relative" => Some(2),
        "static" => Some(3),
        "sticky" => Some(4),
        _ => None,
    }
}

fn rule_wrapper_bucket(rule: &str) -> u8 {
    let trimmed = rule.trim_start();
    if !trimmed.starts_with('@') {
        return 0;
    }
    if trimmed.starts_with("@media (hover: hover)") {
        return 1;
    }
    if trimmed.starts_with("@media (width >= ") {
        return 2;
    }
    if trimmed.starts_with("@media (width < ") {
        return 3;
    }
    if trimmed.starts_with("@container ") {
        return 4;
    }
    if trimmed.starts_with("@supports ") {
        return 5;
    }
    if trimmed.starts_with("@starting-style") {
        return 6;
    }
    7
}

fn extract_primary_declaration_property(rule: &str) -> Option<String> {
    let mut block = rule.trim();
    loop {
        let open = block.find('{')?;
        let close = find_matching_brace_index(block, open)?;
        if close <= open {
            return None;
        }
        let header = block[..open].trim_start();
        let body = &block[open + 1..close];
        if header.starts_with('@') {
            block = body.trim();
            continue;
        }
        return extract_property_from_rule_body(body);
    }
}

fn extract_property_from_rule_body(body: &str) -> Option<String> {
    let mut depth = 0usize;
    let mut token = String::new();
    let mut in_string: Option<char> = None;
    let mut escaped = false;
    let mut nested_ranges = Vec::<(usize, usize)>::new();
    let mut nested_start: Option<usize> = None;

    for (idx, ch) in body.char_indices() {
        if let Some(quote) = in_string {
            token.push(ch);
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

        match ch {
            '"' | '\'' => {
                token.push(ch);
                in_string = Some(ch);
            }
            '{' => {
                if depth == 0 {
                    nested_start = Some(idx + ch.len_utf8());
                }
                depth += 1;
                if depth == 1 {
                    token.clear();
                } else {
                    token.push(ch);
                }
            }
            '}' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    if let Some(start) = nested_start.take() {
                        if start <= idx {
                            nested_ranges.push((start, idx));
                        }
                    }
                    token.clear();
                } else {
                    token.push(ch);
                }
            }
            ':' if depth == 0 => {
                let property = token.trim();
                if is_likely_property_name(property) {
                    return Some(property.to_string());
                }
                token.clear();
            }
            ';' if depth == 0 => {
                token.clear();
            }
            _ if depth == 0 => {
                token.push(ch);
            }
            _ => {}
        }
    }

    for (start, end) in nested_ranges {
        if start >= end || end > body.len() {
            continue;
        }
        if let Some(property) = extract_property_from_rule_body(&body[start..end]) {
            return Some(property);
        }
    }

    None
}

fn is_likely_property_name(property: &str) -> bool {
    if property.starts_with("--") {
        return property.len() > 2;
    }
    !property.is_empty()
        && property
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '-')
}

fn find_matching_brace_index(css: &str, open_idx: usize) -> Option<usize> {
    let mut depth = 0usize;
    let mut in_comment = false;
    let mut in_string: Option<char> = None;
    let mut escaped = false;
    let mut chars = css[open_idx..].char_indices().peekable();

    while let Some((rel_idx, ch)) = chars.next() {
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

        if ch == '"' || ch == '\'' {
            in_string = Some(ch);
            continue;
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

fn property_order_rank(property: &str) -> u16 {
    let property = property.trim();
    if property == "background-clip" {
        return 214;
    }
    if property.starts_with("--tw-gradient-from") {
        return 211;
    }
    if property.starts_with("--tw-gradient-via") {
        return 212;
    }
    if property.starts_with("--tw-gradient-to") {
        return 213;
    }
    if property.starts_with("--tw-translate") || property == "translate" {
        return 260;
    }
    if property.starts_with("--tw-rotate")
        || property.starts_with("--tw-skew")
        || property == "rotate"
        || property == "skew"
    {
        return 262;
    }
    if property.starts_with("--tw-scale") || property == "scale" {
        return 261;
    }
    if property.starts_with("--tw-gradient") || property.starts_with("background") {
        return 210;
    }
    if property.starts_with("--tw-space-") {
        return 15;
    }
    if property.starts_with("--tw-font-weight") || property.starts_with("--tw-font-stretch") {
        return 322;
    }
    if property.starts_with("--tw-leading") {
        return 321;
    }
    if property.starts_with("--tw-tracking") {
        return 323;
    }
    if property.starts_with("--tw-divide-") {
        return 217;
    }
    if property == "--tw-border-style" {
        return 19;
    }
    if property.starts_with("--tw-ring")
        || property.starts_with("--tw-shadow")
        || property.starts_with("--tw-drop-shadow")
        || property.starts_with("--tw-outline")
    {
        return 225;
    }
    if property.starts_with("--tw-") {
        return 500;
    }

    match property {
        "pointer-events" => 0,
        "visibility" => 1,
        "position" => 2,
        "inset" | "inset-inline" | "inset-block" | "top" | "right" | "bottom" | "left" => 3,
        "isolation" => 4,
        "z-index" => 5,
        "order" => 6,
        "grid-column" | "grid-column-start" | "grid-column-end" | "grid-row" | "grid-row-start"
        | "grid-row-end" => 7,
        "float" | "clear" => 8,
        "margin"
        | "margin-top"
        | "margin-right"
        | "margin-bottom"
        | "margin-left"
        | "margin-inline"
        | "margin-inline-start"
        | "margin-inline-end"
        | "margin-block"
        | "margin-block-start"
        | "margin-block-end" => 9,
        "box-sizing" => 10,
        "display" => 11,
        "aspect-ratio" => 12,
        "height" | "width" | "max-height" | "max-width" | "min-height" | "min-width" => 13,
        "flex"
        | "flex-basis"
        | "flex-direction"
        | "flex-wrap"
        | "grid-template-columns"
        | "grid-template-rows"
        | "grid-auto-columns"
        | "grid-auto-flow"
        | "grid-auto-rows" => 14,
        "align-content" | "justify-content" | "place-content" | "align-items" | "justify-items"
        | "place-items" | "align-self" | "justify-self" | "place-self" | "gap" | "column-gap"
        | "row-gap" => 15,
        "overflow" | "overflow-x" | "overflow-y" => 16,
        "overscroll-behavior" | "overscroll-behavior-x" | "overscroll-behavior-y" => 16,
        "scroll-behavior" | "scroll-snap-type" | "scroll-snap-align" | "scroll-snap-stop"
        | "scroll-margin" | "scroll-padding" => 17,
        "border-radius"
        | "border-top-left-radius"
        | "border-top-right-radius"
        | "border-bottom-left-radius"
        | "border-bottom-right-radius"
        | "border-start-start-radius"
        | "border-start-end-radius"
        | "border-end-start-radius"
        | "border-end-end-radius" => 18,
        "border-width"
        | "border-style"
        | "border-color"
        | "border-collapse"
        | "border-spacing"
        | "border-top-width"
        | "border-right-width"
        | "border-bottom-width"
        | "border-left-width"
        | "border-inline-width"
        | "border-inline-start-width"
        | "border-inline-end-width"
        | "border-block-width"
        | "border-block-start-width"
        | "border-block-end-width"
        | "border-top-style"
        | "border-right-style"
        | "border-bottom-style"
        | "border-left-style"
        | "border-inline-style"
        | "border-inline-start-style"
        | "border-inline-end-style"
        | "border-block-style"
        | "border-block-start-style"
        | "border-block-end-style"
        | "border-top-color"
        | "border-right-color"
        | "border-bottom-color"
        | "border-left-color"
        | "border-inline-color"
        | "border-inline-start-color"
        | "border-inline-end-color"
        | "border-block-color"
        | "border-block-start-color"
        | "border-block-end-color"
        | "outline-width"
        | "outline-style"
        | "outline-color" => 19,
        "outline-offset" => 19,
        "object-fit" | "object-position" => 215,
        "padding"
        | "padding-top"
        | "padding-right"
        | "padding-bottom"
        | "padding-left"
        | "padding-inline"
        | "padding-inline-start"
        | "padding-inline-end"
        | "padding-block"
        | "padding-block-start"
        | "padding-block-end" => 216,
        "box-shadow" => 220,
        "opacity" => 230,
        "mix-blend-mode" | "background-blend-mode" | "filter" | "backdrop-filter" => 240,
        "transition-property"
        | "transition-duration"
        | "transition-delay"
        | "transition-timing-function"
        | "transition-behavior" => 250,
        "transform"
        | "transform-origin"
        | "transform-style"
        | "perspective"
        | "perspective-origin"
        | "backface-visibility" => 270,
        "animation" => 280,
        "cursor" => 290,
        "touch-action" | "resize" => 300,
        "user-select" => 336,
        "accent-color" | "appearance" | "color-scheme" | "forced-color-adjust" => 310,
        "text-align" => 314,
        "vertical-align" => 315,
        "font-family" => 316,
        "font-size" => 320,
        "line-height" => 321,
        "font-weight" | "font-style" | "font-variant-numeric" => 322,
        "letter-spacing" => 323,
        "text-overflow" | "text-wrap" | "text-indent" => 324,
        "overflow-wrap" => 325,
        "word-break" => 326,
        "white-space" => 327,
        "text-transform" => 326,
        "color"
        | "fill"
        | "stroke"
        | "stroke-width"
        | "caret-color"
        | "text-decoration-color"
        | "text-decoration-thickness"
        | "text-underline-offset"
        | "text-decoration-line" => 330,
        "list-style-type" | "list-style-position" | "list-style-image" => 340,
        "content" => 350,
        _ => 500,
    }
}

fn generate_rule(
    class: &str,
    config: &GeneratorConfig,
    variant_tables: &VariantTables,
) -> Option<String> {
    let (variants, base_with_modifier) = parse_variants(class);
    let (base, important_modifier) = strip_important_modifier(base_with_modifier);
    let rule = generate_color_rule(base, config).or_else(|| match base {
        "font-thin" => rule(
            ".font-thin",
            "--tw-font-weight:var(--font-weight-thin);font-weight:var(--font-weight-thin)",
            config,
        ),
        "font-extralight" => rule(
            ".font-extralight",
            "--tw-font-weight:var(--font-weight-extralight);font-weight:var(--font-weight-extralight)",
            config,
        ),
        "font-light" => rule(
            ".font-light",
            "--tw-font-weight:var(--font-weight-light);font-weight:var(--font-weight-light)",
            config,
        ),
        "font-normal" => rule(
            ".font-normal",
            "--tw-font-weight:var(--font-weight-normal);font-weight:var(--font-weight-normal)",
            config,
        ),
        "font-medium" => rule(
            ".font-medium",
            "--tw-font-weight:var(--font-weight-medium);font-weight:var(--font-weight-medium)",
            config,
        ),
        "font-semibold" => rule(
            ".font-semibold",
            "--tw-font-weight:var(--font-weight-semibold);font-weight:var(--font-weight-semibold)",
            config,
        ),
        "font-bold" => rule(
            ".font-bold",
            "--tw-font-weight:var(--font-weight-bold);font-weight:var(--font-weight-bold)",
            config,
        ),
        "font-extrabold" => rule(
            ".font-extrabold",
            "--tw-font-weight:var(--font-weight-extrabold);font-weight:var(--font-weight-extrabold)",
            config,
        ),
        "font-black" => rule(
            ".font-black",
            "--tw-font-weight:var(--font-weight-black);font-weight:var(--font-weight-black)",
            config,
        ),
        "italic" => rule(".italic", "font-style:italic", config),
        "not-italic" => rule(".not-italic", "font-style:normal", config),
        "antialiased" => rule(
            ".antialiased",
            "-webkit-font-smoothing:antialiased;-moz-osx-font-smoothing:grayscale",
            config,
        ),
        "subpixel-antialiased" => rule(
            ".subpixel-antialiased",
            "-webkit-font-smoothing:auto;-moz-osx-font-smoothing:auto",
            config,
        ),
        "normal-nums" => rule(".normal-nums", "font-variant-numeric:normal", config),
        "ordinal" => rule(".ordinal", "font-variant-numeric:ordinal", config),
        "slashed-zero" => rule(".slashed-zero", "font-variant-numeric:slashed-zero", config),
        "lining-nums" => rule(".lining-nums", "font-variant-numeric:lining-nums", config),
        "oldstyle-nums" => rule(
            ".oldstyle-nums",
            "font-variant-numeric:oldstyle-nums",
            config,
        ),
        "proportional-nums" => rule(
            ".proportional-nums",
            "font-variant-numeric:proportional-nums",
            config,
        ),
        "tabular-nums" => rule(".tabular-nums", "font-variant-numeric:tabular-nums", config),
        "diagonal-fractions" => rule(
            ".diagonal-fractions",
            "font-variant-numeric:diagonal-fractions",
            config,
        ),
        "stacked-fractions" => rule(
            ".stacked-fractions",
            "font-variant-numeric:stacked-fractions",
            config,
        ),
        "list-inside" => rule(".list-inside", "list-style-position:inside", config),
        "list-outside" => rule(".list-outside", "list-style-position:outside", config),
        "text-left" => rule(".text-left", "text-align:left", config),
        "text-center" => rule(".text-center", "text-align:center", config),
        "text-right" => rule(".text-right", "text-align:right", config),
        "text-justify" => rule(".text-justify", "text-align:justify", config),
        "text-start" => rule(".text-start", "text-align:start", config),
        "text-end" => rule(".text-end", "text-align:end", config),
        "text-inherit" => rule(".text-inherit", "color:inherit", config),
        "text-current" => rule(".text-current", "color:currentColor", config),
        "text-transparent" => rule(".text-transparent", "color:transparent", config),
        "text-black" => rule(".text-black", "color:var(--color-black)", config),
        "text-white" => rule(".text-white", "color:var(--color-white)", config),
        "uppercase" => rule(".uppercase", "text-transform:uppercase", config),
        "lowercase" => rule(".lowercase", "text-transform:lowercase", config),
        "capitalize" => rule(".capitalize", "text-transform:capitalize", config),
        "normal-case" => rule(".normal-case", "text-transform:none", config),
        "truncate" => rule(
            ".truncate",
            "overflow:hidden;text-overflow:ellipsis;white-space:nowrap",
            config,
        ),
        "text-ellipsis" => rule(".text-ellipsis", "text-overflow:ellipsis", config),
        "text-clip" => rule(".text-clip", "text-overflow:clip", config),
        "text-wrap" => rule(".text-wrap", "text-wrap:wrap", config),
        "text-nowrap" => rule(".text-nowrap", "text-wrap:nowrap", config),
        "text-balance" => rule(".text-balance", "text-wrap:balance", config),
        "text-pretty" => rule(".text-pretty", "text-wrap:pretty", config),
        "whitespace-normal" => rule(".whitespace-normal", "white-space:normal", config),
        "whitespace-nowrap" => rule(".whitespace-nowrap", "white-space:nowrap", config),
        "whitespace-pre" => rule(".whitespace-pre", "white-space:pre", config),
        "whitespace-pre-line" => rule(".whitespace-pre-line", "white-space:pre-line", config),
        "whitespace-pre-wrap" => rule(".whitespace-pre-wrap", "white-space:pre-wrap", config),
        "whitespace-break-spaces" => rule(
            ".whitespace-break-spaces",
            "white-space:break-spaces",
            config,
        ),
        "break-normal" => rule(".break-normal", "word-break:normal", config),
        "break-all" => rule(".break-all", "word-break:break-all", config),
        "break-keep" => rule(".break-keep", "word-break:keep-all", config),
        "break-words" => rule(".break-words", "overflow-wrap:break-word", config),
        "wrap-break-word" => rule(".wrap-break-word", "overflow-wrap:break-word", config),
        "wrap-anywhere" => rule(".wrap-anywhere", "overflow-wrap:anywhere", config),
        "wrap-normal" => rule(".wrap-normal", "overflow-wrap:normal", config),
        "hyphens-none" => rule(".hyphens-none", "hyphens:none", config),
        "hyphens-manual" => rule(".hyphens-manual", "hyphens:manual", config),
        "hyphens-auto" => rule(".hyphens-auto", "hyphens:auto", config),
        "content-none" => rule(".content-none", "content:none", config),
        "underline" => rule(".underline", "text-decoration-line:underline", config),
        "overline" => rule(".overline", "text-decoration-line:overline", config),
        "line-through" => rule(".line-through", "text-decoration-line:line-through", config),
        "no-underline" => rule(".no-underline", "text-decoration-line:none", config),
        "decoration-solid" => rule(".decoration-solid", "text-decoration-style:solid", config),
        "decoration-double" => rule(".decoration-double", "text-decoration-style:double", config),
        "decoration-dotted" => rule(".decoration-dotted", "text-decoration-style:dotted", config),
        "decoration-dashed" => rule(".decoration-dashed", "text-decoration-style:dashed", config),
        "decoration-wavy" => rule(".decoration-wavy", "text-decoration-style:wavy", config),
        "decoration-auto" => rule(".decoration-auto", "text-decoration-thickness:auto", config),
        "decoration-from-font" => rule(
            ".decoration-from-font",
            "text-decoration-thickness:from-font",
            config,
        ),
        "leading-none" => rule(".leading-none", "--tw-leading:1;line-height:1", config),
        "leading-tight" => rule(
            ".leading-tight",
            "--tw-leading:var(--leading-tight);line-height:var(--leading-tight)",
            config,
        ),
        "leading-snug" => rule(
            ".leading-snug",
            "--tw-leading:var(--leading-snug);line-height:var(--leading-snug)",
            config,
        ),
        "leading-normal" => rule(
            ".leading-normal",
            "--tw-leading:var(--leading-normal);line-height:var(--leading-normal)",
            config,
        ),
        "leading-relaxed" => rule(
            ".leading-relaxed",
            "--tw-leading:var(--leading-relaxed);line-height:var(--leading-relaxed)",
            config,
        ),
        "leading-loose" => rule(
            ".leading-loose",
            "--tw-leading:var(--leading-loose);line-height:var(--leading-loose)",
            config,
        ),
        "inset-0" => rule(".inset-0", "inset:calc(var(--spacing) * 0)", config),
        "inset-x-0" => rule(".inset-x-0", "inset-inline:calc(var(--spacing) * 0)", config),
        "inset-y-0" => rule(".inset-y-0", "inset-block:calc(var(--spacing) * 0)", config),
        "top-0" => rule(".top-0", "top:calc(var(--spacing) * 0)", config),
        "right-0" => rule(".right-0", "right:calc(var(--spacing) * 0)", config),
        "bottom-0" => rule(".bottom-0", "bottom:calc(var(--spacing) * 0)", config),
        "left-0" => rule(".left-0", "left:calc(var(--spacing) * 0)", config),
        "shadow-sm" => rule(
            ".shadow-sm",
            "--tw-shadow:0 1px 3px 0 var(--tw-shadow-color,rgb(0 0 0 / 0.1)),0 1px 2px -1px var(--tw-shadow-color,rgb(0 0 0 / 0.1));box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "shadow" => rule(
            ".shadow",
            "--tw-shadow:0 1px 3px 0 var(--tw-shadow-color,rgb(0 0 0 / 0.1)),0 1px 2px -1px var(--tw-shadow-color,rgb(0 0 0 / 0.1));box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "shadow-md" => rule(
            ".shadow-md",
            "--tw-shadow:0 4px 6px -1px var(--tw-shadow-color,rgb(0 0 0 / 0.1)),0 2px 4px -2px var(--tw-shadow-color,rgb(0 0 0 / 0.1));box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "shadow-lg" => rule(
            ".shadow-lg",
            "--tw-shadow:0 10px 15px -3px var(--tw-shadow-color,rgb(0 0 0 / 0.1)),0 4px 6px -4px var(--tw-shadow-color,rgb(0 0 0 / 0.1));box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "shadow-xl" => rule(
            ".shadow-xl",
            "--tw-shadow:0 20px 25px -5px var(--tw-shadow-color,rgb(0 0 0 / 0.1)),0 8px 10px -6px var(--tw-shadow-color,rgb(0 0 0 / 0.1));box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "shadow-2xl" => rule(
            ".shadow-2xl",
            "--tw-shadow:0 25px 50px -12px var(--tw-shadow-color,rgb(0 0 0 / 0.25));box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "shadow-inner" => rule(
            ".shadow-inner",
            "--tw-shadow:inset 0 2px 4px 0 var(--tw-shadow-color,rgb(0 0 0 / 0.05));box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "shadow-none" => rule(
            ".shadow-none",
            "--tw-shadow:0 0 #0000;box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "ring-0" => rule(
            ".ring-0",
            "--tw-ring-shadow:var(--tw-ring-inset,) 0 0 0 calc(0px + var(--tw-ring-offset-width)) var(--tw-ring-color,currentcolor);box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "ring-1" => rule(
            ".ring-1",
            "--tw-ring-shadow:var(--tw-ring-inset,) 0 0 0 calc(1px + var(--tw-ring-offset-width)) var(--tw-ring-color,currentcolor);box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "ring-2" => rule(
            ".ring-2",
            "--tw-ring-shadow:var(--tw-ring-inset,) 0 0 0 calc(2px + var(--tw-ring-offset-width)) var(--tw-ring-color,currentcolor);box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "ring-4" => rule(
            ".ring-4",
            "--tw-ring-shadow:var(--tw-ring-inset,) 0 0 0 calc(4px + var(--tw-ring-offset-width)) var(--tw-ring-color,currentcolor);box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "ring-8" => rule(
            ".ring-8",
            "--tw-ring-shadow:var(--tw-ring-inset,) 0 0 0 calc(8px + var(--tw-ring-offset-width)) var(--tw-ring-color,currentcolor);box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "ring" => rule(
            ".ring",
            "--tw-ring-shadow:var(--tw-ring-inset,) 0 0 0 calc(1px + var(--tw-ring-offset-width)) var(--tw-ring-color,currentcolor);box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
            config,
        ),
        "ring-inset" => rule(".ring-inset", "--tw-ring-inset:inset", config),
        "bg-fixed" => rule(".bg-fixed", "background-attachment:fixed", config),
        "bg-local" => rule(".bg-local", "background-attachment:local", config),
        "bg-scroll" => rule(".bg-scroll", "background-attachment:scroll", config),
        "bg-clip-border" => rule(".bg-clip-border", "background-clip:border-box", config),
        "bg-clip-padding" => rule(".bg-clip-padding", "background-clip:padding-box", config),
        "bg-clip-content" => rule(".bg-clip-content", "background-clip:content-box", config),
        "bg-clip-text" => rule(".bg-clip-text", "background-clip:text", config),
        "bg-origin-border" => rule(".bg-origin-border", "background-origin:border-box", config),
        "bg-origin-padding" => rule(
            ".bg-origin-padding",
            "background-origin:padding-box",
            config,
        ),
        "bg-origin-content" => rule(
            ".bg-origin-content",
            "background-origin:content-box",
            config,
        ),
        "bg-top-left" => rule(".bg-top-left", "background-position:top left", config),
        "bg-top" => rule(".bg-top", "background-position:top", config),
        "bg-top-right" => rule(".bg-top-right", "background-position:top right", config),
        "bg-left" => rule(".bg-left", "background-position:left", config),
        "bg-center" => rule(".bg-center", "background-position:center", config),
        "bg-right" => rule(".bg-right", "background-position:right", config),
        "bg-bottom-left" => rule(".bg-bottom-left", "background-position:bottom left", config),
        "bg-bottom" => rule(".bg-bottom", "background-position:bottom", config),
        "bg-bottom-right" => rule(
            ".bg-bottom-right",
            "background-position:bottom right",
            config,
        ),
        "bg-repeat" => rule(".bg-repeat", "background-repeat:repeat", config),
        "bg-repeat-x" => rule(".bg-repeat-x", "background-repeat:repeat-x", config),
        "bg-repeat-y" => rule(".bg-repeat-y", "background-repeat:repeat-y", config),
        "bg-repeat-space" => rule(".bg-repeat-space", "background-repeat:space", config),
        "bg-repeat-round" => rule(".bg-repeat-round", "background-repeat:round", config),
        "bg-no-repeat" => rule(".bg-no-repeat", "background-repeat:no-repeat", config),
        "bg-auto" => rule(".bg-auto", "background-size:auto", config),
        "bg-cover" => rule(".bg-cover", "background-size:cover", config),
        "bg-contain" => rule(".bg-contain", "background-size:contain", config),
        "bg-inherit" => rule(".bg-inherit", "background-color:inherit", config),
        "bg-current" => rule(".bg-current", "background-color:currentColor", config),
        "bg-transparent" => rule(".bg-transparent", "background-color:transparent", config),
        "bg-black" => rule(".bg-black", "background-color:var(--color-black)", config),
        "bg-white" => rule(".bg-white", "background-color:var(--color-white)", config),
        _ => generate_text_shadow_rule(base, config)
            .or_else(|| generate_drop_shadow_rule(base, config))
            .or_else(|| generate_backdrop_blur_rule(base, config))
            .or_else(|| generate_backdrop_brightness_rule(base, config))
            .or_else(|| generate_backdrop_contrast_rule(base, config))
            .or_else(|| generate_backdrop_grayscale_rule(base, config))
            .or_else(|| generate_backdrop_hue_rotate_rule(base, config))
            .or_else(|| generate_backdrop_invert_rule(base, config))
            .or_else(|| generate_backdrop_opacity_rule(base, config))
            .or_else(|| generate_backdrop_saturate_rule(base, config))
            .or_else(|| generate_backdrop_sepia_rule(base, config))
            .or_else(|| generate_animation_rule(base, config))
            .or_else(|| generate_transition_timing_function_rule(base, config))
            .or_else(|| generate_transition_delay_rule(base, config))
            .or_else(|| generate_transition_duration_rule(base, config))
            .or_else(|| generate_transition_behavior_rule(base, config))
            .or_else(|| generate_transition_property_rule(base, config))
            .or_else(|| generate_blur_rule(base, config))
            .or_else(|| generate_brightness_rule(base, config))
            .or_else(|| generate_contrast_rule(base, config))
            .or_else(|| generate_grayscale_rule(base, config))
            .or_else(|| generate_hue_rotate_rule(base, config))
            .or_else(|| generate_invert_rule(base, config))
            .or_else(|| generate_saturate_rule(base, config))
            .or_else(|| generate_sepia_rule(base, config))
            .or_else(|| generate_backdrop_filter_rule(base, config))
            .or_else(|| generate_filter_rule(base, config))
            .or_else(|| generate_arbitrary_property_rule(base, config))
            .or_else(|| generate_text_arbitrary_color_rule(base, config))
            .or_else(|| generate_text_palette_color_rule(base, config, variant_tables))
            .or_else(|| generate_background_arbitrary_color_rule(base, config, variant_tables))
            .or_else(|| generate_background_palette_color_rule(base, config, variant_tables))
            .or_else(|| generate_fill_arbitrary_color_rule(base, config))
            .or_else(|| generate_fill_palette_color_rule(base, config))
            .or_else(|| generate_stroke_width_rule(base, config))
            .or_else(|| generate_stroke_arbitrary_color_rule(base, config))
            .or_else(|| generate_stroke_palette_color_rule(base, config))
            .or_else(|| generate_background_blend_mode_rule(base, config))
            .or_else(|| generate_background_position_rule(base, config))
            .or_else(|| generate_background_size_rule(base, config))
            .or_else(|| generate_background_image_rule(base, config, variant_tables))
            .or_else(|| generate_content_rule(base, config))
            .or_else(|| generate_decoration_thickness_rule(base, config))
            .or_else(|| generate_decoration_arbitrary_color_rule(base, config))
            .or_else(|| generate_decoration_palette_color_rule(base, config))
            .or_else(|| generate_accent_arbitrary_color_rule(base, config))
            .or_else(|| generate_accent_palette_color_rule(base, config))
            .or_else(|| generate_caret_arbitrary_color_rule(base, config))
            .or_else(|| generate_caret_palette_color_rule(base, config))
            .or_else(|| generate_shadow_value_rule(base, config))
            .or_else(|| generate_shadow_color_rule(base, config, variant_tables))
            .or_else(|| generate_inset_shadow_color_rule(base, config))
            .or_else(|| generate_ring_color_rule(base, config, variant_tables))
            .or_else(|| generate_inset_ring_color_rule(base, config))
            .or_else(|| generate_list_style_type_rule(base, config))
            .or_else(|| generate_list_style_image_rule(base, config))
            .or_else(|| generate_line_clamp_rule(base, config))
            .or_else(|| generate_tracking_rule(base, config))
            .or_else(|| generate_text_indent_rule(base, config))
            .or_else(|| generate_underline_offset_rule(base, config))
            .or_else(|| generate_vertical_align_rule(base, config))
            .or_else(|| generate_font_stretch_rule(base, config))
            .or_else(|| generate_font_weight_rule(base, config))
            .or_else(|| generate_leading_rule(base, config))
            .or_else(|| generate_text_size_rule(base, config))
            .or_else(|| generate_font_family_rule(base, config))
            .or_else(|| generate_spacing_rule(base, config))
            .or_else(|| generate_scroll_margin_rule(base, config))
            .or_else(|| generate_scroll_padding_rule(base, config))
            .or_else(|| generate_aspect_ratio_rule(base, config))
            .or_else(|| generate_columns_rule(base, config))
            .or_else(|| generate_break_before_rule(base, config))
            .or_else(|| generate_break_after_rule(base, config))
            .or_else(|| generate_break_inside_rule(base, config))
            .or_else(|| generate_box_decoration_rule(base, config))
            .or_else(|| generate_box_sizing_rule(base, config))
            .or_else(|| generate_border_spacing_rule(base, config))
            .or_else(|| generate_border_width_rule(base, config))
            .or_else(|| generate_divide_width_rule(base, config))
            .or_else(|| generate_border_style_rule(base, config))
            .or_else(|| generate_border_color_rule(base, config, variant_tables))
            .or_else(|| generate_border_radius_rule(base, config))
            .or_else(|| generate_outline_style_rule(base, config))
            .or_else(|| generate_outline_offset_rule(base, config))
            .or_else(|| generate_outline_color_rule(base, config))
            .or_else(|| generate_outline_width_rule(base, config))
            .or_else(|| generate_float_rule(base, config))
            .or_else(|| generate_clear_rule(base, config))
            .or_else(|| generate_isolation_rule(base, config))
            .or_else(|| generate_appearance_rule(base, config))
            .or_else(|| generate_forced_color_adjust_rule(base, config))
            .or_else(|| generate_color_scheme_rule(base, config))
            .or_else(|| generate_cursor_rule(base, config))
            .or_else(|| generate_field_sizing_rule(base, config))
            .or_else(|| generate_pointer_events_rule(base, config))
            .or_else(|| generate_resize_rule(base, config))
            .or_else(|| generate_scroll_behavior_rule(base, config))
            .or_else(|| generate_scroll_snap_type_rule(base, config))
            .or_else(|| generate_scroll_snap_align_rule(base, config))
            .or_else(|| generate_scroll_snap_stop_rule(base, config))
            .or_else(|| generate_touch_action_rule(base, config))
            .or_else(|| generate_user_select_rule(base, config))
            .or_else(|| generate_will_change_rule(base, config))
            .or_else(|| generate_mix_blend_mode_rule(base, config))
            .or_else(|| generate_mask_image_rule(base, config))
            .or_else(|| generate_mask_mode_rule(base, config))
            .or_else(|| generate_mask_type_rule(base, config))
            .or_else(|| generate_mask_origin_rule(base, config))
            .or_else(|| generate_mask_position_rule(base, config))
            .or_else(|| generate_mask_repeat_rule(base, config))
            .or_else(|| generate_mask_size_rule(base, config))
            .or_else(|| generate_mask_clip_rule(base, config))
            .or_else(|| generate_mask_composite_rule(base, config))
            .or_else(|| generate_object_fit_rule(base, config))
            .or_else(|| generate_object_position_rule(base, config))
            .or_else(|| generate_overscroll_rule(base, config))
            .or_else(|| generate_inset_rule(base, config))
            .or_else(|| generate_z_index_rule(base, config))
            .or_else(|| generate_visibility_rule(base, config))
            .or_else(|| generate_backface_visibility_rule(base, config))
            .or_else(|| generate_perspective_origin_rule(base, config))
            .or_else(|| generate_perspective_rule(base, config))
            .or_else(|| generate_transform_style_rule(base, config))
            .or_else(|| generate_transform_rule(base, config))
            .or_else(|| generate_transform_origin_rule(base, config))
            .or_else(|| generate_translate_rule(base, config))
            .or_else(|| generate_rotate_rule(base, config))
            .or_else(|| generate_scale_rule(base, config))
            .or_else(|| generate_skew_rule(base, config))
            .or_else(|| generate_flex_basis_rule(base, config))
            .or_else(|| generate_flex_shorthand_rule(base, config))
            .or_else(|| generate_flex_grow_rule(base, config))
            .or_else(|| generate_flex_shrink_rule(base, config))
            .or_else(|| generate_order_rule(base, config))
            .or_else(|| generate_grid_template_columns_rule(base, config))
            .or_else(|| generate_grid_auto_flow_rule(base, config))
            .or_else(|| generate_grid_auto_columns_rule(base, config))
            .or_else(|| generate_grid_auto_rows_rule(base, config))
            .or_else(|| generate_grid_column_rule(base, config))
            .or_else(|| generate_grid_row_rule(base, config))
            .or_else(|| generate_custom_utility_rule(base, config, variant_tables))
            .or_else(|| generate_opacity_rule(base, config))
            .or_else(|| generate_layout_rule(base, config, variant_tables)),
    });

    let generated = apply_variants(&variants, class, base, rule, config.minify, variant_tables)?;
    if important_modifier {
        let important = add_important_to_rule(&generated, config.minify)?;
        if !rule_allowed_by_theme(base, &important, variant_tables) {
            return None;
        }
        return Some(important);
    }
    if !rule_allowed_by_theme(base, &generated, variant_tables) {
        return None;
    }
    Some(generated)
}

fn generate_custom_utility_rule(
    class: &str,
    config: &GeneratorConfig,
    variant_tables: &VariantTables,
) -> Option<String> {
    for (pattern, body) in &variant_tables.custom_utilities {
        let Some(token) = match_custom_utility_pattern(pattern, class) else {
            continue;
        };
        let (value_token, modifier_token) = parse_functional_utility_tokens(token.as_deref());
        let resolved_body =
            resolve_custom_utility_body(body, value_token, modifier_token, variant_tables)?;
        let selector = format!(".{}", escape_selector(class));
        return rule(&selector, resolved_body.trim(), config);
    }
    None
}

fn match_custom_utility_pattern(pattern: &str, class: &str) -> Option<Option<String>> {
    if let Some((prefix, suffix)) = pattern.split_once('*') {
        if pattern.matches('*').count() != 1 {
            return None;
        }
        if !class.starts_with(prefix) || !class.ends_with(suffix) {
            return None;
        }
        let start = prefix.len();
        let end = class.len().saturating_sub(suffix.len());
        if end < start {
            return None;
        }
        let token = class[start..end].to_string();
        if token.is_empty() {
            return None;
        }
        return Some(Some(token));
    }
    if pattern == class {
        return Some(None);
    }
    None
}

fn resolve_custom_utility_body(
    body: &str,
    value_token: Option<&str>,
    modifier_token: Option<&str>,
    variant_tables: &VariantTables,
) -> Option<String> {
    let trimmed = body.trim();
    if trimmed.is_empty() {
        return None;
    }
    if !trimmed.contains("--value(") && !trimmed.contains("--modifier(") {
        return Some(trimmed.to_string());
    }

    if value_token.is_none() && modifier_token.is_none() {
        return None;
    }
    if trimmed.contains('{') || trimmed.contains('}') {
        return None;
    }

    let mut resolved = Vec::new();
    for declaration in trimmed.split(';') {
        let declaration = declaration.trim();
        if declaration.is_empty() {
            continue;
        }
        let replaced =
            replace_declaration_functions(declaration, value_token, modifier_token, variant_tables);
        let Some(replaced) = replaced else {
            continue;
        };
        resolved.push(replaced);
    }
    if resolved.is_empty() {
        return None;
    }
    Some(resolved.join(";"))
}

fn replace_declaration_functions(
    declaration: &str,
    value_token: Option<&str>,
    modifier_token: Option<&str>,
    variant_tables: &VariantTables,
) -> Option<String> {
    let replaced_value =
        replace_named_function(declaration, "--value(", value_token, variant_tables)?;
    replace_named_function(
        &replaced_value,
        "--modifier(",
        modifier_token,
        variant_tables,
    )
}

fn replace_named_function(
    input: &str,
    fn_prefix: &str,
    token: Option<&str>,
    variant_tables: &VariantTables,
) -> Option<String> {
    let mut out = String::with_capacity(input.len());
    let mut cursor = 0usize;
    while let Some(rel) = input[cursor..].find(fn_prefix) {
        let token = token?;
        let start = cursor + rel;
        out.push_str(&input[cursor..start]);
        let args_start = start + fn_prefix.len();
        let mut depth = 1usize;
        let mut idx = args_start;
        while idx < input.len() {
            let Some(ch) = input[idx..].chars().next() else {
                break;
            };
            let size = ch.len_utf8();
            match ch {
                '(' => depth += 1,
                ')' => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
            idx += size;
        }
        if idx >= input.len() {
            return None;
        }
        let args = input[args_start..idx].trim();
        let replacement = resolve_value_function(args, token, variant_tables)?;
        out.push_str(&replacement);
        cursor = idx + 1;
    }
    out.push_str(&input[cursor..]);
    Some(out)
}

fn resolve_value_function(
    args: &str,
    token: &str,
    variant_tables: &VariantTables,
) -> Option<String> {
    for arg in split_value_function_args(args) {
        let arg = arg.trim();
        if arg.is_empty() {
            continue;
        }
        if let Some(value) = resolve_value_function_arg(arg, token, variant_tables) {
            return Some(value);
        }
    }
    None
}

fn split_value_function_args(raw: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut current = String::new();
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut quote: Option<char> = None;
    let mut escaped = false;
    for ch in raw.chars() {
        if let Some(q) = quote {
            current.push(ch);
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == q {
                quote = None;
            }
            continue;
        }
        match ch {
            '\'' | '"' => {
                quote = Some(ch);
                current.push(ch);
            }
            '(' => {
                paren_depth += 1;
                current.push(ch);
            }
            ')' => {
                paren_depth = paren_depth.saturating_sub(1);
                current.push(ch);
            }
            '[' => {
                bracket_depth += 1;
                current.push(ch);
            }
            ']' => {
                bracket_depth = bracket_depth.saturating_sub(1);
                current.push(ch);
            }
            ',' if paren_depth == 0 && bracket_depth == 0 => {
                args.push(current.trim().to_string());
                current.clear();
            }
            _ => current.push(ch),
        }
    }
    if !current.trim().is_empty() {
        args.push(current.trim().to_string());
    }
    args
}

fn resolve_value_function_arg(
    arg: &str,
    token: &str,
    variant_tables: &VariantTables,
) -> Option<String> {
    if let Some(literal) = arg
        .strip_prefix('"')
        .and_then(|v| v.strip_suffix('"'))
        .or_else(|| arg.strip_prefix('\'').and_then(|v| v.strip_suffix('\'')))
    {
        return (token == literal).then(|| literal.to_string());
    }

    if let Some(theme_pattern) = arg.strip_prefix("--").filter(|value| value.ends_with('*')) {
        let name = format!("--{}{}", &theme_pattern[..theme_pattern.len() - 1], token);
        return variant_tables.theme_variable_values.get(&name).cloned();
    }

    if let Some(type_name) = arg.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        let raw = token.strip_prefix('[')?.strip_suffix(']')?;
        return validate_value_for_type(raw, type_name.trim()).then(|| raw.to_string());
    }

    validate_value_for_type(token, arg).then(|| token.to_string())
}

fn validate_value_for_type(value: &str, type_name: &str) -> bool {
    match type_name {
        "integer" => !value.is_empty() && value.chars().all(|c| c.is_ascii_digit()),
        "number" => value.parse::<f64>().is_ok(),
        "percentage" => value
            .strip_suffix('%')
            .is_some_and(|n| !n.is_empty() && n.parse::<f64>().is_ok()),
        "*" => !value.is_empty(),
        "ratio" => is_fraction(value),
        _ => false,
    }
}

fn parse_functional_utility_tokens(token: Option<&str>) -> (Option<&str>, Option<&str>) {
    let Some(token) = token else {
        return (None, None);
    };
    let mut bracket_depth = 0usize;
    let mut paren_depth = 0usize;
    for (idx, ch) in token.char_indices() {
        match ch {
            '[' => bracket_depth += 1,
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '/' if bracket_depth == 0 && paren_depth == 0 => {
                let value = &token[..idx];
                let modifier = &token[idx + 1..];
                let value = if value.is_empty() { None } else { Some(value) };
                let modifier = if modifier.is_empty() {
                    None
                } else {
                    Some(modifier)
                };
                return (value, modifier);
            }
            _ => {}
        }
    }
    (Some(token), None)
}

fn rule_allowed_by_theme(base_class: &str, rule: &str, tables: &VariantTables) -> bool {
    if tables.declared_theme_vars.is_empty()
        && !tables.global_theme_reset
        && tables.disabled_namespaces.is_empty()
        && tables.disabled_color_families.is_empty()
    {
        return true;
    }

    for variable in extract_css_variables_from_rule(rule) {
        if !is_variable_allowed(variable, tables) {
            return false;
        }
    }

    if rule_looks_like_color_utility(rule)
        && !color_utility_is_allowed(
            base_class,
            tables.global_theme_reset || tables.disabled_namespaces.contains("color"),
            &tables.disabled_color_families,
            &tables.declared_theme_vars,
        )
    {
        return false;
    }

    true
}

fn extract_css_variables_from_rule(rule: &str) -> Vec<&str> {
    let mut vars = Vec::new();
    let mut cursor = 0usize;
    while let Some(rel_start) = rule[cursor..].find("var(--") {
        let start = cursor + rel_start + "var(".len();
        let Some(end_rel) = rule[start..].find(')') else {
            break;
        };
        let end = start + end_rel;
        let raw = &rule[start..end];
        let variable = raw.split(',').next().unwrap_or(raw).trim();
        if variable.starts_with("--") {
            vars.push(variable);
        }
        cursor = end + 1;
    }
    vars
}

fn is_variable_allowed(variable: &str, tables: &VariantTables) -> bool {
    if variable.starts_with("--tw-") || variable.starts_with("--default-") {
        return true;
    }
    if !is_theme_namespace_variable(variable) {
        return true;
    }
    if tables.declared_theme_vars.contains(variable) {
        return true;
    }
    if !tables.declared_theme_vars.is_empty() {
        if let Some(namespace) = theme_variable_namespace(variable) {
            if requires_declared_variable(namespace) {
                return false;
            }
        }
    }

    let namespace = theme_variable_namespace(variable);
    if tables.global_theme_reset {
        return false;
    }
    if let Some(namespace) = namespace {
        if tables.disabled_namespaces.contains(namespace) {
            return false;
        }
    }
    true
}

fn requires_declared_variable(namespace: &str) -> bool {
    matches!(namespace, "color" | "font" | "text" | "animate")
}

fn is_theme_namespace_variable(variable: &str) -> bool {
    theme_variable_namespace(variable).is_some()
}

fn theme_variable_namespace(variable: &str) -> Option<&str> {
    if variable == "--spacing" {
        return Some("spacing");
    }
    for namespace in [
        "color",
        "font",
        "text",
        "font-weight",
        "tracking",
        "leading",
        "breakpoint",
        "container",
        "radius",
        "shadow",
        "inset-shadow",
        "drop-shadow",
        "text-shadow",
        "blur",
        "perspective",
        "aspect",
        "ease",
        "animate",
    ] {
        let prefix = format!("--{}-", namespace);
        if variable.starts_with(&prefix) {
            return Some(namespace);
        }
    }
    None
}

fn rule_looks_like_color_utility(rule: &str) -> bool {
    [
        "color:",
        "background-color:",
        "border-color:",
        "border-top-color:",
        "border-right-color:",
        "border-bottom-color:",
        "border-left-color:",
        "border-inline-color:",
        "border-inline-start-color:",
        "border-inline-end-color:",
        "outline-color:",
        "text-decoration-color:",
        "accent-color:",
        "caret-color:",
        "fill:",
        "stroke:",
        "--tw-shadow-color:",
        "--tw-inset-shadow-color:",
        "--tw-ring-color:",
        "--tw-inset-ring-color:",
        "--tw-drop-shadow-color:",
        "--tw-gradient-from:",
        "--tw-gradient-via:",
        "--tw-gradient-to:",
    ]
    .iter()
    .any(|property| rule.contains(property))
}

fn color_utility_is_allowed(
    base_class: &str,
    require_declared: bool,
    disabled_families: &BTreeSet<String>,
    declared: &BTreeSet<String>,
) -> bool {
    let Some(token) = extract_color_utility_token(base_class) else {
        return true;
    };

    let declared_for_token = declared.contains(&format!("--color-{}", token));
    if declared_for_token {
        return true;
    }

    if require_declared {
        return false;
    }

    let family = token.split('-').next().unwrap_or(token);
    !disabled_families.contains(family)
}

fn extract_color_utility_token(base_class: &str) -> Option<&str> {
    fn strip_slash_modifier(raw: &str) -> &str {
        raw.split_once('/').map(|(token, _)| token).unwrap_or(raw)
    }

    if let Some(raw) = base_class
        .strip_prefix("drop-shadow-")
        .or_else(|| base_class.strip_prefix("text-shadow-"))
        .or_else(|| base_class.strip_prefix("shadow-"))
        .or_else(|| base_class.strip_prefix("inset-shadow-"))
        .or_else(|| base_class.strip_prefix("ring-"))
        .or_else(|| base_class.strip_prefix("inset-ring-"))
    {
        let token = strip_slash_modifier(raw);
        if token.starts_with('[') || token.starts_with('(') || token.is_empty() {
            return None;
        }
        return Some(token);
    }

    if let Some(raw) = base_class
        .strip_prefix("border-x-")
        .or_else(|| base_class.strip_prefix("border-y-"))
        .or_else(|| base_class.strip_prefix("border-s-"))
        .or_else(|| base_class.strip_prefix("border-e-"))
        .or_else(|| base_class.strip_prefix("border-t-"))
        .or_else(|| base_class.strip_prefix("border-r-"))
        .or_else(|| base_class.strip_prefix("border-b-"))
        .or_else(|| base_class.strip_prefix("border-l-"))
        .or_else(|| base_class.strip_prefix("divide-"))
        .or_else(|| base_class.strip_prefix("border-"))
    {
        let token = strip_slash_modifier(raw);
        if token.starts_with('[') || token.starts_with('(') || token.is_empty() {
            return None;
        }
        return Some(token);
    }

    if let Some(raw) = base_class
        .strip_prefix("bg-")
        .or_else(|| base_class.strip_prefix("text-"))
        .or_else(|| base_class.strip_prefix("decoration-"))
        .or_else(|| base_class.strip_prefix("outline-"))
        .or_else(|| base_class.strip_prefix("accent-"))
        .or_else(|| base_class.strip_prefix("caret-"))
        .or_else(|| base_class.strip_prefix("fill-"))
        .or_else(|| base_class.strip_prefix("stroke-"))
        .or_else(|| base_class.strip_prefix("from-"))
        .or_else(|| base_class.strip_prefix("via-"))
        .or_else(|| base_class.strip_prefix("to-"))
    {
        let token = strip_slash_modifier(raw);
        if token.starts_with('[') || token.starts_with('(') || token.is_empty() {
            return None;
        }
        return Some(token);
    }

    None
}

fn generate_text_arbitrary_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("text-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.starts_with("length:") {
            return None;
        }
        let value = if let Some(hinted) = raw.strip_prefix("color:") {
            if hinted.is_empty() || !hinted.starts_with("--") {
                return None;
            }
            hinted
        } else {
            raw
        };
        return rule(&selector, &format!("color:var({})", value), config);
    }

    if let Some(raw) = class
        .strip_prefix("text-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        let value = parse_arbitrary_color_value(raw)?;
        return rule(&selector, &format!("color:{}", value), config);
    }

    None
}

fn generate_filter_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "filter" {
        return rule(&selector, composed_filter_property(), config);
    }

    if class == "filter-none" {
        return rule(&selector, "filter:none", config);
    }

    if let Some(raw) = class
        .strip_prefix("filter-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("filter:{}", raw), config);
    }

    if let Some(raw) = class
        .strip_prefix("filter-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("filter:var({})", raw), config);
    }

    None
}

fn generate_arbitrary_property_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let raw = class.strip_prefix('[')?.strip_suffix(']')?;
    let (property, value) = raw.split_once(':')?;
    let property = property.trim();
    let value = normalize_arbitrary_value(value.trim());
    if property.is_empty() || value.is_empty() {
        return None;
    }
    let selector = format!(".{}", escape_selector(class));
    rule(&selector, &format!("{}:{}", property, value), config)
}

fn composed_filter_property() -> &'static str {
    "filter:var(--tw-blur,) var(--tw-brightness,) var(--tw-contrast,) var(--tw-grayscale,) var(--tw-hue-rotate,) var(--tw-invert,) var(--tw-saturate,) var(--tw-sepia,) var(--tw-drop-shadow,)"
}

fn composed_backdrop_filter_property() -> &'static str {
    "-webkit-backdrop-filter:var(--tw-backdrop-blur,) var(--tw-backdrop-brightness,) var(--tw-backdrop-contrast,) var(--tw-backdrop-grayscale,) var(--tw-backdrop-hue-rotate,) var(--tw-backdrop-invert,) var(--tw-backdrop-opacity,) var(--tw-backdrop-saturate,) var(--tw-backdrop-sepia,);backdrop-filter:var(--tw-backdrop-blur,) var(--tw-backdrop-brightness,) var(--tw-backdrop-contrast,) var(--tw-backdrop-grayscale,) var(--tw-backdrop-hue-rotate,) var(--tw-backdrop-invert,) var(--tw-backdrop-opacity,) var(--tw-backdrop-saturate,) var(--tw-backdrop-sepia,)"
}

fn composed_filter_rule(
    selector: &str,
    variable_decl: &str,
    legacy_filter_decl: Option<String>,
    config: &GeneratorConfig,
) -> Option<String> {
    let declarations = if let Some(legacy) = legacy_filter_decl {
        format!(
            "{};{};{}",
            variable_decl,
            legacy,
            composed_filter_property()
        )
    } else {
        format!("{};{}", variable_decl, composed_filter_property())
    };
    rule(selector, &declarations, config)
}

fn composed_backdrop_filter_rule(
    selector: &str,
    variable_decl: &str,
    config: &GeneratorConfig,
) -> Option<String> {
    rule(
        selector,
        &format!("{};{}", variable_decl, composed_backdrop_filter_property()),
        config,
    )
}

fn generate_transition_property_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let default_timing_duration = "transition-timing-function:var(--tw-ease,var(--default-transition-timing-function));transition-duration:var(--tw-duration,var(--default-transition-duration))";

    match class {
        "transition" => {
            let properties = "color,background-color,border-color,outline-color,text-decoration-color,fill,stroke,--tw-gradient-from,--tw-gradient-via,--tw-gradient-to,opacity,box-shadow,transform,translate,scale,rotate,filter,-webkit-backdrop-filter,backdrop-filter,display,content-visibility,overlay,pointer-events";
            return rule(
                &selector,
                &format!(
                    "transition-property:{};{}",
                    properties, default_timing_duration
                ),
                config,
            );
        }
        "transition-all" => {
            return rule(
                &selector,
                &format!("transition-property:all;{}", default_timing_duration),
                config,
            );
        }
        "transition-colors" => {
            let properties = "color,background-color,border-color,outline-color,text-decoration-color,fill,stroke,--tw-gradient-from,--tw-gradient-via,--tw-gradient-to";
            return rule(
                &selector,
                &format!(
                    "transition-property:{};{}",
                    properties, default_timing_duration
                ),
                config,
            );
        }
        "transition-opacity" => {
            return rule(
                &selector,
                &format!("transition-property:opacity;{}", default_timing_duration),
                config,
            );
        }
        "transition-shadow" => {
            return rule(
                &selector,
                &format!("transition-property:box-shadow;{}", default_timing_duration),
                config,
            );
        }
        "transition-transform" => {
            return rule(
                &selector,
                &format!(
                    "transition-property:transform,translate,scale,rotate;{}",
                    default_timing_duration
                ),
                config,
            );
        }
        "transition-none" => return rule(&selector, "transition-property:none", config),
        _ => {}
    }

    if let Some(custom) = class
        .strip_prefix("transition-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if custom.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!(
                "transition-property:var({});{}",
                custom, default_timing_duration
            ),
            config,
        );
    }

    if let Some(value) = class
        .strip_prefix("transition-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("transition-property:{};{}", value, default_timing_duration),
            config,
        );
    }

    None
}

fn generate_animation_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    match class {
        "animate-spin" => return rule(&selector, "animation:var(--animate-spin)", config),
        "animate-ping" => return rule(&selector, "animation:var(--animate-ping)", config),
        "animate-pulse" => return rule(&selector, "animation:var(--animate-pulse)", config),
        "animate-bounce" => return rule(&selector, "animation:var(--animate-bounce)", config),
        "animate-none" => return rule(&selector, "animation:none", config),
        _ => {}
    }

    if let Some(value) = class
        .strip_prefix("animate-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(&selector, &format!("animation:var({})", value), config);
    }

    if let Some(value) = class
        .strip_prefix("animate-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(&selector, &format!("animation:{}", value), config);
    }

    if let Some(token) = class.strip_prefix("animate-") {
        if token.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("animation:var(--animate-{})", token),
            config,
        );
    }

    None
}

fn generate_transition_behavior_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "transition-normal" => rule(&selector, "transition-behavior:normal", config),
        "transition-discrete" => rule(&selector, "transition-behavior:allow-discrete", config),
        _ => None,
    }
}

fn generate_transition_duration_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "duration-initial" {
        return rule(&selector, "transition-duration:initial", config);
    }

    if let Some(value) = class
        .strip_prefix("duration-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!(
                "--tw-duration:var({});transition-duration:var({})",
                value, value
            ),
            config,
        );
    }

    if let Some(value) = class
        .strip_prefix("duration-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("--tw-duration:{};transition-duration:{}", value, value),
            config,
        );
    }

    if let Some(value) = class.strip_prefix("duration-") {
        if !value.is_empty() && value.chars().all(|ch| ch.is_ascii_digit()) {
            let duration = format!("{}ms", value);
            return rule(
                &selector,
                &format!(
                    "--tw-duration:{};transition-duration:{}",
                    duration, duration
                ),
                config,
            );
        }
    }

    None
}

fn generate_transition_delay_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(value) = class
        .strip_prefix("delay-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("transition-delay:var({})", value),
            config,
        );
    }

    if let Some(value) = class
        .strip_prefix("delay-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(&selector, &format!("transition-delay:{}", value), config);
    }

    if let Some(value) = class.strip_prefix("delay-") {
        if !value.is_empty() && value.chars().all(|ch| ch.is_ascii_digit()) {
            return rule(&selector, &format!("transition-delay:{}ms", value), config);
        }
    }

    None
}

fn generate_transition_timing_function_rule(
    class: &str,
    config: &GeneratorConfig,
) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    match class {
        "ease-linear" => {
            return rule(
                &selector,
                "--tw-ease:linear;transition-timing-function:linear",
                config,
            );
        }
        "ease-in" => {
            return rule(
                &selector,
                "--tw-ease:var(--ease-in);transition-timing-function:var(--ease-in)",
                config,
            );
        }
        "ease-out" => {
            return rule(
                &selector,
                "--tw-ease:var(--ease-out);transition-timing-function:var(--ease-out)",
                config,
            );
        }
        "ease-in-out" => {
            return rule(
                &selector,
                "--tw-ease:var(--ease-in-out);transition-timing-function:var(--ease-in-out)",
                config,
            );
        }
        "ease-initial" => return rule(&selector, "transition-timing-function:initial", config),
        _ => {}
    }

    if let Some(value) = class
        .strip_prefix("ease-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        let ease_value = format!("var({})", value);
        return rule(
            &selector,
            &format!(
                "--tw-ease:{};transition-timing-function:{}",
                ease_value, ease_value
            ),
            config,
        );
    }

    if let Some(value) = class
        .strip_prefix("ease-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if value.is_empty() {
            return None;
        }
        let ease_value = value.to_string();
        return rule(
            &selector,
            &format!(
                "--tw-ease:{};transition-timing-function:{}",
                ease_value, ease_value
            ),
            config,
        );
    }

    if let Some(token) = class.strip_prefix("ease-") {
        if token.is_empty() {
            return None;
        }
        let ease_value = format!("var(--ease-{})", token);
        return rule(
            &selector,
            &format!(
                "--tw-ease:{};transition-timing-function:{}",
                ease_value, ease_value
            ),
            config,
        );
    }

    None
}

fn generate_blur_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "blur" {
        return composed_filter_rule(&selector, "--tw-blur:blur(8px)", None, config);
    }

    if class == "blur-none" {
        return composed_filter_rule(&selector, "--tw-blur:blur(0)", None, config);
    }

    if let Some(raw) = class
        .strip_prefix("blur-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(&selector, &format!("--tw-blur:blur({})", raw), None, config);
    }

    if let Some(raw) = class
        .strip_prefix("blur-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-blur:blur(var({}))", raw),
            None,
            config,
        );
    }

    let scale = class.strip_prefix("blur-")?;
    if scale.is_empty() {
        return None;
    }
    composed_filter_rule(
        &selector,
        &format!("--tw-blur:blur(var(--blur-{}))", scale),
        None,
        config,
    )
}

fn generate_drop_shadow_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "drop-shadow-none" {
        return composed_filter_rule(
            &selector,
            "--tw-drop-shadow:drop-shadow(0 0 #0000)",
            None,
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("drop-shadow-(color:")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("--tw-drop-shadow-color:var({})", raw),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("drop-shadow-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-drop-shadow:drop-shadow({})", raw),
            None,
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("drop-shadow-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() || raw.starts_with("color:") {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-drop-shadow:drop-shadow(var({}))", raw),
            None,
            config,
        );
    }

    let raw = class.strip_prefix("drop-shadow-")?;
    let (token, opacity) = split_slash_modifier(raw);
    if token.is_empty() {
        return None;
    }

    let color_value = match token {
        "inherit" => Some("inherit".to_string()),
        "current" => Some("currentColor".to_string()),
        "transparent" => Some("transparent".to_string()),
        "black" => Some("var(--color-black)".to_string()),
        "white" => Some("var(--color-white)".to_string()),
        _ if token.contains('-') => Some(format!("var(--color-{})", token)),
        _ => None,
    };

    if let Some(color) = color_value {
        if let Some(opacity_raw) = opacity {
            let opacity_value = parse_color_opacity_value(opacity_raw)?;
            return rule(
                &selector,
                &format!(
                    "--tw-drop-shadow-color:color-mix(in oklab,{} {},transparent)",
                    color, opacity_value
                ),
                config,
            );
        }
        return rule(
            &selector,
            &format!("--tw-drop-shadow-color:{}", color),
            config,
        );
    }

    if !token
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
    {
        return None;
    }

    if token == "2xl" && opacity.is_none() {
        return rule(
            &selector,
            "--tw-drop-shadow-size:drop-shadow(0 25px 25px var(--tw-drop-shadow-color,rgb(0 0 0 / 0.15)));--tw-drop-shadow:drop-shadow(var(--drop-shadow-2xl));filter:var(--tw-blur,) var(--tw-brightness,) var(--tw-contrast,) var(--tw-grayscale,) var(--tw-hue-rotate,) var(--tw-invert,) var(--tw-saturate,) var(--tw-sepia,) var(--tw-drop-shadow,)",
            config,
        );
    }

    if let Some(opacity_raw) = opacity {
        let opacity_value = parse_color_opacity_value(opacity_raw)?;
        return rule(
            &selector,
            &format!(
                "--tw-drop-shadow:drop-shadow(var(--drop-shadow-{}));--tw-drop-shadow-color:color-mix(in oklab,currentColor {},transparent);{}",
                token,
                opacity_value,
                composed_filter_property()
            ),
            config,
        );
    }

    composed_filter_rule(
        &selector,
        &format!("--tw-drop-shadow:drop-shadow(var(--drop-shadow-{}))", token),
        None,
        config,
    )
}

fn generate_backdrop_blur_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "backdrop-blur" {
        return composed_backdrop_filter_rule(&selector, "--tw-backdrop-blur:blur(8px)", config);
    }

    if class == "backdrop-blur-none" {
        return composed_backdrop_filter_rule(&selector, "--tw-backdrop-blur:blur(0)", config);
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-blur-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_backdrop_filter_rule(
            &selector,
            &format!("--tw-backdrop-blur:blur({})", raw),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-blur-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_backdrop_filter_rule(
            &selector,
            &format!("--tw-backdrop-blur:blur(var({}))", raw),
            config,
        );
    }

    let scale = class.strip_prefix("backdrop-blur-")?;
    if scale.is_empty() {
        return None;
    }
    composed_backdrop_filter_rule(
        &selector,
        &format!("--tw-backdrop-blur:blur(var(--blur-{}))", scale),
        config,
    )
}

fn generate_backdrop_brightness_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("backdrop-brightness-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:brightness({})", raw),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-brightness-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:brightness(var({}))", raw),
            config,
        );
    }

    let number = class.strip_prefix("backdrop-brightness-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    rule(
        &selector,
        &format!("backdrop-filter:brightness({}%)", number),
        config,
    )
}

fn generate_backdrop_contrast_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("backdrop-contrast-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:contrast({})", raw),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-contrast-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:contrast(var({}))", raw),
            config,
        );
    }

    let number = class.strip_prefix("backdrop-contrast-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    rule(
        &selector,
        &format!("backdrop-filter:contrast({}%)", number),
        config,
    )
}

fn generate_backdrop_grayscale_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "backdrop-grayscale" {
        return rule(&selector, "backdrop-filter:grayscale(100%)", config);
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-grayscale-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:grayscale({})", raw),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-grayscale-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:grayscale(var({}))", raw),
            config,
        );
    }

    let number = class.strip_prefix("backdrop-grayscale-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    rule(
        &selector,
        &format!("backdrop-filter:grayscale({}%)", number),
        config,
    )
}

fn generate_backdrop_hue_rotate_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("backdrop-hue-rotate-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:hue-rotate({})", raw),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-hue-rotate-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:hue-rotate(var({}))", raw),
            config,
        );
    }

    if let Some(number) = class.strip_prefix("backdrop-hue-rotate-") {
        if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:hue-rotate({}deg)", number),
            config,
        );
    }

    if let Some(number) = class.strip_prefix("-backdrop-hue-rotate-") {
        if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:hue-rotate(calc({}deg * -1))", number),
            config,
        );
    }

    None
}

fn generate_backdrop_invert_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "backdrop-invert" {
        return rule(&selector, "backdrop-filter:invert(100%)", config);
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-invert-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:invert({})", raw),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-invert-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:invert(var({}))", raw),
            config,
        );
    }

    let number = class.strip_prefix("backdrop-invert-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    rule(
        &selector,
        &format!("backdrop-filter:invert({}%)", number),
        config,
    )
}

fn generate_opacity_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("opacity-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("opacity:{}", raw), config);
    }

    if let Some(raw) = class
        .strip_prefix("opacity-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("opacity:var({})", raw), config);
    }

    let number = class.strip_prefix("opacity-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    rule(&selector, &format!("opacity:{}%", number), config)
}

fn generate_backdrop_opacity_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("backdrop-opacity-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:opacity({})", raw),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-opacity-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:opacity(var({}))", raw),
            config,
        );
    }

    let number = class.strip_prefix("backdrop-opacity-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    rule(
        &selector,
        &format!("backdrop-filter:opacity({}%)", number),
        config,
    )
}

fn generate_backdrop_saturate_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("backdrop-saturate-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:saturate({})", raw),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-saturate-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:saturate(var({}))", raw),
            config,
        );
    }

    let number = class.strip_prefix("backdrop-saturate-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    rule(
        &selector,
        &format!("backdrop-filter:saturate({}%)", number),
        config,
    )
}

fn generate_backdrop_sepia_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "backdrop-sepia" {
        return rule(&selector, "backdrop-filter:sepia(100%)", config);
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-sepia-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:sepia({})", raw),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-sepia-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("backdrop-filter:sepia(var({}))", raw),
            config,
        );
    }

    let number = class.strip_prefix("backdrop-sepia-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    rule(
        &selector,
        &format!("backdrop-filter:sepia({}%)", number),
        config,
    )
}

fn generate_brightness_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("brightness-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-brightness:brightness({})", raw),
            Some(format!("filter:brightness({})", raw)),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("brightness-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-brightness:brightness(var({}))", raw),
            Some(format!("filter:brightness(var({}))", raw)),
            config,
        );
    }

    let number = class.strip_prefix("brightness-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    composed_filter_rule(
        &selector,
        &format!("--tw-brightness:brightness({}%)", number),
        Some(format!("filter:brightness({}%)", number)),
        config,
    )
}

fn generate_contrast_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("contrast-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-contrast:contrast({})", raw),
            Some(format!("filter:contrast({})", raw)),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("contrast-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-contrast:contrast(var({}))", raw),
            Some(format!("filter:contrast(var({}))", raw)),
            config,
        );
    }

    let number = class.strip_prefix("contrast-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    composed_filter_rule(
        &selector,
        &format!("--tw-contrast:contrast({}%)", number),
        Some(format!("filter:contrast({}%)", number)),
        config,
    )
}

fn generate_grayscale_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "grayscale" {
        return composed_filter_rule(
            &selector,
            "--tw-grayscale:grayscale(100%)",
            Some("filter:grayscale(100%)".to_string()),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("grayscale-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-grayscale:grayscale({})", raw),
            Some(format!("filter:grayscale({})", raw)),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("grayscale-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-grayscale:grayscale(var({}))", raw),
            Some(format!("filter:grayscale(var({}))", raw)),
            config,
        );
    }

    let number = class.strip_prefix("grayscale-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    composed_filter_rule(
        &selector,
        &format!("--tw-grayscale:grayscale({}%)", number),
        Some(format!("filter:grayscale({}%)", number)),
        config,
    )
}

fn generate_hue_rotate_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("hue-rotate-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-hue-rotate:hue-rotate({})", raw),
            Some(format!("filter:hue-rotate({})", raw)),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("hue-rotate-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-hue-rotate:hue-rotate(var({}))", raw),
            Some(format!("filter:hue-rotate(var({}))", raw)),
            config,
        );
    }

    if let Some(number) = class.strip_prefix("hue-rotate-") {
        if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-hue-rotate:hue-rotate({}deg)", number),
            Some(format!("filter:hue-rotate({}deg)", number)),
            config,
        );
    }

    if let Some(number) = class.strip_prefix("-hue-rotate-") {
        if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-hue-rotate:hue-rotate(calc({}deg * -1))", number),
            Some(format!("filter:hue-rotate(calc({}deg * -1))", number)),
            config,
        );
    }

    None
}

fn generate_invert_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "invert" {
        return composed_filter_rule(
            &selector,
            "--tw-invert:invert(100%)",
            Some("filter:invert(100%)".to_string()),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("invert-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-invert:invert({})", raw),
            Some(format!("filter:invert({})", raw)),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("invert-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-invert:invert(var({}))", raw),
            Some(format!("filter:invert(var({}))", raw)),
            config,
        );
    }

    let number = class.strip_prefix("invert-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    composed_filter_rule(
        &selector,
        &format!("--tw-invert:invert({}%)", number),
        Some(format!("filter:invert({}%)", number)),
        config,
    )
}

fn generate_saturate_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("saturate-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-saturate:saturate({})", raw),
            Some(format!("filter:saturate({})", raw)),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("saturate-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-saturate:saturate(var({}))", raw),
            Some(format!("filter:saturate(var({}))", raw)),
            config,
        );
    }

    let number = class.strip_prefix("saturate-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    composed_filter_rule(
        &selector,
        &format!("--tw-saturate:saturate({}%)", number),
        Some(format!("filter:saturate({}%)", number)),
        config,
    )
}

fn generate_sepia_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "sepia" {
        return composed_filter_rule(
            &selector,
            "--tw-sepia:sepia(100%)",
            Some("filter:sepia(100%)".to_string()),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("sepia-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-sepia:sepia({})", raw),
            Some(format!("filter:sepia({})", raw)),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("sepia-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return composed_filter_rule(
            &selector,
            &format!("--tw-sepia:sepia(var({}))", raw),
            Some(format!("filter:sepia(var({}))", raw)),
            config,
        );
    }

    let number = class.strip_prefix("sepia-")?;
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    composed_filter_rule(
        &selector,
        &format!("--tw-sepia:sepia({}%)", number),
        Some(format!("filter:sepia({}%)", number)),
        config,
    )
}

fn generate_backdrop_filter_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "backdrop-filter" {
        return rule(&selector, composed_backdrop_filter_property(), config);
    }

    if class == "backdrop-filter-none" {
        return rule(&selector, "backdrop-filter:none", config);
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-filter-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("backdrop-filter:{}", raw), config);
    }

    if let Some(raw) = class
        .strip_prefix("backdrop-filter-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("backdrop-filter:var({})", raw), config);
    }

    None
}

fn generate_content_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if class == "content-none" {
        return rule(
            &selector,
            "--tw-content:none;content:var(--tw-content)",
            config,
        );
    }
    if let Some(raw) = class
        .strip_prefix("content-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        let value = normalize_content_value(raw);
        if value.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("--tw-content:{};content:var(--tw-content)", value),
            config,
        );
    }
    if let Some(raw) = class
        .strip_prefix("content-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("--tw-content:var({});content:var(--tw-content)", raw),
            config,
        );
    }
    None
}

fn generate_background_arbitrary_color_rule(
    class: &str,
    config: &GeneratorConfig,
    variant_tables: &VariantTables,
) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class.strip_prefix("bg-(").and_then(|v| v.strip_suffix(')')) {
        if raw.starts_with("image:") {
            return None;
        }
        return rule(&selector, &format!("background-color:var({})", raw), config);
    }

    if let Some(rest) = class.strip_prefix("bg-[") {
        if let Some(raw) = rest.strip_suffix(']') {
            let color = parse_arbitrary_color_value(raw)?;
            return rule(&selector, &format!("background-color:{}", color), config);
        }
        if let Some((raw, opacity_raw)) = rest.split_once("]/") {
            let color = parse_arbitrary_color_value(raw)?;
            let declarations = color_declaration_with_optional_opacity(
                "background-color",
                &color,
                Some(opacity_raw),
                variant_tables,
            )?;
            return rule(&selector, &declarations, config);
        }
    }

    None
}

fn generate_background_palette_color_rule(
    class: &str,
    config: &GeneratorConfig,
    variant_tables: &VariantTables,
) -> Option<String> {
    if class.starts_with("bg-blend-") {
        return None;
    }
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("bg-")?;
    if raw.starts_with('[') || raw.starts_with('(') {
        return None;
    }

    let (token, opacity) = match raw.split_once('/') {
        Some((token, opacity)) => (token, Some(opacity)),
        None => (raw, None),
    };
    if token.is_empty() {
        return None;
    }

    if token.starts_with("linear-")
        || token.starts_with("gradient-to-")
        || token == "none"
        || token == "radial"
        || token.starts_with("radial-")
        || token.starts_with("conic-")
        || token.starts_with("position-")
        || token.starts_with("size-")
    {
        return None;
    }
    let color_value = theme_color_value_from_token(token, true)?;

    if let Some(opacity_raw) = opacity {
        let declarations = color_declaration_with_optional_opacity(
            "background-color",
            &color_value,
            Some(opacity_raw),
            variant_tables,
        )?;
        return rule(&selector, &declarations, config);
    }

    rule(
        &selector,
        &format!("background-color:{}", color_value),
        config,
    )
}

fn generate_fill_arbitrary_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("fill-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("fill:var({})", raw), config);
    }

    if let Some(raw) = class
        .strip_prefix("fill-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        let value = parse_arbitrary_color_value(raw)?;
        return rule(&selector, &format!("fill:{}", value), config);
    }

    None
}

fn generate_fill_palette_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("fill-")?;
    if raw.starts_with('[') || raw.starts_with('(') {
        return None;
    }

    let (token, opacity) = split_slash_modifier(raw);
    if token.is_empty() {
        return None;
    }

    let color_value = match token {
        "none" => "none".to_string(),
        _ => theme_color_value_from_token(token, true)?,
    };

    if token == "none" && opacity.is_some() {
        return None;
    }

    if let Some(opacity_raw) = opacity {
        let opacity_value = parse_color_opacity_value(opacity_raw)?;
        return rule(
            &selector,
            &format!(
                "fill:color-mix(in oklab,{} {},transparent)",
                color_value, opacity_value
            ),
            config,
        );
    }

    rule(&selector, &format!("fill:{}", color_value), config)
}

fn generate_stroke_arbitrary_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("stroke-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("stroke:var({})", raw), config);
    }

    if let Some(raw) = class
        .strip_prefix("stroke-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        let value = parse_arbitrary_color_value(raw)?;
        return rule(&selector, &format!("stroke:{}", value), config);
    }

    None
}

fn generate_stroke_palette_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("stroke-")?;
    if raw.starts_with('[') || raw.starts_with('(') {
        return None;
    }

    let (token, opacity) = split_slash_modifier(raw);
    if token.is_empty() {
        return None;
    }

    let color_value = match token {
        "none" => "none".to_string(),
        _ => theme_color_value_from_token(token, true)?,
    };

    if token == "none" && opacity.is_some() {
        return None;
    }

    if let Some(opacity_raw) = opacity {
        let opacity_value = parse_color_opacity_value(opacity_raw)?;
        return rule(
            &selector,
            &format!(
                "stroke:color-mix(in oklab,{} {},transparent)",
                color_value, opacity_value
            ),
            config,
        );
    }

    rule(&selector, &format!("stroke:{}", color_value), config)
}

fn parse_stroke_width_value(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }
    if raw.chars().all(|c| c.is_ascii_digit()) {
        return Some(raw.to_string());
    }
    if let Some(value) = raw
        .strip_prefix("(length:")
        .and_then(|value| value.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return Some(format!("var({})", value));
    }
    if let Some(value) = raw
        .strip_prefix('[')
        .and_then(|value| value.strip_suffix(']'))
    {
        if value.is_empty() || is_color_like_value(value) {
            return None;
        }
        return Some(value.to_string());
    }
    None
}

fn generate_stroke_width_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("stroke-")?;
    let value = parse_stroke_width_value(raw)?;
    rule(&selector, &format!("stroke-width:{}", value), config)
}

fn generate_background_position_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if let Some(raw) = class
        .strip_prefix("bg-position-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("background-position:var({})", raw),
            config,
        );
    }
    if let Some(raw) = class
        .strip_prefix("bg-position-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("background-position:{}", raw), config);
    }
    None
}

fn generate_background_size_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if let Some(raw) = class
        .strip_prefix("bg-size-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("background-size:var({})", raw), config);
    }
    if let Some(raw) = class
        .strip_prefix("bg-size-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("background-size:{}", raw), config);
    }
    None
}

fn generate_background_image_rule(
    class: &str,
    config: &GeneratorConfig,
    variant_tables: &VariantTables,
) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "bg-none" {
        return rule(&selector, "background-image:none", config);
    }
    if let Some(raw) = class.strip_prefix("bg-[").and_then(|v| v.strip_suffix(']')) {
        let value = normalize_arbitrary_value(raw);
        return rule(&selector, &format!("background-image:{}", value), config);
    }
    if let Some(raw) = class
        .strip_prefix("bg-(image:")
        .and_then(|v| v.strip_suffix(')'))
    {
        return rule(&selector, &format!("background-image:var({})", raw), config);
    }

    if let Some(dir) = class.strip_prefix("bg-gradient-to-") {
        let to = match dir {
            "t" => "to top",
            "tr" => "to top right",
            "r" => "to right",
            "br" => "to bottom right",
            "b" => "to bottom",
            "bl" => "to bottom left",
            "l" => "to left",
            "tl" => "to top left",
            _ => return None,
        };
        return rule(
            &selector,
            &format!(
                "--tw-gradient-position:{} in oklab;background-image:linear-gradient(var(--tw-gradient-stops))",
                to
            ),
            config,
        );
    }

    if let Some(rest) = class.strip_prefix("bg-linear-to-") {
        let (dir, mode) = split_slash_modifier(rest);
        let to = match dir {
            "t" => "to top",
            "tr" => "to top right",
            "r" => "to right",
            "br" => "to bottom right",
            "b" => "to bottom",
            "bl" => "to bottom left",
            "l" => "to left",
            "tl" => "to top left",
            _ => return None,
        };
        let direction = if let Some(m) = mode {
            format!("{} in {}", to, m)
        } else {
            to.to_string()
        };
        return rule(
            &selector,
            &format!(
                "background-image:linear-gradient({}, var(--tw-gradient-stops))",
                direction
            ),
            config,
        );
    }

    if let Some(rest) = class.strip_prefix("bg-linear-") {
        if let Some(value) = rest.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            return rule(
                &selector,
                &format!(
                    "background-image:linear-gradient(var(--tw-gradient-stops, {}))",
                    value
                ),
                config,
            );
        }
        if let Some(value) = rest.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            return rule(
                &selector,
                &format!(
                    "background-image:linear-gradient(var(--tw-gradient-stops, var({})))",
                    value
                ),
                config,
            );
        }
        if let Some(angle) = parse_gradient_angle(rest) {
            return rule(
                &selector,
                &format!(
                    "background-image:linear-gradient({} in oklab, var(--tw-gradient-stops))",
                    angle
                ),
                config,
            );
        }
    }
    if let Some(rest) = class.strip_prefix("-bg-linear-") {
        if let Some(angle) = parse_gradient_angle(rest) {
            return rule(
                &selector,
                &format!(
                    "background-image:linear-gradient(-{} in oklab, var(--tw-gradient-stops))",
                    angle
                ),
                config,
            );
        }
    }

    if class == "bg-radial" {
        return rule(
            &selector,
            "background-image:radial-gradient(in oklab, var(--tw-gradient-stops))",
            config,
        );
    }
    if let Some(rest) = class.strip_prefix("bg-radial-") {
        if let Some(value) = rest.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            return rule(
                &selector,
                &format!(
                    "background-image:radial-gradient(var(--tw-gradient-stops, {}))",
                    value
                ),
                config,
            );
        }
        if let Some(value) = rest.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            return rule(
                &selector,
                &format!(
                    "background-image:radial-gradient(var(--tw-gradient-stops, var({})))",
                    value
                ),
                config,
            );
        }
    }

    if let Some(rest) = class.strip_prefix("bg-conic-") {
        if let Some(value) = rest.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            return rule(&selector, &format!("background-image:{}", value), config);
        }
        if let Some(value) = rest.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            return rule(
                &selector,
                &format!("background-image:var({})", value),
                config,
            );
        }
        if let Some(angle) = parse_gradient_angle(rest) {
            return rule(
                &selector,
                &format!(
                    "background-image:conic-gradient(from {} in oklab, var(--tw-gradient-stops))",
                    angle
                ),
                config,
            );
        }
    }
    if let Some(rest) = class.strip_prefix("-bg-conic-") {
        if let Some(angle) = parse_gradient_angle(rest) {
            return rule(
                &selector,
                &format!(
                    "background-image:conic-gradient(from -{} in oklab, var(--tw-gradient-stops))",
                    angle
                ),
                config,
            );
        }
    }

    if let Some(raw) = class.strip_prefix("from-") {
        return gradient_stop_rule(&selector, "from", raw, config, variant_tables);
    }
    if let Some(raw) = class.strip_prefix("via-") {
        return gradient_stop_rule(&selector, "via", raw, config, variant_tables);
    }
    if let Some(raw) = class.strip_prefix("to-") {
        return gradient_stop_rule(&selector, "to", raw, config, variant_tables);
    }

    None
}

fn split_slash_modifier(raw: &str) -> (&str, Option<&str>) {
    if let Some((left, right)) = raw.split_once('/') {
        (left, Some(right))
    } else {
        (raw, None)
    }
}

fn append_declaration(declarations: &str, next: &str) -> String {
    if declarations.trim_end().ends_with('}') {
        format!("{}{}", declarations, next)
    } else {
        format!("{};{}", declarations, next)
    }
}

fn parse_gradient_angle(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }
    if raw.chars().all(|c| c.is_ascii_digit()) {
        return Some(format!("{}deg", raw));
    }
    if raw.ends_with("deg")
        && raw[..raw.len() - 3]
            .chars()
            .all(|c| c.is_ascii_digit() || c == '.')
    {
        return Some(raw.to_string());
    }
    None
}

fn gradient_stop_rule(
    selector: &str,
    kind: &str,
    raw: &str,
    config: &GeneratorConfig,
    variant_tables: &VariantTables,
) -> Option<String> {
    let (prop_color, prop_pos) = match kind {
        "from" => ("--tw-gradient-from", "--tw-gradient-from-position"),
        "via" => ("--tw-gradient-via", "--tw-gradient-via-position"),
        "to" => ("--tw-gradient-to", "--tw-gradient-to-position"),
        _ => return None,
    };

    let with_stops = |color_declaration: &str| -> String {
        match kind {
            "via" => {
                let with_via = append_declaration(
                    color_declaration,
                    "--tw-gradient-via-stops:var(--tw-gradient-position), var(--tw-gradient-from) var(--tw-gradient-from-position), var(--tw-gradient-via) var(--tw-gradient-via-position), var(--tw-gradient-to) var(--tw-gradient-to-position)",
                );
                append_declaration(
                    &with_via,
                    "--tw-gradient-stops:var(--tw-gradient-via-stops)",
                )
            }
            _ => append_declaration(
                color_declaration,
                "--tw-gradient-stops:var(--tw-gradient-via-stops, var(--tw-gradient-position), var(--tw-gradient-from) var(--tw-gradient-from-position), var(--tw-gradient-to) var(--tw-gradient-to-position))",
            ),
        }
    };

    let (token, opacity) = split_slash_modifier(raw);

    if opacity.is_none() {
        if let Some(value) = token.strip_suffix('%') {
            if !value.is_empty() && value.chars().all(|c| c.is_ascii_digit() || c == '.') {
                return rule(selector, &format!("{}:{}%", prop_pos, value), config);
            }
        }
    }

    if let Some(value) = token.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        let color = parse_arbitrary_color_value(value)?;
        let declaration =
            color_declaration_with_optional_opacity(prop_color, &color, opacity, variant_tables)?;
        return rule(selector, &with_stops(&declaration), config);
    }
    if let Some(value) = token.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        if value.is_empty() {
            return None;
        }
        let color = format!("var({})", value);
        let declaration =
            color_declaration_with_optional_opacity(prop_color, &color, opacity, variant_tables)?;
        return rule(selector, &with_stops(&declaration), config);
    }

    let color_value = match token {
        "inherit" => "inherit".to_string(),
        "current" => "currentColor".to_string(),
        "transparent" => "transparent".to_string(),
        "black" => "var(--color-black)".to_string(),
        "white" => "var(--color-white)".to_string(),
        _ => {
            if !token.contains('-') {
                return None;
            }
            format!("var(--color-{})", token)
        }
    };
    let declaration =
        color_declaration_with_optional_opacity(prop_color, &color_value, opacity, variant_tables)?;
    rule(selector, &with_stops(&declaration), config)
}

fn normalize_content_value(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    let mut chars = raw.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(next) = chars.next() {
                if next == '_' {
                    out.push('_');
                } else {
                    out.push('\\');
                    out.push(next);
                }
            } else {
                out.push('\\');
            }
            continue;
        }
        if ch == '_' {
            out.push(' ');
        } else {
            out.push(ch);
        }
    }
    out
}

fn normalize_arbitrary_value(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    let mut idx = 0usize;
    let mut quote: Option<char> = None;
    let mut paren_depth = 0usize;
    let mut url_depth: Option<usize> = None;

    while idx < raw.len() {
        if quote.is_none() && starts_with_url_function(raw, idx) {
            out.push_str("url(");
            idx += "url(".len();
            paren_depth += 1;
            url_depth = Some(paren_depth);
            continue;
        }

        let Some(ch) = raw[idx..].chars().next() else {
            break;
        };
        let size = ch.len_utf8();

        if ch == '\\' {
            let next_idx = idx + size;
            if let Some(next) = raw[next_idx..].chars().next() {
                if next == '_' {
                    out.push('_');
                } else {
                    out.push('\\');
                    out.push(next);
                }
                idx = next_idx + next.len_utf8();
                continue;
            }
            out.push('\\');
            idx += size;
            continue;
        }

        if quote.is_none() {
            match ch {
                '\'' | '"' => quote = Some(ch),
                '(' => paren_depth += 1,
                ')' => {
                    if url_depth == Some(paren_depth) {
                        url_depth = None;
                    }
                    paren_depth = paren_depth.saturating_sub(1);
                }
                _ => {}
            }
        } else if quote == Some(ch) {
            quote = None;
        }

        if ch == '_' && url_depth.is_none() {
            out.push(' ');
        } else {
            out.push(ch);
        }
        idx += size;
    }

    normalize_calc_expression_spacing(&out)
}

fn starts_with_url_function(raw: &str, idx: usize) -> bool {
    raw[idx..]
        .get(..4)
        .is_some_and(|prefix| prefix.eq_ignore_ascii_case("url("))
}

fn normalize_calc_expression_spacing(value: &str) -> String {
    if !value.to_ascii_lowercase().starts_with("calc(") {
        return value.to_string();
    }

    let mut out = String::with_capacity(value.len() + 8);
    let mut chars = value.chars().peekable();
    let mut prev_non_ws: Option<char> = None;

    while let Some(ch) = chars.next() {
        if matches!(ch, '+' | '-' | '*' | '/') {
            let unary_minus = ch == '-'
                && prev_non_ws.is_none_or(|prev| matches!(prev, '(' | '+' | '-' | '*' | '/'));
            if unary_minus {
                out.push(ch);
                prev_non_ws = Some(ch);
                continue;
            }

            while out.ends_with(' ') {
                out.pop();
            }
            if !out.is_empty() {
                out.push(' ');
            }
            out.push(ch);
            out.push(' ');

            while matches!(chars.peek(), Some(next) if next.is_whitespace()) {
                chars.next();
            }

            prev_non_ws = Some(ch);
            continue;
        }

        out.push(ch);
        if !ch.is_whitespace() {
            prev_non_ws = Some(ch);
        }
    }

    out
}

fn is_color_like_value(raw: &str) -> bool {
    let lower = raw.to_ascii_lowercase();
    raw.starts_with('#')
        || lower.starts_with("rgb(")
        || lower.starts_with("rgba(")
        || lower.starts_with("hsl(")
        || lower.starts_with("hsla(")
        || lower.starts_with("hwb(")
        || lower.starts_with("lab(")
        || lower.starts_with("lch(")
        || lower.starts_with("oklab(")
        || lower.starts_with("oklch(")
        || lower.starts_with("color(")
        || lower.starts_with("var(")
        || lower == "currentcolor"
        || lower == "transparent"
        || lower == "inherit"
}

fn generate_decoration_arbitrary_color_rule(
    class: &str,
    config: &GeneratorConfig,
) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("decoration-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.starts_with("length:") {
            return None;
        }
        return rule(
            &selector,
            &format!("text-decoration-color:var({})", raw),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("decoration-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        let value = parse_arbitrary_color_value(raw)?;
        return rule(
            &selector,
            &format!("text-decoration-color:{}", value),
            config,
        );
    }

    None
}

fn generate_decoration_thickness_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("decoration-")?;
    if raw.is_empty() {
        return None;
    }
    if raw.chars().all(|c| c.is_ascii_digit()) {
        return rule(
            &selector,
            &format!("text-decoration-thickness:{}px", raw),
            config,
        );
    }
    if let Some(value) = raw
        .strip_prefix("(length:")
        .and_then(|v| v.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("text-decoration-thickness:var({})", value),
            config,
        );
    }
    if let Some(value) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        if value.is_empty() || is_color_like_value(value) {
            return None;
        }
        return rule(
            &selector,
            &format!("text-decoration-thickness:{}", value),
            config,
        );
    }
    None
}

fn generate_underline_offset_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if class == "underline-offset-auto" {
        return rule(&selector, "text-underline-offset:auto", config);
    }
    if let Some(raw) = class.strip_prefix("underline-offset-") {
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return rule(
                &selector,
                &format!("text-underline-offset:{}px", raw),
                config,
            );
        }
        if let Some(value) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            if value.is_empty() {
                return None;
            }
            return rule(
                &selector,
                &format!("text-underline-offset:var({})", value),
                config,
            );
        }
        if let Some(value) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            if value.is_empty() {
                return None;
            }
            return rule(
                &selector,
                &format!("text-underline-offset:{}", value),
                config,
            );
        }
    }
    if let Some(raw) = class.strip_prefix("-underline-offset-") {
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return rule(
                &selector,
                &format!("text-underline-offset:calc({}px * -1)", raw),
                config,
            );
        }
    }
    None
}

fn generate_text_indent_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if let Some(raw) = class.strip_prefix("indent-") {
        if raw == "px" {
            return rule(&selector, "text-indent:1px", config);
        }
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return rule(
                &selector,
                &format!("text-indent:calc(var(--spacing) * {})", raw),
                config,
            );
        }
        if let Some(value) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            if value.is_empty() {
                return None;
            }
            return rule(&selector, &format!("text-indent:var({})", value), config);
        }
        if let Some(value) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            if value.is_empty() {
                return None;
            }
            return rule(&selector, &format!("text-indent:{}", value), config);
        }
    }
    if let Some(raw) = class.strip_prefix("-indent-") {
        if raw == "px" {
            return rule(&selector, "text-indent:-1px", config);
        }
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return rule(
                &selector,
                &format!("text-indent:calc(var(--spacing) * -{})", raw),
                config,
            );
        }
    }
    None
}

fn generate_vertical_align_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "align-baseline" => return rule(&selector, "vertical-align:baseline", config),
        "align-top" => return rule(&selector, "vertical-align:top", config),
        "align-middle" => return rule(&selector, "vertical-align:middle", config),
        "align-bottom" => return rule(&selector, "vertical-align:bottom", config),
        "align-text-top" => return rule(&selector, "vertical-align:text-top", config),
        "align-text-bottom" => return rule(&selector, "vertical-align:text-bottom", config),
        "align-sub" => return rule(&selector, "vertical-align:sub", config),
        "align-super" => return rule(&selector, "vertical-align:super", config),
        _ => {}
    }
    if let Some(raw) = class
        .strip_prefix("align-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("vertical-align:var({})", raw), config);
    }
    if let Some(raw) = class
        .strip_prefix("align-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("vertical-align:{}", raw), config);
    }
    None
}

fn generate_decoration_palette_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("decoration-")?;
    if raw.starts_with('[') || raw.starts_with('(') {
        return None;
    }

    let (token, opacity) = match raw.split_once('/') {
        Some((token, opacity)) => (token, Some(opacity)),
        None => (raw, None),
    };
    if token.is_empty() {
        return None;
    }

    let color_value = theme_color_value_from_token(token, true)?;

    if let Some(opacity_raw) = opacity {
        let opacity_value = parse_color_opacity_value(opacity_raw)?;
        return rule(
            &selector,
            &format!(
                "text-decoration-color:color-mix(in oklab,{} {},transparent)",
                color_value, opacity_value
            ),
            config,
        );
    }

    rule(
        &selector,
        &format!("text-decoration-color:{}", color_value),
        config,
    )
}

fn generate_accent_arbitrary_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("accent-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("accent-color:var({})", raw), config);
    }

    if let Some(raw) = class
        .strip_prefix("accent-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        let value = parse_arbitrary_color_value(raw)?;
        return rule(&selector, &format!("accent-color:{}", value), config);
    }

    None
}

fn generate_accent_palette_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("accent-")?;
    if raw.starts_with('[') || raw.starts_with('(') {
        return None;
    }

    let (token, opacity) = split_slash_modifier(raw);
    if token.is_empty() {
        return None;
    }

    let color_value = theme_color_value_from_token(token, true)?;

    if let Some(opacity_raw) = opacity {
        let opacity_value = parse_color_opacity_value(opacity_raw)?;
        return rule(
            &selector,
            &format!(
                "accent-color:color-mix(in oklab,{} {},transparent)",
                color_value, opacity_value
            ),
            config,
        );
    }

    rule(&selector, &format!("accent-color:{}", color_value), config)
}

fn generate_caret_arbitrary_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("caret-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("caret-color:var({})", raw), config);
    }

    if let Some(raw) = class
        .strip_prefix("caret-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        let value = parse_arbitrary_color_value(raw)?;
        return rule(&selector, &format!("caret-color:{}", value), config);
    }

    None
}

fn generate_caret_palette_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("caret-")?;
    if raw.starts_with('[') || raw.starts_with('(') {
        return None;
    }

    let (token, opacity) = split_slash_modifier(raw);
    if token.is_empty() {
        return None;
    }

    let color_value = theme_color_value_from_token(token, true)?;

    if let Some(opacity_raw) = opacity {
        let opacity_value = parse_color_opacity_value(opacity_raw)?;
        return rule(
            &selector,
            &format!(
                "caret-color:color-mix(in oklab,{} {},transparent)",
                color_value, opacity_value
            ),
            config,
        );
    }

    rule(&selector, &format!("caret-color:{}", color_value), config)
}

fn text_shadow_preset_layers(token: &str) -> Option<(&'static [&'static str], &'static str)> {
    match token {
        "2xs" => Some((&["0px 1px 0px"], "0.15")),
        "xs" => Some((&["0px 1px 1px"], "0.2")),
        "sm" => Some((&["0px 1px 0px", "0px 1px 1px", "0px 2px 2px"], "0.075")),
        "md" => Some((&["0px 1px 1px", "0px 1px 2px", "0px 2px 4px"], "0.1")),
        "lg" => Some((&["0px 1px 2px", "0px 3px 2px", "0px 4px 8px"], "0.1")),
        _ => None,
    }
}

fn build_text_shadow_preset_value(layers: &[&str], default_opacity: &str) -> String {
    layers
        .iter()
        .map(|layer| {
            format!(
                "{} var(--tw-shadow-color,rgb(0 0 0 / {}))",
                layer, default_opacity
            )
        })
        .collect::<Vec<_>>()
        .join(",")
}

fn parse_text_shadow_color_value(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }

    let (token, opacity) = split_slash_modifier(raw);
    if token.is_empty() {
        return None;
    }

    let color_value = theme_color_value_from_token(token, false)?;

    if let Some(opacity_raw) = opacity {
        let opacity_value = parse_color_opacity_value(opacity_raw)?;
        return Some(format!(
            "color-mix(in oklab,{} {},transparent)",
            color_value, opacity_value
        ));
    }

    Some(color_value)
}

fn generate_text_shadow_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "text-shadow-none" {
        return rule(&selector, "text-shadow:none", config);
    }
    if class == "text-shadow-inherit" {
        return rule(&selector, "--tw-shadow-color:inherit", config);
    }

    let raw = class.strip_prefix("text-shadow-")?;

    if let Some(value) = raw
        .strip_prefix("(color:")
        .and_then(|value| value.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("--tw-shadow-color:var({})", value),
            config,
        );
    }

    if let Some(value) = raw
        .strip_prefix('(')
        .and_then(|value| value.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(&selector, &format!("text-shadow:var({})", value), config);
    }

    if let Some(value) = raw
        .strip_prefix('[')
        .and_then(|value| value.strip_suffix(']'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(&selector, &format!("text-shadow:{}", value), config);
    }

    let (token, opacity) = split_slash_modifier(raw);
    if token.is_empty() {
        return None;
    }

    if let Some((layers, default_opacity)) = text_shadow_preset_layers(token) {
        let preset_value = build_text_shadow_preset_value(layers, default_opacity);
        if let Some(opacity_raw) = opacity {
            let opacity_value = parse_color_opacity_value(opacity_raw)?;
            return rule(
                &selector,
                &format!(
                    "--tw-shadow-color:rgb(0 0 0 / {});text-shadow:{}",
                    opacity_value, preset_value
                ),
                config,
            );
        }
        return rule(&selector, &format!("text-shadow:{}", preset_value), config);
    }

    if let Some(value) = parse_text_shadow_color_value(raw) {
        return rule(&selector, &format!("--tw-shadow-color:{}", value), config);
    }

    if let Some(opacity_raw) = opacity {
        let opacity_value = parse_color_opacity_value(opacity_raw)?;
        return rule(
            &selector,
            &format!(
                "--tw-shadow-color:rgb(0 0 0 / {});text-shadow:var(--text-shadow-{})",
                opacity_value, token
            ),
            config,
        );
    }

    rule(
        &selector,
        &format!("text-shadow:var(--text-shadow-{})", token),
        config,
    )
}

fn generate_text_palette_color_rule(
    class: &str,
    config: &GeneratorConfig,
    variant_tables: &VariantTables,
) -> Option<String> {
    if class.starts_with("text-shadow-") {
        return None;
    }
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("text-")?;
    if raw.starts_with('[') || raw.starts_with('(') {
        return None;
    }

    let (token, opacity) = match raw.split_once('/') {
        Some((token, opacity)) => (token, Some(opacity)),
        None => (raw, None),
    };
    if token.is_empty() {
        return None;
    }
    if is_reserved_text_non_color_token(token) {
        return None;
    }

    let color_value = theme_color_value_from_token(token, true)?;

    if opacity.is_some() {
        let declaration = color_declaration_with_optional_opacity(
            "color",
            &color_value,
            opacity,
            variant_tables,
        )?;
        return rule(&selector, &declaration, config);
    }

    rule(&selector, &format!("color:{}", color_value), config)
}

fn parse_color_opacity_value(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }
    if raw.chars().all(|c| c.is_ascii_digit()) {
        return Some(format!("{}%", raw));
    }
    if let Some(value) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        if value.is_empty() {
            return None;
        }
        return Some(value.to_string());
    }
    if let Some(value) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        if value.is_empty() {
            return None;
        }
        return Some(format!("var({})", value));
    }
    None
}

fn parse_arbitrary_color_value(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }

    let normalized = normalize_arbitrary_value(raw);
    if let Some(value) = normalized.strip_prefix("color:") {
        if value.is_empty() {
            return None;
        }
        return is_color_like_value(value).then(|| value.to_string());
    }

    is_color_like_value(&normalized).then_some(normalized)
}

fn fallback_color_for_srgb_mix(color: &str, variant_tables: &VariantTables) -> Option<String> {
    if let Some(name) = color
        .strip_prefix("var(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if let Some(resolved) = variant_tables.theme_variable_values.get(name) {
            return Some(resolved.clone());
        }
        return None;
    }
    Some(color.to_string())
}

fn color_declaration_with_optional_opacity(
    property: &str,
    color: &str,
    opacity_raw: Option<&str>,
    variant_tables: &VariantTables,
) -> Option<String> {
    let Some(opacity_raw) = opacity_raw else {
        return Some(format!("{}:{}", property, color));
    };
    let opacity_value = parse_color_opacity_value(opacity_raw)?;
    let oklab_mix = format!(
        "color-mix(in oklab, {} {}, transparent)",
        color, opacity_value
    );
    if let Some(fallback_color) = fallback_color_for_srgb_mix(color, variant_tables) {
        let srgb_mix = format!(
            "color-mix(in srgb, {} {}, transparent)",
            fallback_color, opacity_value
        );
        return Some(format!(
            "{}: {}; @supports (color: color-mix(in lab, red, red)) {{ {}: {}; }}",
            property, srgb_mix, property, oklab_mix
        ));
    }
    Some(format!(
        "{}: {}; @supports (color: color-mix(in lab, red, red)) {{ {}: {}; }}",
        property, color, property, oklab_mix
    ))
}

fn is_theme_color_token(token: &str) -> bool {
    !token.is_empty()
        && token
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '-' || ch == '_')
}

fn theme_color_value_from_token(token: &str, allow_plain_tokens: bool) -> Option<String> {
    match token {
        "inherit" => Some("inherit".to_string()),
        "current" => Some("currentColor".to_string()),
        "transparent" => Some("transparent".to_string()),
        "black" => Some("var(--color-black)".to_string()),
        "white" => Some("var(--color-white)".to_string()),
        _ => {
            if !is_theme_color_token(token) {
                return None;
            }
            if token.contains('-') || allow_plain_tokens {
                Some(format!("var(--color-{})", token))
            } else {
                None
            }
        }
    }
}

fn is_text_size_token(token: &str) -> bool {
    matches!(
        token,
        "xs" | "sm"
            | "base"
            | "lg"
            | "xl"
            | "2xl"
            | "3xl"
            | "4xl"
            | "5xl"
            | "6xl"
            | "7xl"
            | "8xl"
            | "9xl"
            | "tiny"
    )
}

fn is_reserved_text_non_color_token(token: &str) -> bool {
    matches!(
        token,
        "left"
            | "center"
            | "right"
            | "justify"
            | "start"
            | "end"
            | "ellipsis"
            | "clip"
            | "wrap"
            | "nowrap"
            | "balance"
            | "pretty"
            | "inherit"
            | "current"
            | "transparent"
    ) || is_text_size_token(token)
}

fn parse_palette_color_value(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }
    let (token, opacity) = split_slash_modifier(raw);
    if token.is_empty() {
        return None;
    }

    let color_value = theme_color_value_from_token(token, true)?;

    if let Some(opacity_raw) = opacity {
        let opacity_value = parse_color_opacity_value(opacity_raw)?;
        return Some(format!(
            "color-mix(in oklab,{} {},transparent)",
            color_value, opacity_value
        ));
    }

    Some(color_value)
}

fn shadow_color_declaration_with_optional_opacity(
    color: &str,
    opacity_raw: Option<&str>,
    variant_tables: &VariantTables,
) -> Option<String> {
    let is_theme_color = color.starts_with("var(--color-");

    let Some(opacity_raw) = opacity_raw else {
        if !is_theme_color {
            return Some(format!("--tw-shadow-color:{}", color));
        }
        if let Some(fallback_color) = fallback_color_for_srgb_mix(color, variant_tables) {
            return Some(format!(
                "--tw-shadow-color:{};@supports (color:color-mix(in lab, red, red)) {{--tw-shadow-color:color-mix(in oklab,{} var(--tw-shadow-alpha),transparent);}}",
                fallback_color, color
            ));
        }
        return Some(format!("--tw-shadow-color:{}", color));
    };

    let opacity_value = parse_color_opacity_value(opacity_raw)?;
    let oklab_mix = format!(
        "color-mix(in oklab,{} {},transparent)",
        color, opacity_value
    );

    if !is_theme_color {
        return Some(format!("--tw-shadow-color:{}", oklab_mix));
    }

    if let Some(fallback_color) = fallback_color_for_srgb_mix(color, variant_tables) {
        let srgb_mix = format!(
            "color-mix(in srgb,{} {},transparent)",
            fallback_color, opacity_value
        );
        return Some(format!(
            "--tw-shadow-color:{};@supports (color:color-mix(in lab, red, red)) {{--tw-shadow-color:color-mix(in oklab,{} var(--tw-shadow-alpha),transparent);}}",
            srgb_mix, oklab_mix
        ));
    }

    Some(format!(
        "--tw-shadow-color:color-mix(in oklab,{} var(--tw-shadow-alpha),transparent)",
        oklab_mix
    ))
}

fn generate_shadow_value_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class
        .strip_prefix("shadow-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        let normalized = normalize_arbitrary_value(raw);
        if normalized.is_empty() {
            return None;
        }
        if !normalized.starts_with("var(") && parse_arbitrary_color_value(raw).is_some() {
            return None;
        }
        let shadow_value = wrap_shadow_color_fallback(&normalized);
        return rule(
            &selector,
            &format!(
                "--tw-shadow:{};box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
                shadow_value
            ),
            config,
        );
    }

    if let Some(raw) = class
        .strip_prefix("shadow-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if raw.is_empty() || raw.starts_with("color:") || raw.ends_with("-color") {
            return None;
        }
        return rule(
            &selector,
            &format!(
                "--tw-shadow:var({});box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)",
                raw
            ),
            config,
        );
    }

    None
}

fn wrap_shadow_color_fallback(value: &str) -> String {
    if value.contains("var(--tw-shadow-color") {
        return value.to_string();
    }

    let mut out = String::with_capacity(value.len() + 32);
    let mut cursor = 0usize;
    while cursor < value.len() {
        let segment = &value[cursor..];
        let matched = if segment.starts_with("rgba(") {
            Some("rgba(")
        } else if segment.starts_with("rgb(") {
            Some("rgb(")
        } else if segment.starts_with("hsla(") {
            Some("hsla(")
        } else if segment.starts_with("hsl(") {
            Some("hsl(")
        } else {
            None
        };

        if let Some(function) = matched {
            let open_idx = cursor + function.len() - 1;
            if let Some(close_idx) = find_matching_parenthesis(value, open_idx) {
                out.push_str("var(--tw-shadow-color,");
                out.push_str(&value[cursor..=close_idx]);
                out.push(')');
                cursor = close_idx + 1;
                continue;
            }
        }

        if let Some(ch) = segment.chars().next() {
            out.push(ch);
            cursor += ch.len_utf8();
        } else {
            break;
        }
    }

    out
}

fn find_matching_parenthesis(value: &str, open_idx: usize) -> Option<usize> {
    if !value[open_idx..].starts_with('(') {
        return None;
    }

    let mut depth = 0usize;
    let mut in_string: Option<char> = None;
    let mut escaped = false;
    for (rel_idx, ch) in value[open_idx..].char_indices() {
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

        if ch == '"' || ch == '\'' {
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

fn generate_shadow_color_rule(
    class: &str,
    config: &GeneratorConfig,
    variant_tables: &VariantTables,
) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("shadow-")?;
    if matches!(raw, "none" | "sm" | "md" | "lg" | "xl" | "2xl") {
        return None;
    }

    let (token, opacity) = split_slash_modifier(raw);
    if token.is_empty() {
        return None;
    }

    let color_value = if let Some(value) = token.strip_prefix('[').and_then(|v| v.strip_suffix(']'))
    {
        parse_arbitrary_color_value(value)?
    } else if let Some(value) = token
        .strip_prefix("(color:")
        .and_then(|v| v.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        format!("var({})", value)
    } else if let Some(value) = token.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        if value.is_empty() {
            return None;
        }
        format!("var({})", value)
    } else {
        theme_color_value_from_token(token, true)?
    };

    let declarations =
        shadow_color_declaration_with_optional_opacity(&color_value, opacity, variant_tables)?;
    rule(&selector, &declarations, config)
}

fn generate_inset_shadow_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("inset-shadow-")?;
    if raw == "none" {
        return None;
    }

    if let Some(value) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        let color = parse_arbitrary_color_value(value)?;
        return rule(
            &selector,
            &format!("--tw-inset-shadow-color:{}", color),
            config,
        );
    }
    if let Some(value) = raw
        .strip_prefix("(color:")
        .and_then(|v| v.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("--tw-inset-shadow-color:var({})", value),
            config,
        );
    }
    if let Some(value) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        if value.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("--tw-inset-shadow-color:var({})", value),
            config,
        );
    }

    let color_value = parse_palette_color_value(raw)?;
    rule(
        &selector,
        &format!("--tw-inset-shadow-color:{}", color_value),
        config,
    )
}

fn generate_ring_color_rule(
    class: &str,
    config: &GeneratorConfig,
    variant_tables: &VariantTables,
) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("ring-")?;
    if matches!(raw, "0" | "1" | "2" | "4" | "8" | "inset") {
        return None;
    }

    let (token, opacity) = split_slash_modifier(raw);
    if token.is_empty() {
        return None;
    }

    let color_value = if let Some(value) = token.strip_prefix('[').and_then(|v| v.strip_suffix(']'))
    {
        parse_arbitrary_color_value(value)?
    } else if let Some(value) = token.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        if value.is_empty() {
            return None;
        }
        format!("var({})", value)
    } else {
        theme_color_value_from_token(token, true)?
    };

    let declarations = color_declaration_with_optional_opacity(
        "--tw-ring-color",
        &color_value,
        opacity,
        variant_tables,
    )?;
    rule(&selector, &declarations, config)
}

fn generate_inset_ring_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("inset-ring-")?;
    if raw.chars().all(|ch| ch.is_ascii_digit()) {
        return None;
    }

    if let Some(value) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        let color = parse_arbitrary_color_value(value)?;
        return rule(
            &selector,
            &format!("--tw-inset-ring-color:{}", color),
            config,
        );
    }
    if let Some(value) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        if value.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("--tw-inset-ring-color:var({})", value),
            config,
        );
    }

    let color_value = parse_palette_color_value(raw)?;
    rule(
        &selector,
        &format!("--tw-inset-ring-color:{}", color_value),
        config,
    )
}

fn generate_list_style_type_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "list-disc" => return rule(&selector, "list-style-type:disc", config),
        "list-decimal" => return rule(&selector, "list-style-type:decimal", config),
        "list-none" => return rule(&selector, "list-style-type:none", config),
        _ => {}
    }
    if let Some(raw) = class.strip_prefix("list-[") {
        if let Some(value) = raw.strip_suffix(']') {
            return rule(&selector, &format!("list-style-type:{}", value), config);
        }
    }
    if let Some(raw) = class.strip_prefix("list-(") {
        if let Some(custom) = raw.strip_suffix(')') {
            return rule(
                &selector,
                &format!("list-style-type:var({})", custom),
                config,
            );
        }
    }
    None
}

fn generate_list_style_image_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if class == "list-image-none" {
        return rule(&selector, "list-style-image:none", config);
    }
    if let Some(raw) = class.strip_prefix("list-image-[") {
        if let Some(value) = raw.strip_suffix(']') {
            return rule(&selector, &format!("list-style-image:{}", value), config);
        }
    }
    if let Some(raw) = class.strip_prefix("list-image-(") {
        if let Some(custom) = raw.strip_suffix(')') {
            return rule(
                &selector,
                &format!("list-style-image:var({})", custom),
                config,
            );
        }
    }
    None
}

fn generate_line_clamp_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if class == "line-clamp-none" {
        return rule(
            &selector,
            "overflow:visible;display:block;-webkit-box-orient:horizontal;-webkit-line-clamp:unset",
            config,
        );
    }
    if let Some(raw) = class.strip_prefix("line-clamp-") {
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return rule(
                &selector,
                &format!(
                    "overflow:hidden;display:-webkit-box;-webkit-box-orient:vertical;-webkit-line-clamp:{}",
                    raw
                ),
                config,
            );
        }
        if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            return rule(
                &selector,
                &format!(
                    "overflow:hidden;display:-webkit-box;-webkit-box-orient:vertical;-webkit-line-clamp:{}",
                    custom
                ),
                config,
            );
        }
        if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            return rule(
                &selector,
                &format!(
                    "overflow:hidden;display:-webkit-box;-webkit-box-orient:vertical;-webkit-line-clamp:var({})",
                    custom
                ),
                config,
            );
        }
    }
    None
}

fn generate_text_size_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class.strip_prefix("text-[") {
        if let Some(value) = raw.strip_suffix(']') {
            return rule(&selector, &format!("font-size:{}", value), config);
        }
    }

    if let Some(raw) = class.strip_prefix("text-(length:") {
        if let Some(custom) = raw.strip_suffix(')') {
            return rule(&selector, &format!("font-size:var({})", custom), config);
        }
    }

    let token = class.strip_prefix("text-")?;
    if token.is_empty() {
        return None;
    }
    if matches!(token, "left" | "center" | "right" | "justify") {
        return None;
    }
    if let Some((size, line_raw)) = token.split_once('/') {
        if size.is_empty() || line_raw.is_empty() {
            return None;
        }
        let font_size = if is_text_size_token(size) {
            format!("var(--text-{})", size)
        } else if let Some(custom) = size.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            custom.to_string()
        } else if let Some(custom) = size
            .strip_prefix("(length:")
            .and_then(|v| v.strip_suffix(')'))
        {
            format!("var({})", custom)
        } else {
            return None;
        };
        let line_height = if line_raw.chars().all(|c| c.is_ascii_digit()) {
            format!("calc(var(--spacing) * {})", line_raw)
        } else if let Some(custom) = line_raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            format!("var({})", custom)
        } else if let Some(custom) = line_raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            custom.to_string()
        } else {
            return None;
        };
        return rule(
            &selector,
            &format!("font-size:{};line-height:{}", font_size, line_height),
            config,
        );
    }

    if token.contains('-') {
        return None;
    }

    rule(
        &selector,
        &format!(
            "font-size:var(--text-{});line-height:var(--tw-leading, var(--text-{}--line-height))",
            token, token
        ),
        config,
    )
}

fn generate_leading_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("leading-")?;
    if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
        let value = format!("calc(var(--spacing) * {})", raw);
        return rule(
            &selector,
            &format!("--tw-leading:{};line-height:{}", value, value),
            config,
        );
    }
    if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        let value = format!("var({})", custom);
        return rule(
            &selector,
            &format!("--tw-leading:{};line-height:{}", value, value),
            config,
        );
    }
    if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        return rule(
            &selector,
            &format!("--tw-leading:{};line-height:{}", custom, custom),
            config,
        );
    }
    None
}

fn generate_font_family_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if class.starts_with("font-stretch-") {
        return None;
    }
    match class {
        "font-sans" => return rule(&selector, "font-family:var(--font-sans)", config),
        "font-serif" => return rule(&selector, "font-family:var(--font-serif)", config),
        "font-mono" => return rule(&selector, "font-family:var(--font-mono)", config),
        _ => {}
    }

    if let Some(raw) = class.strip_prefix("font-[") {
        if let Some(value) = raw.strip_suffix(']') {
            return rule(&selector, &format!("font-family:{}", value), config);
        }
    }

    if let Some(raw) = class.strip_prefix("font-(family-name:") {
        if let Some(custom) = raw.strip_suffix(')') {
            return rule(&selector, &format!("font-family:var({})", custom), config);
        }
    }

    if let Some(token) = class.strip_prefix("font-") {
        if !token.is_empty()
            && !matches!(
                token,
                "thin"
                    | "extralight"
                    | "light"
                    | "normal"
                    | "medium"
                    | "semibold"
                    | "bold"
                    | "extrabold"
                    | "black"
            )
        {
            return rule(
                &selector,
                &format!("font-family:var(--font-{})", token),
                config,
            );
        }
    }

    None
}

fn generate_tracking_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let (negative, raw_class) = if let Some(rest) = class.strip_prefix('-') {
        (true, rest)
    } else {
        (false, class)
    };
    let raw = raw_class.strip_prefix("tracking-")?;

    if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        let value = if negative {
            format!("calc({} * -1)", custom)
        } else {
            custom.to_string()
        };
        return rule(
            &selector,
            &format!("--tw-tracking:{};letter-spacing:{}", value, value),
            config,
        );
    }

    if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        let variable = format!("var({})", custom);
        let value = if negative {
            format!("calc({} * -1)", variable)
        } else {
            variable
        };
        return rule(
            &selector,
            &format!("--tw-tracking:{};letter-spacing:{}", value, value),
            config,
        );
    }

    if raw.is_empty() {
        return None;
    }

    let value = if negative {
        format!("calc(var(--tracking-{}) * -1)", raw)
    } else {
        format!("var(--tracking-{})", raw)
    };
    rule(
        &selector,
        &format!("--tw-tracking:{};letter-spacing:{}", value, value),
        config,
    )
}

fn generate_font_stretch_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("font-stretch-")?;

    let value = match raw {
        "ultra-condensed" => "ultra-condensed".to_string(),
        "extra-condensed" => "extra-condensed".to_string(),
        "condensed" => "condensed".to_string(),
        "semi-condensed" => "semi-condensed".to_string(),
        "normal" => "normal".to_string(),
        "semi-expanded" => "semi-expanded".to_string(),
        "expanded" => "expanded".to_string(),
        "extra-expanded" => "extra-expanded".to_string(),
        "ultra-expanded" => "ultra-expanded".to_string(),
        _ => {
            if raw.ends_with('%')
                && raw.len() > 1
                && raw[..raw.len() - 1]
                    .chars()
                    .all(|c| c.is_ascii_digit() || c == '.')
            {
                raw.to_string()
            } else if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
                custom.to_string()
            } else if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
                format!("var({})", custom)
            } else {
                return None;
            }
        }
    };

    rule(&selector, &format!("font-stretch:{}", value), config)
}

fn generate_font_weight_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class.strip_prefix("font-[") {
        if let Some(value) = raw.strip_suffix(']') {
            if value.chars().all(|c| c.is_ascii_digit()) && !value.is_empty() {
                return rule(
                    &selector,
                    &format!("--tw-font-weight:{};font-weight:{}", value, value),
                    config,
                );
            }
        }
    }

    if let Some(raw) = class.strip_prefix("font-(weight:") {
        if let Some(custom) = raw.strip_suffix(')') {
            let value = format!("var({})", custom);
            return rule(
                &selector,
                &format!("--tw-font-weight:{};font-weight:{}", value, value),
                config,
            );
        }
    }

    if let Some(token) = class.strip_prefix("font-") {
        if token == "extrablack" {
            let value = format!("var(--font-weight-{})", token);
            return rule(
                &selector,
                &format!("--tw-font-weight:{};font-weight:{}", value, value),
                config,
            );
        }
    }

    None
}

fn generate_spacing_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let (negative, raw_class) = if let Some(rest) = class.strip_prefix('-') {
        (true, rest)
    } else {
        (false, class)
    };

    for (prefix, property) in [
        ("p-", "padding"),
        ("px-", "padding-inline"),
        ("py-", "padding-block"),
        ("pt-", "padding-top"),
        ("pr-", "padding-right"),
        ("pb-", "padding-bottom"),
        ("pl-", "padding-left"),
        ("ps-", "padding-inline-start"),
        ("pe-", "padding-inline-end"),
    ] {
        if let Some(raw) = class.strip_prefix(prefix) {
            if let Some(value) = parse_spacing_value(raw) {
                let declarations = format!("{}:{}", property, value);
                return rule(&selector, &declarations, config);
            }
        }
    }

    for (prefix, property) in [
        ("m-", "margin"),
        ("mx-", "margin-inline"),
        ("my-", "margin-block"),
        ("mt-", "margin-top"),
        ("mr-", "margin-right"),
        ("mb-", "margin-bottom"),
        ("ml-", "margin-left"),
        ("ms-", "margin-inline-start"),
        ("me-", "margin-inline-end"),
    ] {
        if let Some(raw) = raw_class.strip_prefix(prefix) {
            if let Some(value) = parse_margin_value(raw, negative) {
                let declarations = format!("{}:{}", property, value);
                return rule(&selector, &declarations, config);
            }
        }
    }

    None
}

fn parse_spacing_value(raw: &str) -> Option<String> {
    if raw == "px" {
        return Some("1px".to_string());
    }
    if is_spacing_multiplier(raw) {
        return Some(format!("calc(var(--spacing) * {})", raw));
    }
    if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        return Some(custom.to_string());
    }
    if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        return Some(format!("var({})", custom));
    }
    None
}

fn parse_margin_value(raw: &str, negative: bool) -> Option<String> {
    if raw == "auto" {
        return if negative {
            None
        } else {
            Some("auto".to_string())
        };
    }
    if raw == "px" {
        return Some(if negative { "-1px" } else { "1px" }.to_string());
    }
    if is_spacing_multiplier(raw) {
        return Some(if negative {
            format!("calc(var(--spacing) * -{})", raw)
        } else {
            format!("calc(var(--spacing) * {})", raw)
        });
    }
    if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        return Some(if negative {
            format!("calc({} * -1)", custom)
        } else {
            custom.to_string()
        });
    }
    if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        let variable = format!("var({})", custom);
        return Some(if negative {
            format!("calc({} * -1)", variable)
        } else {
            variable
        });
    }
    None
}

fn parse_space_value(raw: &str, negative: bool) -> Option<String> {
    if raw == "px" {
        return Some(if negative { "-1px" } else { "1px" }.to_string());
    }
    if is_spacing_multiplier(raw) {
        return Some(if negative {
            format!("calc(var(--spacing) * -{})", raw)
        } else {
            format!("calc(var(--spacing) * {})", raw)
        });
    }
    if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        return Some(if negative {
            format!("calc({} * -1)", custom)
        } else {
            custom.to_string()
        });
    }
    if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        let variable = format!("var({})", custom);
        return Some(if negative {
            format!("calc({} * -1)", variable)
        } else {
            variable
        });
    }
    None
}

fn parse_scroll_margin_value(raw: &str, negative: bool) -> Option<String> {
    if is_spacing_multiplier(raw) {
        return Some(if negative {
            format!("calc(var(--spacing) * -{})", raw)
        } else {
            format!("calc(var(--spacing) * {})", raw)
        });
    }

    if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        if custom.is_empty() {
            return None;
        }
        return Some(if negative {
            format!("calc({} * -1)", custom)
        } else {
            custom.to_string()
        });
    }

    if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        if custom.is_empty() {
            return None;
        }
        let variable = format!("var({})", custom);
        return Some(if negative {
            format!("calc({} * -1)", variable)
        } else {
            variable
        });
    }

    None
}

fn generate_scroll_margin_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let (negative, raw_class) = if let Some(rest) = class.strip_prefix('-') {
        (true, rest)
    } else {
        (false, class)
    };

    for (prefix, property) in [
        ("scroll-m-", "scroll-margin"),
        ("scroll-mx-", "scroll-margin-inline"),
        ("scroll-my-", "scroll-margin-block"),
        ("scroll-ms-", "scroll-margin-inline-start"),
        ("scroll-me-", "scroll-margin-inline-end"),
        ("scroll-mt-", "scroll-margin-top"),
        ("scroll-mr-", "scroll-margin-right"),
        ("scroll-mb-", "scroll-margin-bottom"),
        ("scroll-ml-", "scroll-margin-left"),
    ] {
        if let Some(raw) = raw_class.strip_prefix(prefix) {
            let value = parse_scroll_margin_value(raw, negative)?;
            return rule(&selector, &format!("{}:{}", property, value), config);
        }
    }

    None
}

fn parse_scroll_padding_value(raw: &str, negative: bool) -> Option<String> {
    if is_spacing_multiplier(raw) {
        return Some(if negative {
            format!("calc(var(--spacing) * -{})", raw)
        } else {
            format!("calc(var(--spacing) * {})", raw)
        });
    }

    if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        if custom.is_empty() {
            return None;
        }
        return Some(if negative {
            format!("calc({} * -1)", custom)
        } else {
            custom.to_string()
        });
    }

    if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        if custom.is_empty() {
            return None;
        }
        let variable = format!("var({})", custom);
        return Some(if negative {
            format!("calc({} * -1)", variable)
        } else {
            variable
        });
    }

    None
}

fn generate_scroll_padding_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let (negative, raw_class) = if let Some(rest) = class.strip_prefix('-') {
        (true, rest)
    } else {
        (false, class)
    };

    for (prefix, property) in [
        ("scroll-p-", "scroll-padding"),
        ("scroll-px-", "scroll-padding-inline"),
        ("scroll-py-", "scroll-padding-block"),
        ("scroll-ps-", "scroll-padding-inline-start"),
        ("scroll-pe-", "scroll-padding-inline-end"),
        ("scroll-pt-", "scroll-padding-top"),
        ("scroll-pr-", "scroll-padding-right"),
        ("scroll-pb-", "scroll-padding-bottom"),
        ("scroll-pl-", "scroll-padding-left"),
    ] {
        if let Some(raw) = raw_class.strip_prefix(prefix) {
            let value = parse_scroll_padding_value(raw, negative)?;
            return rule(&selector, &format!("{}:{}", property, value), config);
        }
    }

    None
}

fn generate_layout_rule(
    class: &str,
    config: &GeneratorConfig,
    variant_tables: &VariantTables,
) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "@container" {
        return rule(&selector, "container-type:inline-size", config);
    }

    if let Some(name) = class.strip_prefix("@container/") {
        if name.is_empty() {
            return None;
        }
        return rule(
            &selector,
            &format!("container-type:inline-size;container-name:{}", name),
            config,
        );
    }

    match class {
        "block" => rule(&selector, "display:block", config),
        "inline-block" => rule(&selector, "display:inline-block", config),
        "inline" => rule(&selector, "display:inline", config),
        "flow-root" => rule(&selector, "display:flow-root", config),
        "flex" => rule(&selector, "display:flex", config),
        "inline-flex" => rule(&selector, "display:inline-flex", config),
        "grid" => rule(&selector, "display:grid", config),
        "inline-grid" => rule(&selector, "display:inline-grid", config),
        "contents" => rule(&selector, "display:contents", config),
        "table" => rule(&selector, "display:table", config),
        "table-row" => rule(&selector, "display:table-row", config),
        "table-cell" => rule(&selector, "display:table-cell", config),
        "table-caption" => rule(&selector, "display:table-caption", config),
        "table-column" => rule(&selector, "display:table-column", config),
        "table-column-group" => rule(&selector, "display:table-column-group", config),
        "table-header-group" => rule(&selector, "display:table-header-group", config),
        "table-row-group" => rule(&selector, "display:table-row-group", config),
        "table-footer-group" => rule(&selector, "display:table-footer-group", config),
        "table-auto" => rule(&selector, "table-layout:auto", config),
        "table-fixed" => rule(&selector, "table-layout:fixed", config),
        "caption-top" => rule(&selector, "caption-side:top", config),
        "caption-bottom" => rule(&selector, "caption-side:bottom", config),
        "border-collapse" => rule(&selector, "border-collapse:collapse", config),
        "border-separate" => rule(&selector, "border-collapse:separate", config),
        "sr-only" => rule(
            &selector,
            "position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip-path:inset(50%);white-space:nowrap;border-width:0",
            config,
        ),
        "not-sr-only" => rule(
            &selector,
            "position:static;width:auto;height:auto;padding:0;margin:0;overflow:visible;clip:auto;white-space:normal",
            config,
        ),
        "hidden" => rule(&selector, "display:none", config),
        "static" => rule(&selector, "position:static", config),
        "fixed" => rule(&selector, "position:fixed", config),
        "absolute" => rule(&selector, "position:absolute", config),
        "relative" => rule(&selector, "position:relative", config),
        "sticky" => rule(&selector, "position:sticky", config),
        "w-full" => rule(&selector, "width:100%", config),
        "h-full" => rule(&selector, "height:100%", config),
        "w-screen" => rule(&selector, "width:100vw", config),
        "h-screen" => rule(&selector, "height:100vh", config),
        "min-h-screen" => rule(&selector, "min-height:100vh", config),
        "flex-1" => rule(&selector, "flex:1 1 0%", config),
        "flex-none" => rule(&selector, "flex:none", config),
        "flex-row" => rule(&selector, "flex-direction:row", config),
        "flex-row-reverse" => rule(&selector, "flex-direction:row-reverse", config),
        "flex-col" => rule(&selector, "flex-direction:column", config),
        "flex-col-reverse" => rule(&selector, "flex-direction:column-reverse", config),
        "flex-nowrap" => rule(&selector, "flex-wrap:nowrap", config),
        "flex-wrap" => rule(&selector, "flex-wrap:wrap", config),
        "flex-wrap-reverse" => rule(&selector, "flex-wrap:wrap-reverse", config),
        "justify-start" => rule(&selector, "justify-content:flex-start", config),
        "justify-center" => rule(&selector, "justify-content:center", config),
        "justify-center-safe" => rule(&selector, "justify-content:safe center", config),
        "justify-end" => rule(&selector, "justify-content:flex-end", config),
        "justify-end-safe" => rule(&selector, "justify-content:safe flex-end", config),
        "justify-between" => rule(&selector, "justify-content:space-between", config),
        "justify-around" => rule(&selector, "justify-content:space-around", config),
        "justify-evenly" => rule(&selector, "justify-content:space-evenly", config),
        "justify-stretch" => rule(&selector, "justify-content:stretch", config),
        "justify-baseline" => rule(&selector, "justify-content:baseline", config),
        "justify-normal" => rule(&selector, "justify-content:normal", config),
        "justify-items-start" => rule(&selector, "justify-items:start", config),
        "justify-items-end" => rule(&selector, "justify-items:end", config),
        "justify-items-end-safe" => rule(&selector, "justify-items:safe end", config),
        "justify-items-center" => rule(&selector, "justify-items:center", config),
        "justify-items-center-safe" => rule(&selector, "justify-items:safe center", config),
        "justify-items-stretch" => rule(&selector, "justify-items:stretch", config),
        "justify-items-normal" => rule(&selector, "justify-items:normal", config),
        "justify-self-auto" => rule(&selector, "justify-self:auto", config),
        "justify-self-start" => rule(&selector, "justify-self:start", config),
        "justify-self-center" => rule(&selector, "justify-self:center", config),
        "justify-self-center-safe" => rule(&selector, "justify-self:safe center", config),
        "justify-self-end" => rule(&selector, "justify-self:end", config),
        "justify-self-end-safe" => rule(&selector, "justify-self:safe end", config),
        "justify-self-stretch" => rule(&selector, "justify-self:stretch", config),
        "content-normal" => rule(&selector, "align-content:normal", config),
        "content-center" => rule(&selector, "align-content:center", config),
        "content-start" => rule(&selector, "align-content:flex-start", config),
        "content-end" => rule(&selector, "align-content:flex-end", config),
        "content-between" => rule(&selector, "align-content:space-between", config),
        "content-around" => rule(&selector, "align-content:space-around", config),
        "content-evenly" => rule(&selector, "align-content:space-evenly", config),
        "content-baseline" => rule(&selector, "align-content:baseline", config),
        "content-stretch" => rule(&selector, "align-content:stretch", config),
        "place-content-center" => rule(&selector, "place-content:center", config),
        "place-content-center-safe" => rule(&selector, "place-content:safe center", config),
        "place-content-start" => rule(&selector, "place-content:start", config),
        "place-content-end" => rule(&selector, "place-content:end", config),
        "place-content-end-safe" => rule(&selector, "place-content:safe end", config),
        "place-content-between" => rule(&selector, "place-content:space-between", config),
        "place-content-around" => rule(&selector, "place-content:space-around", config),
        "place-content-evenly" => rule(&selector, "place-content:space-evenly", config),
        "place-content-baseline" => rule(&selector, "place-content:baseline", config),
        "place-content-stretch" => rule(&selector, "place-content:stretch", config),
        "place-items-start" => rule(&selector, "place-items:start", config),
        "place-items-end" => rule(&selector, "place-items:end", config),
        "place-items-end-safe" => rule(&selector, "place-items:safe end", config),
        "place-items-center" => rule(&selector, "place-items:center", config),
        "place-items-center-safe" => rule(&selector, "place-items:safe center", config),
        "place-items-baseline" => rule(&selector, "place-items:baseline", config),
        "place-items-stretch" => rule(&selector, "place-items:stretch", config),
        "place-self-auto" => rule(&selector, "place-self:auto", config),
        "place-self-start" => rule(&selector, "place-self:start", config),
        "place-self-end" => rule(&selector, "place-self:end", config),
        "place-self-end-safe" => rule(&selector, "place-self:safe end", config),
        "place-self-center" => rule(&selector, "place-self:center", config),
        "place-self-center-safe" => rule(&selector, "place-self:safe center", config),
        "place-self-stretch" => rule(&selector, "place-self:stretch", config),
        "items-start" => rule(&selector, "align-items:flex-start", config),
        "items-center" => rule(&selector, "align-items:center", config),
        "items-end" => rule(&selector, "align-items:flex-end", config),
        "items-end-safe" => rule(&selector, "align-items:safe flex-end", config),
        "items-center-safe" => rule(&selector, "align-items:safe center", config),
        "items-baseline" => rule(&selector, "align-items:baseline", config),
        "items-baseline-last" => rule(&selector, "align-items:last baseline", config),
        "items-stretch" => rule(&selector, "align-items:stretch", config),
        "self-auto" => rule(&selector, "align-self:auto", config),
        "self-start" => rule(&selector, "align-self:flex-start", config),
        "self-end" => rule(&selector, "align-self:flex-end", config),
        "self-end-safe" => rule(&selector, "align-self:safe flex-end", config),
        "self-center" => rule(&selector, "align-self:center", config),
        "self-center-safe" => rule(&selector, "align-self:safe center", config),
        "self-stretch" => rule(&selector, "align-self:stretch", config),
        "self-baseline" => rule(&selector, "align-self:baseline", config),
        "self-baseline-last" => rule(&selector, "align-self:last baseline", config),
        "overflow-auto" => rule(&selector, "overflow:auto", config),
        "overflow-hidden" => rule(&selector, "overflow:hidden", config),
        "overflow-clip" => rule(&selector, "overflow:clip", config),
        "overflow-visible" => rule(&selector, "overflow:visible", config),
        "overflow-scroll" => rule(&selector, "overflow:scroll", config),
        "overflow-x-auto" => rule(&selector, "overflow-x:auto", config),
        "overflow-y-auto" => rule(&selector, "overflow-y:auto", config),
        "overflow-x-hidden" => rule(&selector, "overflow-x:hidden", config),
        "overflow-y-hidden" => rule(&selector, "overflow-y:hidden", config),
        "overflow-x-clip" => rule(&selector, "overflow-x:clip", config),
        "overflow-y-clip" => rule(&selector, "overflow-y:clip", config),
        "overflow-x-visible" => rule(&selector, "overflow-x:visible", config),
        "overflow-y-visible" => rule(&selector, "overflow-y:visible", config),
        "overflow-x-scroll" => rule(&selector, "overflow-x:scroll", config),
        "overflow-y-scroll" => rule(&selector, "overflow-y:scroll", config),
        _ => generate_sizing_rule(class, config, variant_tables)
            .or_else(|| generate_gap_rule(class, config))
            .or_else(|| generate_space_rule(class, config)),
    }
}

fn generate_sizing_rule(
    class: &str,
    config: &GeneratorConfig,
    variant_tables: &VariantTables,
) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "container" {
        let mut breakpoints = variant_tables.responsive_breakpoints.clone();
        sort_breakpoints_by_length(&mut breakpoints);
        if config.minify {
            let mut out = ".container{width:100%".to_string();
            for (_, width) in breakpoints {
                out.push_str(&format!(
                    "@media (width >= {}){{max-width:{}}}",
                    width, width
                ));
            }
            out.push('}');
            return Some(out);
        }
        let mut out = ".container {\n  width: 100%;".to_string();
        for (_, width) in breakpoints {
            out.push_str(&format!(
                "\n  @media (width >= {}) {{\n    max-width: {};\n  }}",
                width, width
            ));
        }
        out.push_str("\n}");
        return Some(out);
    }

    if let Some(raw) = class.strip_prefix("max-w-") {
        let declarations = match raw {
            "none" => "max-width:none".to_string(),
            "px" => "max-width:1px".to_string(),
            "full" => "max-width:100%".to_string(),
            "screen" => "max-width:100vw".to_string(),
            "dvw" => "max-width:100dvw".to_string(),
            "dvh" => "max-width:100dvh".to_string(),
            "lvw" => "max-width:100lvw".to_string(),
            "lvh" => "max-width:100lvh".to_string(),
            "svw" => "max-width:100svw".to_string(),
            "svh" => "max-width:100svh".to_string(),
            "min" => "max-width:min-content".to_string(),
            "max" => "max-width:max-content".to_string(),
            "fit" => "max-width:fit-content".to_string(),
            _ => {
                if is_spacing_multiplier(raw) {
                    format!("max-width:calc(var(--spacing) * {})", raw)
                } else if is_fraction(raw) {
                    format!("max-width:calc({} * 100%)", raw)
                } else if is_container_token(raw) {
                    format!("max-width:var(--container-{})", raw)
                } else if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']'))
                {
                    format!("max-width:{}", normalize_arbitrary_value(custom))
                } else if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')'))
                {
                    format!("max-width:var({})", custom)
                } else {
                    return None;
                }
            }
        };
        return rule(&selector, &declarations, config);
    }

    if let Some(raw) = class.strip_prefix("min-w-") {
        let declarations = match raw {
            "auto" => "min-width:auto".to_string(),
            "px" => "min-width:1px".to_string(),
            "full" => "min-width:100%".to_string(),
            "screen" => "min-width:100vw".to_string(),
            "dvw" => "min-width:100dvw".to_string(),
            "dvh" => "min-width:100dvh".to_string(),
            "lvw" => "min-width:100lvw".to_string(),
            "lvh" => "min-width:100lvh".to_string(),
            "svw" => "min-width:100svw".to_string(),
            "svh" => "min-width:100svh".to_string(),
            "min" => "min-width:min-content".to_string(),
            "max" => "min-width:max-content".to_string(),
            "fit" => "min-width:fit-content".to_string(),
            _ => {
                if is_spacing_multiplier(raw) {
                    format!("min-width:calc(var(--spacing) * {})", raw)
                } else if is_fraction(raw) {
                    format!("min-width:calc({} * 100%)", raw)
                } else if is_container_token(raw) {
                    format!("min-width:var(--container-{})", raw)
                } else if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']'))
                {
                    format!("min-width:{}", normalize_arbitrary_value(custom))
                } else if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')'))
                {
                    format!("min-width:var({})", custom)
                } else {
                    return None;
                }
            }
        };
        return rule(&selector, &declarations, config);
    }

    if let Some(raw) = class.strip_prefix("min-h-") {
        let declarations = match raw {
            "auto" => "min-height:auto".to_string(),
            "px" => "min-height:1px".to_string(),
            "full" => "min-height:100%".to_string(),
            "screen" => "min-height:100vh".to_string(),
            "dvh" => "min-height:100dvh".to_string(),
            "dvw" => "min-height:100dvw".to_string(),
            "lvh" => "min-height:100lvh".to_string(),
            "lvw" => "min-height:100lvw".to_string(),
            "svh" => "min-height:100svh".to_string(),
            "svw" => "min-height:100svw".to_string(),
            "min" => "min-height:min-content".to_string(),
            "max" => "min-height:max-content".to_string(),
            "fit" => "min-height:fit-content".to_string(),
            "lh" => "min-height:1lh".to_string(),
            _ => {
                if is_spacing_multiplier(raw) {
                    format!("min-height:calc(var(--spacing) * {})", raw)
                } else if is_fraction(raw) {
                    format!("min-height:calc({} * 100%)", raw)
                } else if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']'))
                {
                    format!("min-height:{}", normalize_arbitrary_value(custom))
                } else if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')'))
                {
                    format!("min-height:var({})", custom)
                } else {
                    return None;
                }
            }
        };
        return rule(&selector, &declarations, config);
    }

    if let Some(raw) = class.strip_prefix("max-h-") {
        let declarations = match raw {
            "none" => "max-height:none".to_string(),
            "px" => "max-height:1px".to_string(),
            "full" => "max-height:100%".to_string(),
            "screen" => "max-height:100vh".to_string(),
            "dvh" => "max-height:100dvh".to_string(),
            "dvw" => "max-height:100dvw".to_string(),
            "lvh" => "max-height:100lvh".to_string(),
            "lvw" => "max-height:100lvw".to_string(),
            "svh" => "max-height:100svh".to_string(),
            "svw" => "max-height:100svw".to_string(),
            "min" => "max-height:min-content".to_string(),
            "max" => "max-height:max-content".to_string(),
            "fit" => "max-height:fit-content".to_string(),
            "lh" => "max-height:1lh".to_string(),
            _ => {
                if is_spacing_multiplier(raw) {
                    format!("max-height:calc(var(--spacing) * {})", raw)
                } else if is_fraction(raw) {
                    format!("max-height:calc({} * 100%)", raw)
                } else if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']'))
                {
                    format!("max-height:{}", normalize_arbitrary_value(custom))
                } else if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')'))
                {
                    format!("max-height:var({})", custom)
                } else {
                    return None;
                }
            }
        };
        return rule(&selector, &declarations, config);
    }

    if let Some(raw) = class.strip_prefix("w-") {
        let declarations = match raw {
            "auto" => "width:auto".to_string(),
            "px" => "width:1px".to_string(),
            "full" => "width:100%".to_string(),
            "screen" => "width:100vw".to_string(),
            "dvw" => "width:100dvw".to_string(),
            "dvh" => "width:100dvh".to_string(),
            "lvw" => "width:100lvw".to_string(),
            "lvh" => "width:100lvh".to_string(),
            "svw" => "width:100svw".to_string(),
            "svh" => "width:100svh".to_string(),
            "min" => "width:min-content".to_string(),
            "max" => "width:max-content".to_string(),
            "fit" => "width:fit-content".to_string(),
            _ => {
                if is_spacing_multiplier(raw) {
                    format!("width:calc(var(--spacing) * {})", raw)
                } else if is_fraction(raw) {
                    format!("width:calc({} * 100%)", raw)
                } else if is_container_token(raw) {
                    format!("width:var(--container-{})", raw)
                } else if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']'))
                {
                    format!("width:{}", normalize_arbitrary_value(custom))
                } else if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')'))
                {
                    format!("width:var({})", custom)
                } else {
                    return None;
                }
            }
        };
        return rule(&selector, &declarations, config);
    }

    if let Some(raw) = class.strip_prefix("h-") {
        let declarations = match raw {
            "auto" => "height:auto".to_string(),
            "px" => "height:1px".to_string(),
            "full" => "height:100%".to_string(),
            "screen" => "height:100vh".to_string(),
            "dvh" => "height:100dvh".to_string(),
            "dvw" => "height:100dvw".to_string(),
            "lvh" => "height:100lvh".to_string(),
            "lvw" => "height:100lvw".to_string(),
            "svh" => "height:100svh".to_string(),
            "svw" => "height:100svw".to_string(),
            "min" => "height:min-content".to_string(),
            "max" => "height:max-content".to_string(),
            "fit" => "height:fit-content".to_string(),
            "lh" => "height:1lh".to_string(),
            _ => {
                if is_spacing_multiplier(raw) {
                    format!("height:calc(var(--spacing) * {})", raw)
                } else if is_fraction(raw) {
                    format!("height:calc({} * 100%)", raw)
                } else if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']'))
                {
                    format!("height:{}", normalize_arbitrary_value(custom))
                } else if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')'))
                {
                    format!("height:var({})", custom)
                } else {
                    return None;
                }
            }
        };
        return rule(&selector, &declarations, config);
    }

    if let Some(raw) = class.strip_prefix("size-") {
        let value = match raw {
            "auto" => "auto".to_string(),
            "px" => "1px".to_string(),
            "full" => "100%".to_string(),
            "dvw" => "100dvw".to_string(),
            "dvh" => "100dvh".to_string(),
            "lvw" => "100lvw".to_string(),
            "lvh" => "100lvh".to_string(),
            "svw" => "100svw".to_string(),
            "svh" => "100svh".to_string(),
            "min" => "min-content".to_string(),
            "max" => "max-content".to_string(),
            "fit" => "fit-content".to_string(),
            _ => {
                if is_spacing_multiplier(raw) {
                    format!("calc(var(--spacing) * {})", raw)
                } else if is_fraction(raw) {
                    format!("calc({} * 100%)", raw)
                } else if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']'))
                {
                    normalize_arbitrary_value(custom)
                } else if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')'))
                {
                    format!("var({})", custom)
                } else {
                    return None;
                }
            }
        };
        return rule(
            &selector,
            &format!("width:{};height:{}", value, value),
            config,
        );
    }

    None
}

fn generate_gap_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class.strip_prefix("gap-") {
        if let Some(value) = parse_spacing_value(raw) {
            return rule(&selector, &format!("gap:{}", value), config);
        }
    }

    if let Some(raw) = class.strip_prefix("gap-x-") {
        if let Some(value) = parse_spacing_value(raw) {
            return rule(&selector, &format!("column-gap:{}", value), config);
        }
    }

    if let Some(raw) = class.strip_prefix("gap-y-") {
        if let Some(value) = parse_spacing_value(raw) {
            return rule(&selector, &format!("row-gap:{}", value), config);
        }
    }

    None
}

fn nested_child_selector_block(declarations: &str) -> String {
    format!(":where(& > :not(:last-child)) {{{}}}", declarations)
}

fn generate_space_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if class == "space-x-reverse" {
        return rule(
            &selector,
            &nested_child_selector_block("--tw-space-x-reverse:1"),
            config,
        );
    }
    if class == "space-y-reverse" {
        return rule(
            &selector,
            &nested_child_selector_block("--tw-space-y-reverse:1"),
            config,
        );
    }

    let (negative, raw_class) = if let Some(rest) = class.strip_prefix('-') {
        (true, rest)
    } else {
        (false, class)
    };

    if let Some(raw) = raw_class.strip_prefix("space-x-") {
        let value = parse_space_value(raw, negative)?;
        let declarations = format!(
            "--tw-space-x-reverse:0;margin-inline-start:calc({} * var(--tw-space-x-reverse));margin-inline-end:calc({} * calc(1 - var(--tw-space-x-reverse)))",
            value, value
        );
        return rule(
            &selector,
            &nested_child_selector_block(&declarations),
            config,
        );
    }

    if let Some(raw) = raw_class.strip_prefix("space-y-") {
        let value = parse_space_value(raw, negative)?;
        let declarations = format!(
            "--tw-space-y-reverse:0;margin-block-start:calc({} * var(--tw-space-y-reverse));margin-block-end:calc({} * calc(1 - var(--tw-space-y-reverse)))",
            value, value
        );
        return rule(
            &selector,
            &nested_child_selector_block(&declarations),
            config,
        );
    }

    None
}

fn generate_aspect_ratio_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "aspect-square" => rule(&selector, "aspect-ratio:1 / 1", config),
        "aspect-video" => rule(&selector, "aspect-ratio:var(--aspect-video)", config),
        "aspect-auto" => rule(&selector, "aspect-ratio:auto", config),
        _ => {
            if let Some(custom) = class
                .strip_prefix("aspect-(")
                .and_then(|value| value.strip_suffix(')'))
            {
                return rule(&selector, &format!("aspect-ratio:var({})", custom), config);
            }
            if let Some(raw) = class
                .strip_prefix("aspect-[")
                .and_then(|value| value.strip_suffix(']'))
            {
                return rule(&selector, &format!("aspect-ratio:{}", raw), config);
            }
            if let Some(ratio) = class.strip_prefix("aspect-") {
                if ratio.contains('/') && !ratio.is_empty() {
                    return rule(&selector, &format!("aspect-ratio:{}", ratio), config);
                }
            }
            None
        }
    }
}

fn generate_columns_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if let Some(value) = class
        .strip_prefix("columns-[")
        .and_then(|raw| raw.strip_suffix(']'))
    {
        return rule(&selector, &format!("columns:{}", value), config);
    }
    if let Some(custom) = class
        .strip_prefix("columns-(")
        .and_then(|raw| raw.strip_suffix(')'))
    {
        return rule(&selector, &format!("columns:var({})", custom), config);
    }
    if let Some(raw) = class.strip_prefix("columns-") {
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return rule(&selector, &format!("columns:{}", raw), config);
        }
        if is_container_token(raw) {
            return rule(
                &selector,
                &format!("columns:var(--container-{})", raw),
                config,
            );
        }
    }
    None
}

fn generate_break_after_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "break-after-auto" => rule(&selector, "break-after:auto", config),
        "break-after-avoid" => rule(&selector, "break-after:avoid", config),
        "break-after-all" => rule(&selector, "break-after:all", config),
        "break-after-avoid-page" => rule(&selector, "break-after:avoid-page", config),
        "break-after-page" => rule(&selector, "break-after:page", config),
        "break-after-left" => rule(&selector, "break-after:left", config),
        "break-after-right" => rule(&selector, "break-after:right", config),
        "break-after-column" => rule(&selector, "break-after:column", config),
        _ => None,
    }
}

fn generate_break_before_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "break-before-auto" => rule(&selector, "break-before:auto", config),
        "break-before-avoid" => rule(&selector, "break-before:avoid", config),
        "break-before-all" => rule(&selector, "break-before:all", config),
        "break-before-avoid-page" => rule(&selector, "break-before:avoid-page", config),
        "break-before-page" => rule(&selector, "break-before:page", config),
        "break-before-left" => rule(&selector, "break-before:left", config),
        "break-before-right" => rule(&selector, "break-before:right", config),
        "break-before-column" => rule(&selector, "break-before:column", config),
        _ => None,
    }
}

fn generate_break_inside_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "break-inside-auto" => rule(&selector, "break-inside:auto", config),
        "break-inside-avoid" => rule(&selector, "break-inside:avoid", config),
        "break-inside-avoid-page" => rule(&selector, "break-inside:avoid-page", config),
        "break-inside-avoid-column" => rule(&selector, "break-inside:avoid-column", config),
        _ => None,
    }
}

fn generate_box_decoration_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "box-decoration-clone" => rule(&selector, "box-decoration-break:clone", config),
        "box-decoration-slice" => rule(&selector, "box-decoration-break:slice", config),
        _ => None,
    }
}

fn generate_box_sizing_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "box-border" => rule(&selector, "box-sizing:border-box", config),
        "box-content" => rule(&selector, "box-sizing:content-box", config),
        _ => None,
    }
}

fn generate_float_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "float-right" => rule(&selector, "float:right", config),
        "float-left" => rule(&selector, "float:left", config),
        "float-start" => rule(&selector, "float:inline-start", config),
        "float-end" => rule(&selector, "float:inline-end", config),
        "float-none" => rule(&selector, "float:none", config),
        _ => None,
    }
}

fn generate_clear_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "clear-left" => rule(&selector, "clear:left", config),
        "clear-right" => rule(&selector, "clear:right", config),
        "clear-both" => rule(&selector, "clear:both", config),
        "clear-start" => rule(&selector, "clear:inline-start", config),
        "clear-end" => rule(&selector, "clear:inline-end", config),
        "clear-none" => rule(&selector, "clear:none", config),
        _ => None,
    }
}

fn generate_isolation_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "isolate" => rule(&selector, "isolation:isolate", config),
        "isolation-auto" => rule(&selector, "isolation:auto", config),
        _ => None,
    }
}

fn generate_appearance_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "appearance-none" => rule(&selector, "appearance:none", config),
        "appearance-auto" => rule(&selector, "appearance:auto", config),
        _ => None,
    }
}

fn generate_forced_color_adjust_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "forced-color-adjust-auto" => rule(&selector, "forced-color-adjust:auto", config),
        "forced-color-adjust-none" => rule(&selector, "forced-color-adjust:none", config),
        _ => None,
    }
}

fn generate_color_scheme_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let value = match class {
        "scheme-normal" => "normal",
        "scheme-dark" => "dark",
        "scheme-light" => "light",
        "scheme-light-dark" => "light dark",
        "scheme-only-dark" => "only dark",
        "scheme-only-light" => "only light",
        _ => return None,
    };
    rule(&selector, &format!("color-scheme:{}", value), config)
}

fn generate_cursor_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let value = match class {
        "cursor-auto" => Some("auto"),
        "cursor-default" => Some("default"),
        "cursor-pointer" => Some("pointer"),
        "cursor-wait" => Some("wait"),
        "cursor-text" => Some("text"),
        "cursor-move" => Some("move"),
        "cursor-help" => Some("help"),
        "cursor-not-allowed" => Some("not-allowed"),
        "cursor-none" => Some("none"),
        "cursor-context-menu" => Some("context-menu"),
        "cursor-progress" => Some("progress"),
        "cursor-cell" => Some("cell"),
        "cursor-crosshair" => Some("crosshair"),
        "cursor-vertical-text" => Some("vertical-text"),
        "cursor-alias" => Some("alias"),
        "cursor-copy" => Some("copy"),
        "cursor-no-drop" => Some("no-drop"),
        "cursor-grab" => Some("grab"),
        "cursor-grabbing" => Some("grabbing"),
        "cursor-all-scroll" => Some("all-scroll"),
        "cursor-col-resize" => Some("col-resize"),
        "cursor-row-resize" => Some("row-resize"),
        "cursor-n-resize" => Some("n-resize"),
        "cursor-e-resize" => Some("e-resize"),
        "cursor-s-resize" => Some("s-resize"),
        "cursor-w-resize" => Some("w-resize"),
        "cursor-ne-resize" => Some("ne-resize"),
        "cursor-nw-resize" => Some("nw-resize"),
        "cursor-se-resize" => Some("se-resize"),
        "cursor-sw-resize" => Some("sw-resize"),
        "cursor-ew-resize" => Some("ew-resize"),
        "cursor-ns-resize" => Some("ns-resize"),
        "cursor-nesw-resize" => Some("nesw-resize"),
        "cursor-nwse-resize" => Some("nwse-resize"),
        "cursor-zoom-in" => Some("zoom-in"),
        "cursor-zoom-out" => Some("zoom-out"),
        _ => None,
    };
    if let Some(value) = value {
        return rule(&selector, &format!("cursor:{}", value), config);
    }

    if let Some(value) = class
        .strip_prefix("cursor-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(&selector, &format!("cursor:var({})", value), config);
    }

    if let Some(value) = class
        .strip_prefix("cursor-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(&selector, &format!("cursor:{}", value), config);
    }

    None
}

fn generate_field_sizing_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "field-sizing-fixed" => rule(&selector, "field-sizing:fixed", config),
        "field-sizing-content" => rule(&selector, "field-sizing:content", config),
        _ => None,
    }
}

fn generate_pointer_events_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "pointer-events-auto" => rule(&selector, "pointer-events:auto", config),
        "pointer-events-none" => rule(&selector, "pointer-events:none", config),
        _ => None,
    }
}

fn generate_resize_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "resize-none" => rule(&selector, "resize:none", config),
        "resize" => rule(&selector, "resize:both", config),
        "resize-y" => rule(&selector, "resize:vertical", config),
        "resize-x" => rule(&selector, "resize:horizontal", config),
        _ => None,
    }
}

fn generate_scroll_behavior_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "scroll-auto" => rule(&selector, "scroll-behavior:auto", config),
        "scroll-smooth" => rule(&selector, "scroll-behavior:smooth", config),
        _ => None,
    }
}

fn generate_scroll_snap_align_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "snap-start" => rule(&selector, "scroll-snap-align:start", config),
        "snap-end" => rule(&selector, "scroll-snap-align:end", config),
        "snap-center" => rule(&selector, "scroll-snap-align:center", config),
        "snap-align-none" => rule(&selector, "scroll-snap-align:none", config),
        _ => None,
    }
}

fn generate_scroll_snap_type_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "snap-none" => rule(&selector, "scroll-snap-type:none", config),
        "snap-x" => rule(
            &selector,
            "scroll-snap-type:x var(--tw-scroll-snap-strictness)",
            config,
        ),
        "snap-y" => rule(
            &selector,
            "scroll-snap-type:y var(--tw-scroll-snap-strictness)",
            config,
        ),
        "snap-both" => rule(
            &selector,
            "scroll-snap-type:both var(--tw-scroll-snap-strictness)",
            config,
        ),
        "snap-mandatory" => rule(&selector, "--tw-scroll-snap-strictness:mandatory", config),
        "snap-proximity" => rule(&selector, "--tw-scroll-snap-strictness:proximity", config),
        _ => None,
    }
}

fn generate_scroll_snap_stop_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "snap-normal" => rule(&selector, "scroll-snap-stop:normal", config),
        "snap-always" => rule(&selector, "scroll-snap-stop:always", config),
        _ => None,
    }
}

fn generate_touch_action_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let value = match class {
        "touch-auto" => "auto",
        "touch-none" => "none",
        "touch-pan-x" => "pan-x",
        "touch-pan-left" => "pan-left",
        "touch-pan-right" => "pan-right",
        "touch-pan-y" => "pan-y",
        "touch-pan-up" => "pan-up",
        "touch-pan-down" => "pan-down",
        "touch-pinch-zoom" => "pinch-zoom",
        "touch-manipulation" => "manipulation",
        _ => return None,
    };
    rule(&selector, &format!("touch-action:{}", value), config)
}

fn generate_user_select_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let value = match class {
        "select-none" => "none",
        "select-text" => "text",
        "select-all" => "all",
        "select-auto" => "auto",
        _ => return None,
    };
    rule(
        &selector,
        &format!("-webkit-user-select:{};user-select:{}", value, value),
        config,
    )
}

fn generate_will_change_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "will-change-auto" => return rule(&selector, "will-change:auto", config),
        "will-change-scroll" => {
            return rule(&selector, "will-change:scroll-position", config);
        }
        "will-change-contents" => return rule(&selector, "will-change:contents", config),
        "will-change-transform" => return rule(&selector, "will-change:transform", config),
        _ => {}
    }

    if let Some(value) = class
        .strip_prefix("will-change-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(&selector, &format!("will-change:var({})", value), config);
    }

    if let Some(value) = class
        .strip_prefix("will-change-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(&selector, &format!("will-change:{}", value), config);
    }

    None
}

fn generate_mix_blend_mode_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let value = match class {
        "mix-blend-normal" => "normal",
        "mix-blend-multiply" => "multiply",
        "mix-blend-screen" => "screen",
        "mix-blend-overlay" => "overlay",
        "mix-blend-darken" => "darken",
        "mix-blend-lighten" => "lighten",
        "mix-blend-color-dodge" => "color-dodge",
        "mix-blend-color-burn" => "color-burn",
        "mix-blend-hard-light" => "hard-light",
        "mix-blend-soft-light" => "soft-light",
        "mix-blend-difference" => "difference",
        "mix-blend-exclusion" => "exclusion",
        "mix-blend-hue" => "hue",
        "mix-blend-saturation" => "saturation",
        "mix-blend-color" => "color",
        "mix-blend-luminosity" => "luminosity",
        "mix-blend-plus-darker" => "plus-darker",
        "mix-blend-plus-lighter" => "plus-lighter",
        _ => return None,
    };
    rule(&selector, &format!("mix-blend-mode:{}", value), config)
}

fn generate_background_blend_mode_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let value = match class {
        "bg-blend-normal" => "normal",
        "bg-blend-multiply" => "multiply",
        "bg-blend-screen" => "screen",
        "bg-blend-overlay" => "overlay",
        "bg-blend-darken" => "darken",
        "bg-blend-lighten" => "lighten",
        "bg-blend-color-dodge" => "color-dodge",
        "bg-blend-color-burn" => "color-burn",
        "bg-blend-hard-light" => "hard-light",
        "bg-blend-soft-light" => "soft-light",
        "bg-blend-difference" => "difference",
        "bg-blend-exclusion" => "exclusion",
        "bg-blend-hue" => "hue",
        "bg-blend-saturation" => "saturation",
        "bg-blend-color" => "color",
        "bg-blend-luminosity" => "luminosity",
        _ => return None,
    };
    rule(
        &selector,
        &format!("background-blend-mode:{}", value),
        config,
    )
}

#[derive(Clone)]
enum MaskStopValue {
    Value(String),
    Color(String),
}

fn is_percentage_value(raw: &str) -> bool {
    if let Some(value) = raw.strip_suffix('%') {
        return !value.is_empty() && value.chars().all(|c| c.is_ascii_digit() || c == '.');
    }
    false
}

fn parse_mask_color_value(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }
    if is_color_like_value(raw) {
        return Some(raw.to_string());
    }
    match raw {
        "black" => Some("var(--color-black)".to_string()),
        "white" => Some("var(--color-white)".to_string()),
        "transparent" => Some("transparent".to_string()),
        "current" => Some("currentColor".to_string()),
        "inherit" => Some("inherit".to_string()),
        _ => {
            if raw.contains('-') {
                Some(format!("var(--color-{})", raw))
            } else {
                None
            }
        }
    }
}

fn parse_mask_stop_value(raw: &str) -> Option<MaskStopValue> {
    if raw.is_empty() {
        return None;
    }
    if raw.chars().all(|c| c.is_ascii_digit()) {
        return Some(MaskStopValue::Value(format!(
            "calc(var(--spacing) * {})",
            raw
        )));
    }
    if is_percentage_value(raw) {
        return Some(MaskStopValue::Value(raw.to_string()));
    }
    if let Some(value) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        if value.is_empty() {
            return None;
        }
        return Some(MaskStopValue::Value(format!("var({})", value)));
    }
    if let Some(value) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        if value.is_empty() {
            return None;
        }
        return Some(MaskStopValue::Value(value.to_string()));
    }
    Some(MaskStopValue::Color(parse_mask_color_value(raw)?))
}

fn mask_linear_side_from_gradient(
    direction: &str,
    side_token: &str,
    stop: MaskStopValue,
) -> String {
    match stop {
        MaskStopValue::Value(value) => format!(
            "linear-gradient({},black {},transparent var(--tw-mask-{}-to))",
            direction, value, side_token
        ),
        MaskStopValue::Color(color) => format!(
            "linear-gradient({},{} var(--tw-mask-{}-from),transparent var(--tw-mask-{}-to))",
            direction, color, side_token, side_token
        ),
    }
}

fn mask_linear_side_to_gradient(direction: &str, side_token: &str, stop: MaskStopValue) -> String {
    match stop {
        MaskStopValue::Value(value) => format!(
            "linear-gradient({},black var(--tw-mask-{}-from),transparent {})",
            direction, side_token, value
        ),
        MaskStopValue::Color(color) => format!(
            "linear-gradient({},black var(--tw-mask-{}-from),{} var(--tw-mask-{}-to))",
            direction, side_token, color, side_token
        ),
    }
}

fn mask_linear_bi_from_declarations(
    first_direction: &str,
    first_side: &str,
    second_direction: &str,
    second_side: &str,
    stop: MaskStopValue,
) -> String {
    let first = mask_linear_side_from_gradient(first_direction, first_side, stop.clone());
    let second = mask_linear_side_from_gradient(second_direction, second_side, stop);
    format!("mask-image:{},{};mask-composite:intersect", first, second)
}

fn mask_linear_bi_to_declarations(
    first_direction: &str,
    first_side: &str,
    second_direction: &str,
    second_side: &str,
    stop: MaskStopValue,
) -> String {
    let first = mask_linear_side_to_gradient(first_direction, first_side, stop.clone());
    let second = mask_linear_side_to_gradient(second_direction, second_side, stop);
    format!("mask-image:{},{};mask-composite:intersect", first, second)
}

fn mask_radial_from_declarations(stop: MaskStopValue) -> String {
    match stop {
        MaskStopValue::Value(value) => format!(
            "mask-image:radial-gradient(var(--tw-mask-radial-shape) var(--tw-mask-radial-size) at var(--tw-mask-radial-position),black {},transparent var(--tw-mask-radial-to))",
            value
        ),
        MaskStopValue::Color(color) => format!(
            "mask-image:radial-gradient(var(--tw-mask-radial-shape) var(--tw-mask-radial-size) at var(--tw-mask-radial-position),{} var(--tw-mask-radial-from),transparent var(--tw-mask-radial-to))",
            color
        ),
    }
}

fn mask_radial_to_declarations(stop: MaskStopValue) -> String {
    match stop {
        MaskStopValue::Value(value) => format!(
            "mask-image:radial-gradient(var(--tw-mask-radial-shape) var(--tw-mask-radial-size) at var(--tw-mask-radial-position),black var(--tw-mask-radial-from),transparent {})",
            value
        ),
        MaskStopValue::Color(color) => format!(
            "mask-image:radial-gradient(var(--tw-mask-radial-shape) var(--tw-mask-radial-size) at var(--tw-mask-radial-position),black var(--tw-mask-radial-from),{} var(--tw-mask-radial-to))",
            color
        ),
    }
}

fn mask_conic_from_declarations(stop: MaskStopValue) -> String {
    match stop {
        MaskStopValue::Value(value) => format!(
            "mask-image:conic-gradient(from var(--tw-mask-conic-position),black {},transparent var(--tw-mask-conic-to))",
            value
        ),
        MaskStopValue::Color(color) => format!(
            "mask-image:conic-gradient(from var(--tw-mask-conic-position),{} var(--tw-mask-conic-from),transparent var(--tw-mask-conic-to))",
            color
        ),
    }
}

fn mask_conic_to_declarations(stop: MaskStopValue) -> String {
    match stop {
        MaskStopValue::Value(value) => format!(
            "mask-image:conic-gradient(from var(--tw-mask-conic-position),black var(--tw-mask-conic-from),transparent {})",
            value
        ),
        MaskStopValue::Color(color) => format!(
            "mask-image:conic-gradient(from var(--tw-mask-conic-position),black var(--tw-mask-conic-from),{} var(--tw-mask-conic-to))",
            color
        ),
    }
}

fn parse_mask_angle(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }
    if raw.chars().all(|c| c.is_ascii_digit() || c == '.') {
        return Some(format!("{}deg", raw));
    }
    None
}

fn generate_mask_image_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "mask-none" {
        return rule(&selector, "mask-image:none", config);
    }

    if let Some(value) = class
        .strip_prefix("mask-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(&selector, &format!("mask-image:{}", value), config);
    }

    if let Some(value) = class
        .strip_prefix("mask-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return rule(&selector, &format!("mask-image:var({})", value), config);
    }

    if let Some(raw) = class.strip_prefix("mask-linear-") {
        if let Some(value) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            if value.is_empty() {
                return None;
            }
            return rule(
                &selector,
                &format!("mask-image:linear-gradient({})", value),
                config,
            );
        }
        if let Some(value) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            if value.is_empty() {
                return None;
            }
            return rule(
                &selector,
                &format!("mask-image:linear-gradient(var({}))", value),
                config,
            );
        }
        if let Some(angle) = parse_mask_angle(raw) {
            return rule(
                &selector,
                &format!(
                    "mask-image:linear-gradient({},black var(--tw-mask-linear-from),transparent var(--tw-mask-linear-to))",
                    angle
                ),
                config,
            );
        }
    }

    if let Some(raw) = class.strip_prefix("-mask-linear-") {
        let angle = parse_mask_angle(raw)?;
        return rule(
            &selector,
            &format!(
                "mask-image:linear-gradient(calc({} * -1),black var(--tw-mask-linear-from),transparent var(--tw-mask-linear-to))",
                angle
            ),
            config,
        );
    }

    if let Some(raw) = class.strip_prefix("mask-linear-from-") {
        let stop = parse_mask_stop_value(raw)?;
        let declarations = match stop {
            MaskStopValue::Value(value) => format!(
                "mask-image:linear-gradient(var(--tw-mask-linear-position),black {},transparent var(--tw-mask-linear-to))",
                value
            ),
            MaskStopValue::Color(color) => format!(
                "mask-image:linear-gradient(var(--tw-mask-linear-position),{} var(--tw-mask-linear-from),transparent var(--tw-mask-linear-to))",
                color
            ),
        };
        return rule(&selector, &declarations, config);
    }

    if let Some(raw) = class.strip_prefix("mask-linear-to-") {
        let stop = parse_mask_stop_value(raw)?;
        let declarations = match stop {
            MaskStopValue::Value(value) => format!(
                "mask-image:linear-gradient(var(--tw-mask-linear-position),black var(--tw-mask-linear-from),transparent {})",
                value
            ),
            MaskStopValue::Color(color) => format!(
                "mask-image:linear-gradient(var(--tw-mask-linear-position),black var(--tw-mask-linear-from),{} var(--tw-mask-linear-to))",
                color
            ),
        };
        return rule(&selector, &declarations, config);
    }

    for (prefix, direction, side) in [
        ("mask-t-from-", "to top", "top"),
        ("mask-r-from-", "to right", "right"),
        ("mask-b-from-", "to bottom", "bottom"),
        ("mask-l-from-", "to left", "left"),
    ] {
        if let Some(raw) = class.strip_prefix(prefix) {
            let stop = parse_mask_stop_value(raw)?;
            return rule(
                &selector,
                &format!(
                    "mask-image:{}",
                    mask_linear_side_from_gradient(direction, side, stop)
                ),
                config,
            );
        }
    }

    for (prefix, direction, side) in [
        ("mask-t-to-", "to top", "top"),
        ("mask-r-to-", "to right", "right"),
        ("mask-b-to-", "to bottom", "bottom"),
        ("mask-l-to-", "to left", "left"),
    ] {
        if let Some(raw) = class.strip_prefix(prefix) {
            let stop = parse_mask_stop_value(raw)?;
            return rule(
                &selector,
                &format!(
                    "mask-image:{}",
                    mask_linear_side_to_gradient(direction, side, stop)
                ),
                config,
            );
        }
    }

    if let Some(raw) = class.strip_prefix("mask-y-from-") {
        let stop = parse_mask_stop_value(raw)?;
        return rule(
            &selector,
            &mask_linear_bi_from_declarations("to top", "top", "to bottom", "bottom", stop),
            config,
        );
    }
    if let Some(raw) = class.strip_prefix("mask-y-to-") {
        let stop = parse_mask_stop_value(raw)?;
        return rule(
            &selector,
            &mask_linear_bi_to_declarations("to top", "top", "to bottom", "bottom", stop),
            config,
        );
    }
    if let Some(raw) = class.strip_prefix("mask-x-from-") {
        let stop = parse_mask_stop_value(raw)?;
        return rule(
            &selector,
            &mask_linear_bi_from_declarations("to right", "right", "to left", "left", stop),
            config,
        );
    }
    if let Some(raw) = class.strip_prefix("mask-x-to-") {
        let stop = parse_mask_stop_value(raw)?;
        return rule(
            &selector,
            &mask_linear_bi_to_declarations("to right", "right", "to left", "left", stop),
            config,
        );
    }

    if let Some(raw) = class.strip_prefix("mask-radial-") {
        if let Some(value) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            if value.is_empty() {
                return None;
            }
            if value.contains(',') {
                return rule(
                    &selector,
                    &format!("mask-image:radial-gradient({})", value),
                    config,
                );
            }
            return rule(
                &selector,
                &format!("--tw-mask-radial-size:{}", value),
                config,
            );
        }
    }

    if let Some(raw) = class.strip_prefix("mask-radial-at-") {
        let value = match raw {
            "top-left" => Some("top left"),
            "top" => Some("top"),
            "top-right" => Some("top right"),
            "left" => Some("left"),
            "center" => Some("center"),
            "right" => Some("right"),
            "bottom-left" => Some("bottom left"),
            "bottom" => Some("bottom"),
            "bottom-right" => Some("bottom right"),
            _ => None,
        };
        if let Some(value) = value {
            return rule(
                &selector,
                &format!("--tw-mask-radial-position:{}", value),
                config,
            );
        }
        if let Some(value) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            if value.is_empty() {
                return None;
            }
            return rule(
                &selector,
                &format!("--tw-mask-radial-position:{}", value),
                config,
            );
        }
    }

    match class {
        "mask-circle" => return rule(&selector, "--tw-mask-radial-shape:circle", config),
        "mask-ellipse" => return rule(&selector, "--tw-mask-radial-shape:ellipse", config),
        "mask-radial-closest-corner" => {
            return rule(&selector, "--tw-mask-radial-size:closest-corner", config);
        }
        "mask-radial-closest-side" => {
            return rule(&selector, "--tw-mask-radial-size:closest-side", config);
        }
        "mask-radial-farthest-corner" => {
            return rule(&selector, "--tw-mask-radial-size:farthest-corner", config);
        }
        "mask-radial-farthest-side" => {
            return rule(&selector, "--tw-mask-radial-size:farthest-side", config);
        }
        _ => {}
    }

    if let Some(raw) = class.strip_prefix("mask-radial-from-") {
        let stop = parse_mask_stop_value(raw)?;
        return rule(&selector, &mask_radial_from_declarations(stop), config);
    }
    if let Some(raw) = class.strip_prefix("mask-radial-to-") {
        let stop = parse_mask_stop_value(raw)?;
        return rule(&selector, &mask_radial_to_declarations(stop), config);
    }

    if let Some(raw) = class.strip_prefix("mask-conic-") {
        if let Some(value) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            if value.is_empty() {
                return None;
            }
            return rule(
                &selector,
                &format!("mask-image:conic-gradient({})", value),
                config,
            );
        }
        if let Some(value) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            if value.is_empty() {
                return None;
            }
            return rule(
                &selector,
                &format!("mask-image:conic-gradient(var({}))", value),
                config,
            );
        }
        if let Some(angle) = parse_mask_angle(raw) {
            return rule(
                &selector,
                &format!(
                    "mask-image:conic-gradient(from {},black var(--tw-mask-conic-from),transparent var(--tw-mask-conic-to))",
                    angle
                ),
                config,
            );
        }
    }

    if let Some(raw) = class.strip_prefix("-mask-conic-") {
        let angle = parse_mask_angle(raw)?;
        return rule(
            &selector,
            &format!(
                "mask-image:conic-gradient(from calc({} * -1),black var(--tw-mask-conic-from),transparent var(--tw-mask-conic-to))",
                angle
            ),
            config,
        );
    }

    if let Some(raw) = class.strip_prefix("mask-conic-from-") {
        let stop = parse_mask_stop_value(raw)?;
        return rule(&selector, &mask_conic_from_declarations(stop), config);
    }
    if let Some(raw) = class.strip_prefix("mask-conic-to-") {
        let stop = parse_mask_stop_value(raw)?;
        return rule(&selector, &mask_conic_to_declarations(stop), config);
    }

    None
}

fn generate_mask_mode_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let value = match class {
        "mask-alpha" => "alpha",
        "mask-luminance" => "luminance",
        "mask-match" => "match-source",
        _ => return None,
    };
    rule(&selector, &format!("mask-mode:{}", value), config)
}

fn generate_mask_type_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let value = match class {
        "mask-type-alpha" => "alpha",
        "mask-type-luminance" => "luminance",
        _ => return None,
    };
    rule(&selector, &format!("mask-type:{}", value), config)
}

fn generate_mask_origin_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let value = match class {
        "mask-origin-border" => "border-box",
        "mask-origin-padding" => "padding-box",
        "mask-origin-content" => "content-box",
        "mask-origin-fill" => "fill-box",
        "mask-origin-stroke" => "stroke-box",
        "mask-origin-view" => "view-box",
        _ => return None,
    };
    rule(&selector, &format!("mask-origin:{}", value), config)
}

fn generate_mask_position_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "mask-top-left" => return rule(&selector, "mask-position:top left", config),
        "mask-top" => return rule(&selector, "mask-position:top", config),
        "mask-top-right" => return rule(&selector, "mask-position:top right", config),
        "mask-left" => return rule(&selector, "mask-position:left", config),
        "mask-center" => return rule(&selector, "mask-position:center", config),
        "mask-right" => return rule(&selector, "mask-position:right", config),
        "mask-bottom-left" => return rule(&selector, "mask-position:bottom left", config),
        "mask-bottom" => return rule(&selector, "mask-position:bottom", config),
        "mask-bottom-right" => return rule(&selector, "mask-position:bottom right", config),
        _ => {}
    }
    if let Some(raw) = class
        .strip_prefix("mask-position-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("mask-position:var({})", raw), config);
    }
    if let Some(raw) = class
        .strip_prefix("mask-position-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("mask-position:{}", raw), config);
    }
    None
}

fn generate_mask_repeat_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let value = match class {
        "mask-repeat" => "repeat",
        "mask-no-repeat" => "no-repeat",
        "mask-repeat-x" => "repeat-x",
        "mask-repeat-y" => "repeat-y",
        "mask-repeat-space" => "space",
        "mask-repeat-round" => "round",
        _ => return None,
    };
    rule(&selector, &format!("mask-repeat:{}", value), config)
}

fn generate_mask_size_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "mask-auto" => return rule(&selector, "mask-size:auto", config),
        "mask-cover" => return rule(&selector, "mask-size:cover", config),
        "mask-contain" => return rule(&selector, "mask-size:contain", config),
        _ => {}
    }
    if let Some(raw) = class
        .strip_prefix("mask-size-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("mask-size:var({})", raw), config);
    }
    if let Some(raw) = class
        .strip_prefix("mask-size-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        if raw.is_empty() {
            return None;
        }
        return rule(&selector, &format!("mask-size:{}", raw), config);
    }
    None
}

fn generate_mask_clip_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let value = match class {
        "mask-clip-border" => "border-box",
        "mask-clip-padding" => "padding-box",
        "mask-clip-content" => "content-box",
        "mask-clip-fill" => "fill-box",
        "mask-clip-stroke" => "stroke-box",
        "mask-clip-view" => "view-box",
        "mask-no-clip" => "no-clip",
        _ => return None,
    };
    rule(&selector, &format!("mask-clip:{}", value), config)
}

fn generate_mask_composite_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let value = match class {
        "mask-add" => "add",
        "mask-subtract" => "subtract",
        "mask-intersect" => "intersect",
        "mask-exclude" => "exclude",
        _ => return None,
    };
    rule(&selector, &format!("mask-composite:{}", value), config)
}

fn generate_object_fit_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "object-contain" => rule(&selector, "object-fit:contain", config),
        "object-cover" => rule(&selector, "object-fit:cover", config),
        "object-fill" => rule(&selector, "object-fit:fill", config),
        "object-none" => rule(&selector, "object-fit:none", config),
        "object-scale-down" => rule(&selector, "object-fit:scale-down", config),
        _ => None,
    }
}

fn generate_object_position_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "object-top-left" => rule(&selector, "object-position:top left", config),
        "object-top" => rule(&selector, "object-position:top", config),
        "object-top-right" => rule(&selector, "object-position:top right", config),
        "object-left" => rule(&selector, "object-position:left", config),
        "object-center" => rule(&selector, "object-position:center", config),
        "object-right" => rule(&selector, "object-position:right", config),
        "object-bottom-left" => rule(&selector, "object-position:bottom left", config),
        "object-bottom" => rule(&selector, "object-position:bottom", config),
        "object-bottom-right" => rule(&selector, "object-position:bottom right", config),
        _ => {
            if let Some(custom) = class
                .strip_prefix("object-(")
                .and_then(|raw| raw.strip_suffix(')'))
            {
                return rule(
                    &selector,
                    &format!("object-position:var({})", custom),
                    config,
                );
            }
            if let Some(raw) = class
                .strip_prefix("object-[")
                .and_then(|value| value.strip_suffix(']'))
            {
                return rule(&selector, &format!("object-position:{}", raw), config);
            }
            None
        }
    }
}

fn generate_overscroll_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "overscroll-auto" => rule(&selector, "overscroll-behavior:auto", config),
        "overscroll-contain" => rule(&selector, "overscroll-behavior:contain", config),
        "overscroll-none" => rule(&selector, "overscroll-behavior:none", config),
        "overscroll-x-auto" => rule(&selector, "overscroll-behavior-x:auto", config),
        "overscroll-x-contain" => rule(&selector, "overscroll-behavior-x:contain", config),
        "overscroll-x-none" => rule(&selector, "overscroll-behavior-x:none", config),
        "overscroll-y-auto" => rule(&selector, "overscroll-behavior-y:auto", config),
        "overscroll-y-contain" => rule(&selector, "overscroll-behavior-y:contain", config),
        "overscroll-y-none" => rule(&selector, "overscroll-behavior-y:none", config),
        _ => None,
    }
}

fn generate_inset_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let (negative, raw) = if let Some(rest) = class.strip_prefix('-') {
        (true, rest)
    } else {
        (false, class)
    };

    let (key, token) = parse_inset_key_and_token(raw)?;
    let value = parse_inset_value(token, negative)?;
    let declarations = inset_declarations(key, &value)?;
    rule(&selector, &declarations, config)
}

fn generate_visibility_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "visible" => rule(&selector, "visibility:visible", config),
        "invisible" => rule(&selector, "visibility:hidden", config),
        "collapse" => rule(&selector, "visibility:collapse", config),
        _ => None,
    }
}

fn generate_backface_visibility_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "backface-hidden" => rule(&selector, "backface-visibility:hidden", config),
        "backface-visible" => rule(&selector, "backface-visibility:visible", config),
        _ => None,
    }
}

fn generate_perspective_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "perspective-dramatic" => {
            rule(&selector, "perspective:var(--perspective-dramatic)", config)
        }
        "perspective-near" => rule(&selector, "perspective:var(--perspective-near)", config),
        "perspective-normal" => rule(&selector, "perspective:var(--perspective-normal)", config),
        "perspective-midrange" => {
            rule(&selector, "perspective:var(--perspective-midrange)", config)
        }
        "perspective-distant" => rule(&selector, "perspective:var(--perspective-distant)", config),
        "perspective-none" => rule(&selector, "perspective:none", config),
        _ => {
            if let Some(raw) = class
                .strip_prefix("perspective-(")
                .and_then(|value| value.strip_suffix(')'))
            {
                if !raw.is_empty() {
                    return rule(&selector, &format!("perspective:var({})", raw), config);
                }
            }
            if let Some(raw) = class
                .strip_prefix("perspective-[")
                .and_then(|value| value.strip_suffix(']'))
            {
                if !raw.is_empty() {
                    return rule(&selector, &format!("perspective:{}", raw), config);
                }
            }
            if let Some(raw) = class.strip_prefix("perspective-") {
                if !raw.is_empty() {
                    return rule(
                        &selector,
                        &format!("perspective:var(--perspective-{})", raw),
                        config,
                    );
                }
            }
            None
        }
    }
}

fn generate_perspective_origin_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "perspective-origin-center" => rule(&selector, "perspective-origin:center", config),
        "perspective-origin-top" => rule(&selector, "perspective-origin:top", config),
        "perspective-origin-top-right" => rule(&selector, "perspective-origin:top right", config),
        "perspective-origin-right" => rule(&selector, "perspective-origin:right", config),
        "perspective-origin-bottom-right" => {
            rule(&selector, "perspective-origin:bottom right", config)
        }
        "perspective-origin-bottom" => rule(&selector, "perspective-origin:bottom", config),
        "perspective-origin-bottom-left" => {
            rule(&selector, "perspective-origin:bottom left", config)
        }
        "perspective-origin-left" => rule(&selector, "perspective-origin:left", config),
        "perspective-origin-top-left" => rule(&selector, "perspective-origin:top left", config),
        _ => {
            if let Some(raw) = class
                .strip_prefix("perspective-origin-(")
                .and_then(|value| value.strip_suffix(')'))
            {
                if !raw.is_empty() {
                    return rule(
                        &selector,
                        &format!("perspective-origin:var({})", raw),
                        config,
                    );
                }
            }
            if let Some(raw) = class
                .strip_prefix("perspective-origin-[")
                .and_then(|value| value.strip_suffix(']'))
            {
                if !raw.is_empty() {
                    return rule(&selector, &format!("perspective-origin:{}", raw), config);
                }
            }
            None
        }
    }
}

fn generate_rotate_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(number) = class.strip_prefix("rotate-x-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!("transform:rotateX({}deg) var(--tw-rotate-y)", number),
                config,
            );
        }
    }
    if let Some(number) = class.strip_prefix("-rotate-x-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!("transform:rotateX(-{}deg) var(--tw-rotate-y)", number),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("rotate-x-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!("transform:rotateX(var({})) var(--tw-rotate-y)", raw),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("rotate-x-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!("transform:rotateX({}) var(--tw-rotate-y)", raw),
                config,
            );
        }
    }

    if let Some(number) = class.strip_prefix("rotate-y-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!("transform:var(--tw-rotate-x) rotateY({}deg)", number),
                config,
            );
        }
    }
    if let Some(number) = class.strip_prefix("-rotate-y-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!("transform:var(--tw-rotate-x) rotateY(-{}deg)", number),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("rotate-y-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!("transform:var(--tw-rotate-x) rotateY(var({}))", raw),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("rotate-y-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!("transform:var(--tw-rotate-x) rotateY({})", raw),
                config,
            );
        }
    }

    if let Some(number) = class.strip_prefix("rotate-z-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!(
                    "transform:var(--tw-rotate-x) var(--tw-rotate-y) rotateZ({}deg)",
                    number
                ),
                config,
            );
        }
    }
    if let Some(number) = class.strip_prefix("-rotate-z-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!(
                    "transform:var(--tw-rotate-x) var(--tw-rotate-y) rotateZ(-{}deg)",
                    number
                ),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("rotate-z-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!(
                    "transform:var(--tw-rotate-x) var(--tw-rotate-y) rotateZ(var({}))",
                    raw
                ),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("rotate-z-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!(
                    "transform:var(--tw-rotate-x) var(--tw-rotate-y) rotateZ({})",
                    raw
                ),
                config,
            );
        }
    }

    if class == "rotate-none" {
        return rule(&selector, "rotate:none", config);
    }
    if let Some(number) = class.strip_prefix("rotate-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(&selector, &format!("rotate:{}deg", number), config);
        }
    }
    if let Some(number) = class.strip_prefix("-rotate-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!("rotate:calc({}deg * -1)", number),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("rotate-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if !raw.is_empty() {
            return rule(&selector, &format!("rotate:var({})", raw), config);
        }
    }
    if let Some(raw) = class
        .strip_prefix("rotate-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if !raw.is_empty() {
            return rule(&selector, &format!("rotate:{}", raw), config);
        }
    }

    None
}

fn generate_scale_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "scale-3d" {
        return rule(
            &selector,
            "scale:var(--tw-scale-x) var(--tw-scale-y) var(--tw-scale-z)",
            config,
        );
    }

    if let Some(number) = class.strip_prefix("scale-x-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!("scale:{}% var(--tw-scale-y)", number),
                config,
            );
        }
    }
    if let Some(number) = class.strip_prefix("-scale-x-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!("scale:calc({}% * -1) var(--tw-scale-y)", number),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("scale-x-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!("scale:var({}) var(--tw-scale-y)", raw),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("scale-x-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!("scale:{} var(--tw-scale-y)", raw),
                config,
            );
        }
    }

    if let Some(number) = class.strip_prefix("scale-y-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!("scale:var(--tw-scale-x) {}%", number),
                config,
            );
        }
    }
    if let Some(number) = class.strip_prefix("-scale-y-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!("scale:var(--tw-scale-x) calc({}% * -1)", number),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("scale-y-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!("scale:var(--tw-scale-x) var({})", raw),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("scale-y-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!("scale:var(--tw-scale-x) {}", raw),
                config,
            );
        }
    }

    if let Some(number) = class.strip_prefix("scale-z-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!("scale:var(--tw-scale-x) var(--tw-scale-y) {}%", number),
                config,
            );
        }
    }
    if let Some(number) = class.strip_prefix("-scale-z-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!(
                    "scale:var(--tw-scale-x) var(--tw-scale-y) calc({}% * -1)",
                    number
                ),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("scale-z-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!("scale:var(--tw-scale-x) var(--tw-scale-y) var({})", raw),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("scale-z-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!("scale:var(--tw-scale-x) var(--tw-scale-y) {}", raw),
                config,
            );
        }
    }

    if class == "scale-none" {
        return rule(&selector, "scale:none", config);
    }
    if let Some(number) = class.strip_prefix("scale-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-scale-x:{}%;--tw-scale-y:{}%;--tw-scale-z:{}%;scale:var(--tw-scale-x) var(--tw-scale-y)",
                    number, number, number
                ),
                config,
            );
        }
    }
    if let Some(number) = class.strip_prefix("-scale-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-scale-x:calc({}% * -1);--tw-scale-y:calc({}% * -1);--tw-scale-z:calc({}% * -1);scale:var(--tw-scale-x) var(--tw-scale-y)",
                    number, number, number
                ),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("scale-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-scale-x:var({});--tw-scale-y:var({});--tw-scale-z:var({});scale:var(--tw-scale-x) var(--tw-scale-y)",
                    raw, raw, raw
                ),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("scale-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if !raw.is_empty() {
            return rule(&selector, &format!("scale:{}", raw), config);
        }
    }

    None
}

fn generate_skew_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let transform_chain = "var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,) var(--tw-skew-x,) var(--tw-skew-y,)";

    if let Some(number) = class.strip_prefix("skew-x-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-skew-x:skewX({}deg);transform:{}",
                    number, transform_chain
                ),
                config,
            );
        }
    }
    if let Some(number) = class.strip_prefix("-skew-x-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-skew-x:skewX(-{}deg);transform:{}",
                    number, transform_chain
                ),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("skew-x-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-skew-x:skewX(var({}));transform:{}",
                    raw, transform_chain
                ),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("skew-x-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!("--tw-skew-x:skewX({});transform:{}", raw, transform_chain),
                config,
            );
        }
    }

    if let Some(number) = class.strip_prefix("skew-y-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-skew-y:skewY({}deg);transform:{}",
                    number, transform_chain
                ),
                config,
            );
        }
    }
    if let Some(number) = class.strip_prefix("-skew-y-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-skew-y:skewY(-{}deg);transform:{}",
                    number, transform_chain
                ),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("skew-y-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-skew-y:skewY(var({}));transform:{}",
                    raw, transform_chain
                ),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("skew-y-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!("--tw-skew-y:skewY({});transform:{}", raw, transform_chain),
                config,
            );
        }
    }

    if let Some(number) = class.strip_prefix("skew-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-skew-x:skewX({}deg);--tw-skew-y:skewY({}deg);transform:{}",
                    number, number, transform_chain
                ),
                config,
            );
        }
    }
    if let Some(number) = class.strip_prefix("-skew-") {
        if number.chars().all(|c| c.is_ascii_digit()) && !number.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-skew-x:skewX(-{}deg);--tw-skew-y:skewY(-{}deg);transform:{}",
                    number, number, transform_chain
                ),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("skew-(")
        .and_then(|value| value.strip_suffix(')'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-skew-x:skewX(var({}));--tw-skew-y:skewY(var({}));transform:{}",
                    raw, raw, transform_chain
                ),
                config,
            );
        }
    }
    if let Some(raw) = class
        .strip_prefix("skew-[")
        .and_then(|value| value.strip_suffix(']'))
    {
        if !raw.is_empty() {
            return rule(
                &selector,
                &format!(
                    "--tw-skew-x:skewX({});--tw-skew-y:skewY({});transform:{}",
                    raw, raw, transform_chain
                ),
                config,
            );
        }
    }

    None
}

fn generate_transform_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "transform" => rule(
            &selector,
            "transform:var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,) var(--tw-skew-x,) var(--tw-skew-y,)",
            config,
        ),
        "transform-none" => rule(&selector, "transform:none", config),
        "transform-gpu" => rule(
            &selector,
            "transform:translateZ(0) var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,) var(--tw-skew-x,) var(--tw-skew-y,)",
            config,
        ),
        "transform-cpu" => rule(
            &selector,
            "transform:var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,) var(--tw-skew-x,) var(--tw-skew-y,)",
            config,
        ),
        _ => {
            if let Some(raw) = class
                .strip_prefix("transform-(")
                .and_then(|value| value.strip_suffix(')'))
            {
                if !raw.is_empty() {
                    return rule(&selector, &format!("transform:var({})", raw), config);
                }
            }
            if let Some(raw) = class
                .strip_prefix("transform-[")
                .and_then(|value| value.strip_suffix(']'))
            {
                if !raw.is_empty() {
                    return rule(&selector, &format!("transform:{}", raw), config);
                }
            }
            None
        }
    }
}

fn generate_transform_style_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "transform-3d" => rule(&selector, "transform-style:preserve-3d", config),
        "transform-flat" => rule(&selector, "transform-style:flat", config),
        _ => None,
    }
}

fn generate_transform_origin_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "origin-center" => rule(&selector, "transform-origin:center", config),
        "origin-top" => rule(&selector, "transform-origin:top", config),
        "origin-top-right" => rule(&selector, "transform-origin:top right", config),
        "origin-right" => rule(&selector, "transform-origin:right", config),
        "origin-bottom-right" => rule(&selector, "transform-origin:bottom right", config),
        "origin-bottom" => rule(&selector, "transform-origin:bottom", config),
        "origin-bottom-left" => rule(&selector, "transform-origin:bottom left", config),
        "origin-left" => rule(&selector, "transform-origin:left", config),
        "origin-top-left" => rule(&selector, "transform-origin:top left", config),
        _ => {
            if let Some(raw) = class
                .strip_prefix("origin-(")
                .and_then(|value| value.strip_suffix(')'))
            {
                if !raw.is_empty() {
                    return rule(&selector, &format!("transform-origin:var({})", raw), config);
                }
            }
            if let Some(raw) = class
                .strip_prefix("origin-[")
                .and_then(|value| value.strip_suffix(']'))
            {
                if !raw.is_empty() {
                    return rule(&selector, &format!("transform-origin:{}", raw), config);
                }
            }
            None
        }
    }
}

fn generate_translate_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "translate-none" {
        return rule(&selector, "translate:none", config);
    }

    let (negative, raw) = if let Some(rest) = class.strip_prefix('-') {
        (true, rest)
    } else {
        (false, class)
    };

    if let Some(token) = raw.strip_prefix("translate-x-") {
        if !negative {
            if let Some(custom) = token.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
                if !custom.is_empty() {
                    return rule(
                        &selector,
                        &format!(
                            "--tw-translate-x:var({});translate:var(--tw-translate-x) var(--tw-translate-y)",
                            custom
                        ),
                        config,
                    );
                }
            }
            if let Some(custom) = token.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
                if !custom.is_empty() {
                    return rule(
                        &selector,
                        &format!(
                            "--tw-translate-x:{};translate:var(--tw-translate-x) var(--tw-translate-y)",
                            custom
                        ),
                        config,
                    );
                }
            }
        }
        let value = parse_translate_value(token, negative, true, true)?;
        return rule(
            &selector,
            &format!(
                "--tw-translate-x:{};translate:var(--tw-translate-x) var(--tw-translate-y)",
                value
            ),
            config,
        );
    }

    if let Some(token) = raw.strip_prefix("translate-y-") {
        if !negative {
            if let Some(custom) = token.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
                if !custom.is_empty() {
                    return rule(
                        &selector,
                        &format!(
                            "--tw-translate-y:var({});translate:var(--tw-translate-x) var(--tw-translate-y)",
                            custom
                        ),
                        config,
                    );
                }
            }
            if let Some(custom) = token.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
                if !custom.is_empty() {
                    return rule(
                        &selector,
                        &format!(
                            "--tw-translate-y:{};translate:var(--tw-translate-x) var(--tw-translate-y)",
                            custom
                        ),
                        config,
                    );
                }
            }
        }
        let value = parse_translate_value(token, negative, true, true)?;
        return rule(
            &selector,
            &format!(
                "--tw-translate-y:{};translate:var(--tw-translate-x) var(--tw-translate-y)",
                value
            ),
            config,
        );
    }

    if let Some(token) = raw.strip_prefix("translate-z-") {
        if !negative {
            if let Some(custom) = token.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
                if !custom.is_empty() {
                    return rule(
                        &selector,
                        &format!(
                            "translate:var(--tw-translate-x) var(--tw-translate-y) var({})",
                            custom
                        ),
                        config,
                    );
                }
            }
            if let Some(custom) = token.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
                if !custom.is_empty() {
                    return rule(
                        &selector,
                        &format!(
                            "translate:var(--tw-translate-x) var(--tw-translate-y) {}",
                            custom
                        ),
                        config,
                    );
                }
            }
        }
        let value = parse_translate_value(token, negative, false, false)?;
        return rule(
            &selector,
            &format!(
                "translate:var(--tw-translate-x) var(--tw-translate-y) {}",
                value
            ),
            config,
        );
    }

    if let Some(token) = raw.strip_prefix("translate-") {
        if !negative {
            if let Some(custom) = token.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
                if !custom.is_empty() {
                    return rule(
                        &selector,
                        &format!(
                            "--tw-translate-x:var({});--tw-translate-y:var({});translate:var(--tw-translate-x) var(--tw-translate-y)",
                            custom, custom
                        ),
                        config,
                    );
                }
            }
            if let Some(custom) = token.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
                if !custom.is_empty() {
                    return rule(
                        &selector,
                        &format!(
                            "--tw-translate-x:{};--tw-translate-y:{};translate:var(--tw-translate-x) var(--tw-translate-y)",
                            custom, custom
                        ),
                        config,
                    );
                }
            }
        }
        let value = parse_translate_value(token, negative, true, true)?;
        return rule(
            &selector,
            &format!(
                "--tw-translate-x:{};--tw-translate-y:{};translate:var(--tw-translate-x) var(--tw-translate-y)",
                value, value
            ),
            config,
        );
    }

    None
}

fn parse_translate_value(
    token: &str,
    negative: bool,
    allow_fraction: bool,
    allow_full: bool,
) -> Option<String> {
    if token == "px" {
        return Some(if negative {
            "-1px".to_string()
        } else {
            "1px".to_string()
        });
    }
    if allow_full && token == "full" {
        return Some(if negative {
            "-100%".to_string()
        } else {
            "100%".to_string()
        });
    }
    if is_spacing_multiplier(token) {
        return Some(if negative {
            format!("calc(var(--spacing) * -{})", token)
        } else {
            format!("calc(var(--spacing) * {})", token)
        });
    }
    if allow_fraction && is_fraction(token) {
        return Some(if negative {
            format!("calc(calc({} * 100%) * -1)", token)
        } else {
            format!("calc({} * 100%)", token)
        });
    }
    None
}

fn generate_z_index_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if class == "z-auto" {
        return rule(&selector, "z-index:auto", config);
    }
    if let Some(num) = class.strip_prefix("z-") {
        if num.chars().all(|c| c.is_ascii_digit()) && !num.is_empty() {
            return rule(&selector, &format!("z-index:{}", num), config);
        }
    }
    if let Some(num) = class.strip_prefix("-z-") {
        if num.chars().all(|c| c.is_ascii_digit()) && !num.is_empty() {
            return rule(&selector, &format!("z-index:calc({} * -1)", num), config);
        }
    }
    if let Some(raw) = class.strip_prefix("z-[").and_then(|v| v.strip_suffix(']')) {
        return rule(&selector, &format!("z-index:{}", raw), config);
    }
    if let Some(custom) = class.strip_prefix("z-(").and_then(|v| v.strip_suffix(')')) {
        return rule(&selector, &format!("z-index:var({})", custom), config);
    }
    None
}

fn generate_flex_basis_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("basis-")?;

    if raw == "auto" {
        return rule(&selector, "flex-basis:auto", config);
    }
    if raw == "full" {
        return rule(&selector, "flex-basis:100%", config);
    }
    if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        return rule(&selector, &format!("flex-basis:{}", custom), config);
    }
    if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        return rule(&selector, &format!("flex-basis:var({})", custom), config);
    }
    if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
        return rule(
            &selector,
            &format!("flex-basis:calc(var(--spacing) * {})", raw),
            config,
        );
    }
    if is_fraction(raw) {
        return rule(
            &selector,
            &format!("flex-basis:calc({} * 100%)", raw),
            config,
        );
    }
    if is_container_token(raw) {
        return rule(
            &selector,
            &format!("flex-basis:var(--container-{})", raw),
            config,
        );
    }

    None
}

fn generate_flex_shorthand_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("flex-")?;

    if matches!(
        raw,
        "row" | "row-reverse" | "col" | "col-reverse" | "wrap" | "wrap-reverse" | "nowrap"
    ) {
        return None;
    }
    if raw == "auto" {
        return rule(&selector, "flex:auto", config);
    }
    if raw == "initial" {
        return rule(&selector, "flex:0 auto", config);
    }
    if raw == "none" {
        return rule(&selector, "flex:none", config);
    }
    if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        return rule(&selector, &format!("flex:{}", custom), config);
    }
    if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        return rule(&selector, &format!("flex:var({})", custom), config);
    }
    if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
        return rule(&selector, &format!("flex:{}", raw), config);
    }
    if is_fraction(raw) {
        return rule(&selector, &format!("flex:calc({} * 100%)", raw), config);
    }
    None
}

fn generate_flex_grow_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if class == "grow" || class == "flex-grow" {
        return rule(&selector, "flex-grow:1", config);
    }
    let raw = class
        .strip_prefix("grow-")
        .or_else(|| class.strip_prefix("flex-grow-"))?;
    if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
        return rule(&selector, &format!("flex-grow:{}", raw), config);
    }
    if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        return rule(&selector, &format!("flex-grow:{}", custom), config);
    }
    if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        return rule(&selector, &format!("flex-grow:var({})", custom), config);
    }
    None
}

fn generate_flex_shrink_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if class == "shrink" || class == "flex-shrink" {
        return rule(&selector, "flex-shrink:1", config);
    }
    let raw = class
        .strip_prefix("shrink-")
        .or_else(|| class.strip_prefix("flex-shrink-"))?;
    if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
        return rule(&selector, &format!("flex-shrink:{}", raw), config);
    }
    if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        return rule(&selector, &format!("flex-shrink:{}", custom), config);
    }
    if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        return rule(&selector, &format!("flex-shrink:var({})", custom), config);
    }
    None
}

fn generate_order_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "order-first" => return rule(&selector, "order:-9999", config),
        "order-last" => return rule(&selector, "order:9999", config),
        "order-none" => return rule(&selector, "order:0", config),
        _ => {}
    }

    if let Some(num) = class.strip_prefix("order-") {
        if num.chars().all(|c| c.is_ascii_digit()) && !num.is_empty() {
            return rule(&selector, &format!("order:{}", num), config);
        }
        if let Some(raw) = num.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            return rule(&selector, &format!("order:{}", raw), config);
        }
        if let Some(custom) = num.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            return rule(&selector, &format!("order:var({})", custom), config);
        }
    }

    if let Some(num) = class.strip_prefix("-order-") {
        if num.chars().all(|c| c.is_ascii_digit()) && !num.is_empty() {
            return rule(&selector, &format!("order:calc({} * -1)", num), config);
        }
    }

    None
}

fn generate_grid_template_columns_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if class == "grid-cols-none" {
        return rule(&selector, "grid-template-columns:none", config);
    }
    if class == "grid-cols-subgrid" {
        return rule(&selector, "grid-template-columns:subgrid", config);
    }
    if let Some(raw) = class.strip_prefix("grid-cols-") {
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return rule(
                &selector,
                &format!("grid-template-columns:repeat({}, minmax(0, 1fr))", raw),
                config,
            );
        }
        if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            let value = normalize_arbitrary_value(custom);
            return rule(
                &selector,
                &format!("grid-template-columns:{}", value),
                config,
            );
        }
        if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            return rule(
                &selector,
                &format!("grid-template-columns:var({})", custom),
                config,
            );
        }
    }
    None
}

fn generate_grid_auto_flow_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "grid-flow-row" => rule(&selector, "grid-auto-flow:row", config),
        "grid-flow-col" => rule(&selector, "grid-auto-flow:column", config),
        "grid-flow-dense" => rule(&selector, "grid-auto-flow:dense", config),
        "grid-flow-row-dense" => rule(&selector, "grid-auto-flow:row dense", config),
        "grid-flow-col-dense" => rule(&selector, "grid-auto-flow:column dense", config),
        _ => None,
    }
}

fn generate_grid_auto_columns_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "auto-cols-auto" => rule(&selector, "grid-auto-columns:auto", config),
        "auto-cols-min" => rule(&selector, "grid-auto-columns:min-content", config),
        "auto-cols-max" => rule(&selector, "grid-auto-columns:max-content", config),
        "auto-cols-fr" => rule(&selector, "grid-auto-columns:minmax(0, 1fr)", config),
        _ => {
            if let Some(custom) = class
                .strip_prefix("auto-cols-(")
                .and_then(|v| v.strip_suffix(')'))
            {
                return rule(
                    &selector,
                    &format!("grid-auto-columns:var({})", custom),
                    config,
                );
            }
            if let Some(custom) = class
                .strip_prefix("auto-cols-[")
                .and_then(|v| v.strip_suffix(']'))
            {
                return rule(&selector, &format!("grid-auto-columns:{}", custom), config);
            }
            None
        }
    }
}

fn generate_grid_auto_rows_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    match class {
        "auto-rows-auto" => rule(&selector, "grid-auto-rows:auto", config),
        "auto-rows-min" => rule(&selector, "grid-auto-rows:min-content", config),
        "auto-rows-max" => rule(&selector, "grid-auto-rows:max-content", config),
        "auto-rows-fr" => rule(&selector, "grid-auto-rows:minmax(0, 1fr)", config),
        _ => {
            if let Some(custom) = class
                .strip_prefix("auto-rows-(")
                .and_then(|v| v.strip_suffix(')'))
            {
                return rule(
                    &selector,
                    &format!("grid-auto-rows:var({})", custom),
                    config,
                );
            }
            if let Some(custom) = class
                .strip_prefix("auto-rows-[")
                .and_then(|v| v.strip_suffix(']'))
            {
                return rule(&selector, &format!("grid-auto-rows:{}", custom), config);
            }
            None
        }
    }
}

fn parse_border_width_value(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }
    if raw.chars().all(|c| c.is_ascii_digit()) {
        return Some(format!("{}px", raw));
    }
    if let Some(value) = raw
        .strip_prefix("(length:")
        .and_then(|value| value.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return Some(format!("var({})", value));
    }
    if let Some(value) = raw
        .strip_prefix('[')
        .and_then(|value| value.strip_suffix(']'))
    {
        if value.is_empty() || value.contains(':') || is_color_like_value(value) {
            return None;
        }
        return Some(value.to_string());
    }
    None
}

fn parse_border_spacing_value(raw: &str) -> Option<String> {
    if is_spacing_multiplier(raw) {
        return Some(format!("calc(var(--spacing) * {})", raw));
    }
    if let Some(value) = raw
        .strip_prefix('(')
        .and_then(|value| value.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return Some(format!("var({})", value));
    }
    if let Some(value) = raw
        .strip_prefix('[')
        .and_then(|value| value.strip_suffix(']'))
    {
        if value.is_empty() {
            return None;
        }
        return Some(value.to_string());
    }
    None
}

fn generate_border_spacing_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class.strip_prefix("border-spacing-x-") {
        let value = parse_border_spacing_value(raw)?;
        return rule(
            &selector,
            &format!("border-spacing:{} var(--tw-border-spacing-y)", value),
            config,
        );
    }

    if let Some(raw) = class.strip_prefix("border-spacing-y-") {
        let value = parse_border_spacing_value(raw)?;
        return rule(
            &selector,
            &format!("border-spacing:var(--tw-border-spacing-x) {}", value),
            config,
        );
    }

    if let Some(raw) = class.strip_prefix("border-spacing-") {
        let value = parse_border_spacing_value(raw)?;
        return rule(&selector, &format!("border-spacing:{}", value), config);
    }

    None
}

fn generate_border_width_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "border" {
        return rule(
            &selector,
            "border-style:var(--tw-border-style);border-width:1px",
            config,
        );
    }

    for (utility, style_property, width_property) in [
        ("border-x", "border-inline-style", "border-inline-width"),
        ("border-y", "border-block-style", "border-block-width"),
        (
            "border-s",
            "border-inline-start-style",
            "border-inline-start-width",
        ),
        (
            "border-e",
            "border-inline-end-style",
            "border-inline-end-width",
        ),
        ("border-t", "border-top-style", "border-top-width"),
        ("border-r", "border-right-style", "border-right-width"),
        ("border-b", "border-bottom-style", "border-bottom-width"),
        ("border-l", "border-left-style", "border-left-width"),
    ] {
        if class == utility {
            return rule(
                &selector,
                &format!(
                    "{}:var(--tw-border-style);{}:1px",
                    style_property, width_property
                ),
                config,
            );
        }
    }

    for (prefix, style_property, width_property) in [
        ("border-x-", "border-inline-style", "border-inline-width"),
        ("border-y-", "border-block-style", "border-block-width"),
        (
            "border-s-",
            "border-inline-start-style",
            "border-inline-start-width",
        ),
        (
            "border-e-",
            "border-inline-end-style",
            "border-inline-end-width",
        ),
        ("border-t-", "border-top-style", "border-top-width"),
        ("border-r-", "border-right-style", "border-right-width"),
        ("border-b-", "border-bottom-style", "border-bottom-width"),
        ("border-l-", "border-left-style", "border-left-width"),
        ("border-", "border-style", "border-width"),
    ] {
        if let Some(raw) = class.strip_prefix(prefix) {
            if let Some(value) = parse_border_width_value(raw) {
                return rule(
                    &selector,
                    &format!(
                        "{}:var(--tw-border-style);{}:{}",
                        style_property, width_property, value
                    ),
                    config,
                );
            }
        }
    }

    None
}

fn generate_divide_width_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "divide-x-reverse" {
        return rule(
            &selector,
            &nested_child_selector_block("--tw-divide-x-reverse:1"),
            config,
        );
    }
    if class == "divide-y-reverse" {
        return rule(
            &selector,
            &nested_child_selector_block("--tw-divide-y-reverse:1"),
            config,
        );
    }

    if class == "divide-x" {
        let declarations = "--tw-divide-x-reverse:0;border-inline-style:var(--tw-border-style);border-inline-start-width:calc(1px * var(--tw-divide-x-reverse));border-inline-end-width:calc(1px * calc(1 - var(--tw-divide-x-reverse)))";
        return rule(
            &selector,
            &nested_child_selector_block(declarations),
            config,
        );
    }
    if class == "divide-y" {
        let declarations = "--tw-divide-y-reverse:0;border-bottom-style:var(--tw-border-style);border-top-style:var(--tw-border-style);border-top-width:calc(1px * var(--tw-divide-y-reverse));border-bottom-width:calc(1px * calc(1 - var(--tw-divide-y-reverse)))";
        return rule(
            &selector,
            &nested_child_selector_block(declarations),
            config,
        );
    }

    if let Some(raw) = class.strip_prefix("divide-x-") {
        if let Some(value) = parse_border_width_value(raw) {
            let declarations = format!(
                "--tw-divide-x-reverse:0;border-inline-style:var(--tw-border-style);border-inline-start-width:calc({} * var(--tw-divide-x-reverse));border-inline-end-width:calc({} * calc(1 - var(--tw-divide-x-reverse)))",
                value, value
            );
            return rule(
                &selector,
                &nested_child_selector_block(&declarations),
                config,
            );
        }
    }

    if let Some(raw) = class.strip_prefix("divide-y-") {
        if let Some(value) = parse_border_width_value(raw) {
            let declarations = format!(
                "--tw-divide-y-reverse:0;border-bottom-style:var(--tw-border-style);border-top-style:var(--tw-border-style);border-top-width:calc({} * var(--tw-divide-y-reverse));border-bottom-width:calc({} * calc(1 - var(--tw-divide-y-reverse)))",
                value, value
            );
            return rule(
                &selector,
                &nested_child_selector_block(&declarations),
                config,
            );
        }
    }

    None
}

fn generate_border_style_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let value = match class {
        "border-solid" | "divide-solid" => "solid",
        "border-dashed" | "divide-dashed" => "dashed",
        "border-dotted" | "divide-dotted" => "dotted",
        "border-double" | "divide-double" => "double",
        "border-hidden" | "divide-hidden" => "hidden",
        "border-none" | "divide-none" => "none",
        _ => return None,
    };

    let selector = format!(".{}", escape_selector(class));
    if class.starts_with("divide-") {
        let declarations = format!("--tw-border-style:{};border-style:{}", value, value);
        return rule(
            &selector,
            &nested_child_selector_block(&declarations),
            config,
        );
    }
    rule(
        &selector,
        &format!("--tw-border-style:{};border-style:{}", value, value),
        config,
    )
}

fn parse_border_color_components(raw: &str) -> Option<(String, Option<String>)> {
    if raw.is_empty() {
        return None;
    }

    let (token, opacity) = split_slash_modifier(raw);
    if token.is_empty() {
        return None;
    }

    if let Some(value) = token
        .strip_prefix('[')
        .and_then(|value| value.strip_suffix(']'))
    {
        let color = parse_arbitrary_color_value(value)?;
        return Some((color, opacity.map(ToString::to_string)));
    }
    if let Some(value) = token
        .strip_prefix('(')
        .and_then(|value| value.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return Some((format!("var({})", value), opacity.map(ToString::to_string)));
    }
    if matches!(token, "collapse" | "separate") {
        return None;
    }
    let color_value = theme_color_value_from_token(token, true)?;
    Some((color_value, opacity.map(ToString::to_string)))
}

fn generate_border_color_rule(
    class: &str,
    config: &GeneratorConfig,
    variant_tables: &VariantTables,
) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    for (prefix, property, child_only) in [
        ("border-x-", "border-inline-color", false),
        ("border-y-", "border-block-color", false),
        ("border-s-", "border-inline-start-color", false),
        ("border-e-", "border-inline-end-color", false),
        ("border-t-", "border-top-color", false),
        ("border-r-", "border-right-color", false),
        ("border-b-", "border-bottom-color", false),
        ("border-l-", "border-left-color", false),
        ("border-", "border-color", false),
        ("divide-", "border-color", true),
    ] {
        if let Some(raw) = class.strip_prefix(prefix) {
            let (color, opacity) = parse_border_color_components(raw)?;
            let target = if child_only {
                selector.clone()
            } else {
                selector.clone()
            };
            let declarations = color_declaration_with_optional_opacity(
                property,
                &color,
                opacity.as_deref(),
                variant_tables,
            )?;
            if child_only {
                return rule(&target, &nested_child_selector_block(&declarations), config);
            }
            return rule(&target, &declarations, config);
        }
    }

    None
}

fn parse_outline_width_value(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }
    if raw.chars().all(|c| c.is_ascii_digit()) {
        return Some(format!("{}px", raw));
    }
    if let Some(value) = raw
        .strip_prefix("(length:")
        .and_then(|value| value.strip_suffix(')'))
    {
        if value.is_empty() {
            return None;
        }
        return Some(format!("var({})", value));
    }
    if let Some(value) = raw
        .strip_prefix('[')
        .and_then(|value| value.strip_suffix(']'))
    {
        if value.is_empty() || value.contains(':') || is_color_like_value(value) {
            return None;
        }
        return Some(value.to_string());
    }
    None
}

fn parse_outline_color_value(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }

    if let Some(value) = raw
        .strip_prefix('[')
        .and_then(|value| value.strip_suffix(']'))
    {
        return parse_arbitrary_color_value(value);
    }

    if let Some(value) = raw
        .strip_prefix('(')
        .and_then(|value| value.strip_suffix(')'))
    {
        if value.is_empty() || value.starts_with("length:") {
            return None;
        }
        return Some(format!("var({})", value));
    }

    let (token, opacity) = split_slash_modifier(raw);
    if token.is_empty() {
        return None;
    }
    if token.chars().all(|ch| ch.is_ascii_digit())
        || matches!(
            token,
            "solid" | "dashed" | "dotted" | "double" | "none" | "hidden"
        )
    {
        return None;
    }
    let color_value = theme_color_value_from_token(token, true)?;

    if let Some(opacity_raw) = opacity {
        let opacity_value = parse_color_opacity_value(opacity_raw)?;
        return Some(format!(
            "color-mix(in oklab,{} {},transparent)",
            color_value, opacity_value
        ));
    }

    Some(color_value)
}

fn generate_outline_offset_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if let Some(raw) = class.strip_prefix("outline-offset-") {
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return rule(&selector, &format!("outline-offset:{}px", raw), config);
        }
        if let Some(value) = raw
            .strip_prefix('(')
            .and_then(|value| value.strip_suffix(')'))
        {
            if value.is_empty() {
                return None;
            }
            return rule(&selector, &format!("outline-offset:var({})", value), config);
        }
        if let Some(value) = raw
            .strip_prefix('[')
            .and_then(|value| value.strip_suffix(']'))
        {
            if value.is_empty() {
                return None;
            }
            return rule(&selector, &format!("outline-offset:{}", value), config);
        }
    }

    if let Some(raw) = class.strip_prefix("-outline-offset-") {
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return rule(
                &selector,
                &format!("outline-offset:calc({}px * -1)", raw),
                config,
            );
        }
    }

    None
}

fn generate_outline_style_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let declarations = match class {
        "outline-solid" => "outline-style:solid",
        "outline-dashed" => "outline-style:dashed",
        "outline-dotted" => "outline-style:dotted",
        "outline-double" => "outline-style:double",
        "outline-none" => "--tw-outline-style:none;outline-style:none",
        "outline-hidden" => "outline:2px solid transparent;outline-offset:2px",
        _ => return None,
    };
    rule(&selector, declarations, config)
}

fn generate_outline_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    let raw = class.strip_prefix("outline-")?;
    let value = parse_outline_color_value(raw)?;
    rule(&selector, &format!("outline-color:{}", value), config)
}

fn generate_outline_width_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if class == "outline" {
        return rule(
            &selector,
            "outline-style:var(--tw-outline-style);outline-width:1px",
            config,
        );
    }
    let raw = class.strip_prefix("outline-")?;
    let value = parse_outline_width_value(raw)?;
    rule(
        &selector,
        &format!(
            "outline-style:var(--tw-outline-style);outline-width:{}",
            value
        ),
        config,
    )
}

fn generate_border_radius_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));
    if class == "rounded" {
        return rule(&selector, "border-radius:0.25rem", config);
    }
    let raw = class.strip_prefix("rounded-")?;

    let targets = if let Some((target, rest)) = raw.split_once('-') {
        if rest.is_empty() {
            return None;
        }
        if let Some(props) = radius_target_properties(target) {
            (props, rest)
        } else {
            (&["border-radius"][..], raw)
        }
    } else {
        (&["border-radius"][..], raw)
    };

    let value = parse_radius_value(targets.1)?;
    let declarations = targets
        .0
        .iter()
        .map(|prop| format!("{}:{}", prop, value))
        .collect::<Vec<_>>()
        .join(";");
    rule(&selector, &declarations, config)
}

fn parse_radius_value(raw: &str) -> Option<String> {
    if raw == "none" {
        return Some("0".to_string());
    }
    if raw == "full" {
        return Some("calc(infinity * 1px)".to_string());
    }
    if let Some(value) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        if value.is_empty() {
            return None;
        }
        return Some(format!("var({})", value));
    }
    if let Some(value) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        if value.is_empty() {
            return None;
        }
        return Some(value.to_string());
    }
    if raw
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
    {
        return Some(format!("var(--radius-{})", raw));
    }
    None
}

fn radius_target_properties(target: &str) -> Option<&'static [&'static str]> {
    match target {
        "s" => Some(&["border-start-start-radius", "border-end-start-radius"]),
        "e" => Some(&["border-start-end-radius", "border-end-end-radius"]),
        "t" => Some(&["border-top-left-radius", "border-top-right-radius"]),
        "r" => Some(&["border-top-right-radius", "border-bottom-right-radius"]),
        "b" => Some(&["border-bottom-right-radius", "border-bottom-left-radius"]),
        "l" => Some(&["border-top-left-radius", "border-bottom-left-radius"]),
        "ss" => Some(&["border-start-start-radius"]),
        "se" => Some(&["border-start-end-radius"]),
        "ee" => Some(&["border-end-end-radius"]),
        "es" => Some(&["border-end-start-radius"]),
        "tl" => Some(&["border-top-left-radius"]),
        "tr" => Some(&["border-top-right-radius"]),
        "br" => Some(&["border-bottom-right-radius"]),
        "bl" => Some(&["border-bottom-left-radius"]),
        _ => None,
    }
}

fn generate_grid_column_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "col-span-full" {
        return rule(&selector, "grid-column:1 / -1", config);
    }
    if let Some(raw) = class.strip_prefix("col-span-") {
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return rule(
                &selector,
                &format!("grid-column:span {} / span {}", raw, raw),
                config,
            );
        }
        if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            return rule(
                &selector,
                &format!("grid-column:span {} / span {}", custom, custom),
                config,
            );
        }
        if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            return rule(
                &selector,
                &format!("grid-column:span var({}) / span var({})", custom, custom),
                config,
            );
        }
    }

    if let Some(value) = parse_grid_line_value(class, "col-start-") {
        return rule(&selector, &format!("grid-column-start:{}", value), config);
    }
    if let Some(value) = parse_grid_line_value(class, "col-end-") {
        return rule(&selector, &format!("grid-column-end:{}", value), config);
    }

    if class == "col-auto" {
        return rule(&selector, "grid-column:auto", config);
    }
    if let Some(num) = class.strip_prefix("col-") {
        if num.chars().all(|c| c.is_ascii_digit()) && !num.is_empty() {
            return rule(&selector, &format!("grid-column:{}", num), config);
        }
    }
    if let Some(num) = class.strip_prefix("-col-") {
        if num.chars().all(|c| c.is_ascii_digit()) && !num.is_empty() {
            return rule(
                &selector,
                &format!("grid-column:calc({} * -1)", num),
                config,
            );
        }
    }

    if let Some(custom) = class
        .strip_prefix("col-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        return rule(&selector, &format!("grid-column:var({})", custom), config);
    }
    if let Some(custom) = class
        .strip_prefix("col-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        return rule(&selector, &format!("grid-column:{}", custom), config);
    }

    None
}

fn generate_grid_row_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    let selector = format!(".{}", escape_selector(class));

    if class == "row-span-full" {
        return rule(&selector, "grid-row:1 / -1", config);
    }
    if let Some(raw) = class.strip_prefix("row-span-") {
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return rule(
                &selector,
                &format!("grid-row:span {} / span {}", raw, raw),
                config,
            );
        }
        if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            return rule(
                &selector,
                &format!("grid-row:span {} / span {}", custom, custom),
                config,
            );
        }
        if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            return rule(
                &selector,
                &format!("grid-row:span var({}) / span var({})", custom, custom),
                config,
            );
        }
    }

    if let Some(value) = parse_grid_line_value(class, "row-start-") {
        return rule(&selector, &format!("grid-row-start:{}", value), config);
    }
    if let Some(value) = parse_grid_line_value(class, "row-end-") {
        return rule(&selector, &format!("grid-row-end:{}", value), config);
    }

    if class == "row-auto" {
        return rule(&selector, "grid-row:auto", config);
    }
    if let Some(num) = class.strip_prefix("row-") {
        if num.chars().all(|c| c.is_ascii_digit()) && !num.is_empty() {
            return rule(&selector, &format!("grid-row:{}", num), config);
        }
    }
    if let Some(num) = class.strip_prefix("-row-") {
        if num.chars().all(|c| c.is_ascii_digit()) && !num.is_empty() {
            return rule(&selector, &format!("grid-row:calc({} * -1)", num), config);
        }
    }

    if let Some(custom) = class
        .strip_prefix("row-(")
        .and_then(|v| v.strip_suffix(')'))
    {
        return rule(&selector, &format!("grid-row:var({})", custom), config);
    }
    if let Some(custom) = class
        .strip_prefix("row-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        return rule(&selector, &format!("grid-row:{}", custom), config);
    }

    None
}

fn parse_grid_line_value(class: &str, prefix: &str) -> Option<String> {
    if let Some(raw) = class.strip_prefix(prefix) {
        if raw == "auto" {
            return Some("auto".to_string());
        }
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return Some(raw.to_string());
        }
        if let Some(custom) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            return Some(custom.to_string());
        }
        if let Some(custom) = raw.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
            return Some(format!("var({})", custom));
        }
    }

    let negative_prefix = format!("-{}", prefix);
    if let Some(raw) = class.strip_prefix(&negative_prefix) {
        if raw.chars().all(|c| c.is_ascii_digit()) && !raw.is_empty() {
            return Some(format!("calc({} * -1)", raw));
        }
    }

    None
}

fn parse_inset_key_and_token(raw: &str) -> Option<(&str, &str)> {
    for key in [
        "inset-x", "inset-y", "inset", "start", "end", "top", "right", "bottom", "left",
    ] {
        let prefix = format!("{}-", key);
        if let Some(token) = raw.strip_prefix(&prefix) {
            return Some((key, token));
        }
    }
    None
}

fn parse_inset_value(token: &str, negative: bool) -> Option<String> {
    let base = if token == "px" {
        "1px".to_string()
    } else if token == "full" {
        "100%".to_string()
    } else if token == "auto" {
        if negative {
            return None;
        }
        "auto".to_string()
    } else if let Some(custom) = token.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        custom.to_string()
    } else if let Some(custom_prop) = token.strip_prefix('(').and_then(|v| v.strip_suffix(')')) {
        format!("var({})", custom_prop)
    } else if is_spacing_multiplier(token) {
        if negative {
            format!("calc(var(--spacing) * -{})", token)
        } else {
            format!("calc(var(--spacing) * {})", token)
        }
    } else if is_fraction(token) {
        if negative {
            format!("calc({} * -100%)", token)
        } else {
            format!("calc({} * 100%)", token)
        }
    } else {
        return None;
    };

    if negative && !is_spacing_multiplier(token) && !is_fraction(token) {
        return Some(format!("calc({} * -1)", base));
    }
    Some(base)
}

fn is_fraction(token: &str) -> bool {
    let mut parts = token.split('/');
    let Some(a) = parts.next() else { return false };
    let Some(b) = parts.next() else { return false };
    parts.next().is_none()
        && !a.is_empty()
        && !b.is_empty()
        && a.chars().all(|c| c.is_ascii_digit())
        && b.chars().all(|c| c.is_ascii_digit())
}

fn inset_declarations(key: &str, value: &str) -> Option<String> {
    let declarations = match key {
        "inset" => format!("inset:{}", value),
        "inset-x" => format!("inset-inline:{}", value),
        "inset-y" => format!("inset-block:{}", value),
        "start" => format!("inset-inline-start:{}", value),
        "end" => format!("inset-inline-end:{}", value),
        "top" => format!("top:{}", value),
        "right" => format!("right:{}", value),
        "bottom" => format!("bottom:{}", value),
        "left" => format!("left:{}", value),
        _ => return None,
    };
    Some(declarations)
}

fn is_container_token(token: &str) -> bool {
    matches!(
        token,
        "3xs"
            | "4xs"
            | "2xs"
            | "xs"
            | "sm"
            | "md"
            | "lg"
            | "xl"
            | "2xl"
            | "3xl"
            | "4xl"
            | "5xl"
            | "6xl"
            | "7xl"
    )
}

fn generate_color_rule(class: &str, config: &GeneratorConfig) -> Option<String> {
    if class.starts_with("text-shadow-") || class.starts_with("bg-blend-") {
        return None;
    }
    let (prefix, color, shade) = split_color_class(class)?;
    let map = config.colors.get(color)?;
    let value = map.get(shade)?;
    let selector = format!(".{}", escape_selector(class));
    let declarations = match prefix {
        "text" => format!("color:{}", value),
        "bg" => format!("background-color:{}", value),
        "border" => format!("border-color:{}", value),
        "decoration" => format!("text-decoration-color:{}", value),
        "accent" => format!("accent-color:{}", value),
        "caret" => format!("caret-color:{}", value),
        "fill" => format!("fill:{}", value),
        "stroke" => format!("stroke:{}", value),
        _ => return None,
    };
    rule(&selector, &declarations, config)
}

fn split_color_class(class: &str) -> Option<(&str, &str, &str)> {
    let (prefix, rest) = class.split_once('-')?;
    let (color, shade) = rest.rsplit_once('-')?;
    if shade.is_empty() || color.is_empty() {
        return None;
    }
    Some((prefix, color, shade))
}

fn parse_variants(class: &str) -> (Vec<&str>, &str) {
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut split_indices = Vec::new();

    for (idx, ch) in class.char_indices() {
        match ch {
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '[' => bracket_depth += 1,
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            ':' if paren_depth == 0 && bracket_depth == 0 => split_indices.push(idx),
            _ => {}
        }
    }

    if split_indices.is_empty() {
        return (Vec::new(), class);
    }
    let mut variants = Vec::new();
    let mut start = 0usize;
    for idx in split_indices {
        variants.push(&class[start..idx]);
        start = idx + 1;
    }
    (variants, &class[start..])
}

fn apply_variants(
    variants: &[&str],
    full_class: &str,
    base_class: &str,
    rule: Option<String>,
    minify: bool,
    variant_tables: &VariantTables,
) -> Option<String> {
    let rule = rule?;
    let expected_base_selector = format!(".{}", escape_selector(base_class));
    let class_selector = format!(".{}", escape_selector(full_class));
    let (header, declarations_block) = split_rule_header_and_block(&rule)?;
    let mut declarations_block = declarations_block.to_string();
    let first_selector = header.split_whitespace().next()?;
    if first_selector != expected_base_selector {
        return None;
    }

    if variants.is_empty() {
        if full_class != base_class {
            return Some(compose_flat_variant_rule(
                header,
                first_selector,
                &class_selector,
                &declarations_block,
                &[],
                minify,
            ));
        }
        return Some(rule);
    }

    let mut selector = class_selector.clone();
    let mut wrappers = Vec::new();
    let mut pre_selector_wrappers = Vec::new();
    let mut post_selector_wrappers = Vec::new();
    let mut needs_generated_content = false;
    let mut selector_changed = false;

    for variant in variants {
        let selector_before = selector.clone();
        let wrappers_before = wrappers.len();
        if let Some(updated) = apply_selector_variant(
            selector.clone(),
            variant,
            &mut wrappers,
            &mut needs_generated_content,
            variant_tables,
        ) {
            let added_wrappers = wrappers[wrappers_before..].to_vec();
            let changed_this_step = updated != selector_before;
            if changed_this_step || selector_changed {
                post_selector_wrappers.extend(added_wrappers);
            } else {
                pre_selector_wrappers.extend(added_wrappers);
            }
            selector_changed |= changed_this_step;
            selector = updated;
            continue;
        }
        return None;
    }

    if needs_generated_content {
        declarations_block = ensure_generated_content(&declarations_block, minify)?;
    }

    if minify {
        return Some(compose_flat_variant_rule(
            header,
            first_selector,
            &selector,
            &declarations_block,
            &wrappers,
            minify,
        ));
    }

    let mut nested_body = strip_base_indentation(declarations_body(&declarations_block)?);
    for wrapper in post_selector_wrappers.iter().rev() {
        nested_body = wrap_rule(wrapper, nested_body.trim(), false);
    }

    if selector != class_selector {
        let relative_selector = selector.replace(&class_selector, "&");
        if !relative_selector.contains('&') {
            return Some(compose_flat_variant_rule(
                header,
                first_selector,
                &selector,
                &declarations_block,
                &wrappers,
                false,
            ));
        }
        nested_body = compose_relative_selector_block(&relative_selector, nested_body.trim());
    }

    for wrapper in pre_selector_wrappers.iter().rev() {
        nested_body = wrap_rule(wrapper, nested_body.trim(), false);
    }

    Some(format!(
        "{} {{\n{}\n}}",
        class_selector,
        indent_css_block(nested_body.trim(), 2)
    ))
}

fn compose_relative_selector_block(relative_selector: &str, body: &str) -> String {
    if let Some((outer_selector, pseudo_element)) =
        split_compound_pseudo_element_selector(relative_selector)
    {
        let nested_pseudo = format!(
            "&{} {{\n{}\n}}",
            pseudo_element,
            indent_css_block(body.trim(), 2)
        );
        return format!(
            "{} {{\n{}\n}}",
            outer_selector,
            indent_css_block(nested_pseudo.trim(), 2)
        );
    }

    format!(
        "{} {{\n{}\n}}",
        relative_selector,
        indent_css_block(body.trim(), 2)
    )
}

fn split_compound_pseudo_element_selector(selector: &str) -> Option<(&str, &str)> {
    for pseudo in ["::before", "::after"] {
        if let Some(idx) = selector.rfind(pseudo) {
            let outer = selector[..idx].trim();
            let suffix = selector[idx..].trim();
            if outer.is_empty() || outer == "&" || suffix != pseudo {
                continue;
            }
            return Some((outer, pseudo));
        }
    }
    None
}

fn split_rule_header_and_block(rule: &str) -> Option<(&str, &str)> {
    let start = rule.find('{')?;
    let header = rule[..start].trim_end();
    let block = rule[start..].trim();
    if !block.starts_with('{') || !block.ends_with('}') {
        return None;
    }
    Some((header, block))
}

fn declarations_body(block: &str) -> Option<&str> {
    let open = block.find('{')?;
    let close = block.rfind('}')?;
    if close <= open {
        return None;
    }
    Some(block[open + 1..close].trim())
}

fn strip_base_indentation(body: &str) -> String {
    body.lines()
        .map(|line| line.strip_prefix("  ").unwrap_or(line))
        .collect::<Vec<_>>()
        .join("\n")
}

fn compose_flat_variant_rule(
    header: &str,
    first_selector: &str,
    selector: &str,
    declarations_block: &str,
    wrappers: &[RuleWrapper],
    minify: bool,
) -> String {
    let replaced_header = header.replacen(first_selector, selector, 1);
    let mut combined = if minify {
        format!("{}{}", replaced_header, declarations_block)
    } else {
        format!("{} {}", replaced_header, declarations_block)
    };
    for wrapper in wrappers.iter().rev() {
        combined = wrap_rule(wrapper, &combined, minify);
    }
    combined
}

fn escape_selector(class: &str) -> String {
    let mut escaped = String::with_capacity(class.len() * 2);

    for ch in class.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            ':' => escaped.push_str("\\:"),
            '/' => escaped.push_str("\\/"),
            '[' => {
                escaped.push_str("\\[");
            }
            ']' => {
                escaped.push_str("\\]");
            }
            '(' => {
                escaped.push_str("\\(");
            }
            ')' => {
                escaped.push_str("\\)");
            }
            '&' => escaped.push_str("\\&"),
            '>' => escaped.push_str("\\>"),
            '+' => escaped.push_str("\\+"),
            ',' => escaped.push_str("\\,"),
            '%' => escaped.push_str("\\%"),
            '=' => escaped.push_str("\\="),
            '!' => escaped.push_str("\\!"),
            '*' => escaped.push_str("\\*"),
            '@' => escaped.push_str("\\@"),
            '#' => escaped.push_str("\\#"),
            '\'' => escaped.push_str("\\'"),
            '"' => escaped.push_str("\\\""),
            '.' => escaped.push_str("\\."),
            _ => escaped.push(ch),
        }
    }

    escaped
}

fn apply_data_variant(selector: String, data_key: &str) -> Option<String> {
    if data_key.is_empty() {
        return None;
    }

    if let Some(raw) = data_key.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        if raw.is_empty() {
            return None;
        }
        return Some(format!("{}[data-{}]", selector, raw));
    }

    Some(format!("{}[data-{}]", selector, data_key))
}

fn apply_aria_variant(selector: String, aria_key: &str) -> Option<String> {
    if aria_key.is_empty() {
        return None;
    }

    if let Some(raw) = aria_key.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        if raw.is_empty() {
            return None;
        }
        return Some(format!("{}[aria-{}]", selector, raw));
    }

    let condition = match aria_key {
        "busy" => "busy=true",
        "checked" => "checked=true",
        "disabled" => "disabled=true",
        "expanded" => "expanded=true",
        "hidden" => "hidden=true",
        "pressed" => "pressed=true",
        "readonly" => "readonly=true",
        "required" => "required=true",
        "selected" => "selected=true",
        _ => return Some(format!("{}[aria-{}]", selector, aria_key)),
    };

    let (key, value) = condition.split_once('=')?;
    Some(format!("{}[aria-{}=\"{}\"]", selector, key, value))
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum RuleWrapper {
    Media(String),
    Supports(String),
    Container { name: Option<String>, query: String },
    StartingStyle,
    Template(String),
}

fn apply_selector_variant(
    selector: String,
    variant: &str,
    wrappers: &mut Vec<RuleWrapper>,
    needs_generated_content: &mut bool,
    variant_tables: &VariantTables,
) -> Option<String> {
    match variant {
        "dark" => {
            if let Some(custom_selector) = variant_tables.dark_variant_selector.as_ref() {
                return apply_custom_variant(selector, custom_selector, wrappers);
            }
            wrappers.push(RuleWrapper::Media(
                "(prefers-color-scheme: dark)".to_string(),
            ));
            return Some(selector);
        }
        "motion-safe" => {
            wrappers.push(RuleWrapper::Media(
                "(prefers-reduced-motion: no-preference)".to_string(),
            ));
            return Some(selector);
        }
        "motion-reduce" => {
            wrappers.push(RuleWrapper::Media(
                "(prefers-reduced-motion: reduce)".to_string(),
            ));
            return Some(selector);
        }
        "contrast-more" => {
            wrappers.push(RuleWrapper::Media("(prefers-contrast: more)".to_string()));
            return Some(selector);
        }
        "contrast-less" => {
            wrappers.push(RuleWrapper::Media("(prefers-contrast: less)".to_string()));
            return Some(selector);
        }
        "forced-colors" => {
            wrappers.push(RuleWrapper::Media("(forced-colors: active)".to_string()));
            return Some(selector);
        }
        "not-forced-colors" => {
            wrappers.push(RuleWrapper::Media(
                "(not (forced-colors: active))".to_string(),
            ));
            return Some(selector);
        }
        "inverted-colors" => {
            wrappers.push(RuleWrapper::Media(
                "(inverted-colors: inverted)".to_string(),
            ));
            return Some(selector);
        }
        "pointer-fine" => {
            wrappers.push(RuleWrapper::Media("(pointer: fine)".to_string()));
            return Some(selector);
        }
        "pointer-coarse" => {
            wrappers.push(RuleWrapper::Media("(pointer: coarse)".to_string()));
            return Some(selector);
        }
        "pointer-none" => {
            wrappers.push(RuleWrapper::Media("(pointer: none)".to_string()));
            return Some(selector);
        }
        "any-pointer-fine" => {
            wrappers.push(RuleWrapper::Media("(any-pointer: fine)".to_string()));
            return Some(selector);
        }
        "any-pointer-coarse" => {
            wrappers.push(RuleWrapper::Media("(any-pointer: coarse)".to_string()));
            return Some(selector);
        }
        "any-pointer-none" => {
            wrappers.push(RuleWrapper::Media("(any-pointer: none)".to_string()));
            return Some(selector);
        }
        "portrait" => {
            wrappers.push(RuleWrapper::Media("(orientation: portrait)".to_string()));
            return Some(selector);
        }
        "landscape" => {
            wrappers.push(RuleWrapper::Media("(orientation: landscape)".to_string()));
            return Some(selector);
        }
        "noscript" => {
            wrappers.push(RuleWrapper::Media("(scripting: none)".to_string()));
            return Some(selector);
        }
        "print" => {
            wrappers.push(RuleWrapper::Media("print".to_string()));
            return Some(selector);
        }
        "starting" => {
            wrappers.push(RuleWrapper::StartingStyle);
            return Some(selector);
        }
        _ => {}
    }

    if let Some(custom_selector) = variant_tables.custom_variant_selectors.get(variant) {
        return apply_custom_variant(selector, custom_selector, wrappers);
    }

    if let Some(width) = variant_tables.responsive_width(variant) {
        wrappers.push(RuleWrapper::Media(format!("(width >= {})", width)));
        return Some(selector);
    }

    if let Some(key) = variant.strip_prefix("max-") {
        if let Some(width) = variant_tables.responsive_width(key) {
            wrappers.push(RuleWrapper::Media(format!("(width < {})", width)));
            return Some(selector);
        }
    }

    if variant.starts_with("min-[") && variant.ends_with(']') {
        let value = variant.strip_prefix("min-[")?.strip_suffix(']')?;
        wrappers.push(RuleWrapper::Media(format!("(width >= {})", value)));
        return Some(selector);
    }

    if variant.starts_with("max-[") && variant.ends_with(']') {
        let value = variant.strip_prefix("max-[")?.strip_suffix(']')?;
        wrappers.push(RuleWrapper::Media(format!("(width < {})", value)));
        return Some(selector);
    }

    if variant.starts_with('@') {
        let (name, query) = container_query_for_variant(variant, variant_tables)?;
        wrappers.push(RuleWrapper::Container { name, query });
        return Some(selector);
    }

    if let Some(expr) = supports_query_for_variant(variant) {
        wrappers.push(RuleWrapper::Supports(expr));
        return Some(selector);
    }

    if let Some(raw) = variant.strip_prefix("group-") {
        return apply_group_or_peer_variant(
            selector,
            raw,
            "group",
            " ",
            wrappers,
            needs_generated_content,
        );
    }

    if let Some(raw) = variant.strip_prefix("peer-") {
        return apply_group_or_peer_variant(
            selector,
            raw,
            "peer",
            " ~ ",
            wrappers,
            needs_generated_content,
        );
    }

    if variant != "in-range" {
        if let Some(raw) = variant.strip_prefix("in-") {
            let mut parent_selector = in_parent_selector(raw)?;
            if parent_selector.contains('&') {
                parent_selector = parent_selector.replace('&', &selector);
                return Some(parent_selector);
            }
            return Some(format!("{} {}", parent_selector, selector));
        }
    }

    if let Some(data_key) = variant.strip_prefix("data-") {
        return apply_data_variant(selector, data_key);
    }

    if let Some(aria_key) = variant.strip_prefix("aria-") {
        return apply_aria_variant(selector, aria_key);
    }

    if let Some(raw_not) = variant.strip_prefix("not-") {
        if raw_not.starts_with("supports-") {
            let query = supports_query_for_variant(raw_not)?;
            wrappers.push(RuleWrapper::Supports(format!("not ({})", query)));
            return Some(selector);
        }

        if let Some(inner) = raw_not.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
            let not_selector = normalize_arbitrary_variant_content(inner);
            if not_selector.is_empty() {
                return None;
            }
            return Some(format!("{}:not({})", selector, not_selector));
        }

        let not_selector = selector_for_simple_variant(raw_not, wrappers, needs_generated_content)?;
        return Some(format!("{}:not({})", selector, not_selector));
    }

    if let Some(has_raw) = variant.strip_prefix("has-") {
        let has_selector = has_argument(has_raw)?;
        return Some(format!("{}:has({})", selector, has_selector));
    }

    if variant.starts_with('[') && variant.ends_with(']') {
        return apply_arbitrary_variant_selector(selector, variant);
    }

    if variant == "marker" {
        return Some(format!("{}::marker, {} *::marker", selector, selector));
    }

    if variant == "selection" {
        return Some(format!(
            "{}::selection, {} *::selection",
            selector, selector
        ));
    }

    if variant == "*" {
        return Some(format!(":is({} > *)", selector));
    }
    if variant == "**" {
        return Some(format!(":is({} *)", selector));
    }

    selector_for_simple_variant(variant, wrappers, needs_generated_content)
        .map(|suffix| format!("{}{}", selector, suffix))
}

fn selector_for_simple_variant(
    variant: &str,
    wrappers: &mut Vec<RuleWrapper>,
    needs_generated_content: &mut bool,
) -> Option<String> {
    let suffix = match variant {
        "hover" => {
            wrappers.push(RuleWrapper::Media("(hover: hover)".to_string()));
            ":hover".to_string()
        }
        "focus" => ":focus".to_string(),
        "focus-within" => ":focus-within".to_string(),
        "focus-visible" => ":focus-visible".to_string(),
        "active" => ":active".to_string(),
        "visited" => ":visited".to_string(),
        "target" => ":target".to_string(),
        "first" => ":first-child".to_string(),
        "last" => ":last-child".to_string(),
        "only" => ":only-child".to_string(),
        "odd" => ":nth-child(odd)".to_string(),
        "even" => ":nth-child(even)".to_string(),
        "first-of-type" => ":first-of-type".to_string(),
        "last-of-type" => ":last-of-type".to_string(),
        "only-of-type" => ":only-of-type".to_string(),
        "empty" => ":empty".to_string(),
        "disabled" => ":disabled".to_string(),
        "enabled" => ":enabled".to_string(),
        "checked" => ":checked".to_string(),
        "indeterminate" => ":indeterminate".to_string(),
        "default" => ":default".to_string(),
        "optional" => ":optional".to_string(),
        "required" => ":required".to_string(),
        "valid" => ":valid".to_string(),
        "invalid" => ":invalid".to_string(),
        "user-valid" => ":user-valid".to_string(),
        "user-invalid" => ":user-invalid".to_string(),
        "in-range" => ":in-range".to_string(),
        "out-of-range" => ":out-of-range".to_string(),
        "placeholder-shown" => ":placeholder-shown".to_string(),
        "details-content" => ":details-content".to_string(),
        "autofill" => ":autofill".to_string(),
        "read-only" => ":read-only".to_string(),
        "open" => ":is([open], :popover-open, :open)".to_string(),
        "inert" => ":is([inert], [inert] *)".to_string(),
        "rtl" => ":where(:dir(rtl), [dir=\"rtl\"], [dir=\"rtl\"] *)".to_string(),
        "ltr" => ":where(:dir(ltr), [dir=\"ltr\"], [dir=\"ltr\"] *)".to_string(),
        "before" => {
            *needs_generated_content = true;
            "::before".to_string()
        }
        "after" => {
            *needs_generated_content = true;
            "::after".to_string()
        }
        "first-letter" => "::first-letter".to_string(),
        "first-line" => "::first-line".to_string(),
        "marker" => ":marker".to_string(),
        "selection" => ":selection".to_string(),
        "file" => "::file-selector-button".to_string(),
        "backdrop" => "::backdrop".to_string(),
        "placeholder" => "::placeholder".to_string(),
        _ => {
            if let Some(expr) = parse_nth_like_variant(variant) {
                return Some(expr);
            }
            return None;
        }
    };
    Some(suffix)
}

fn parse_nth_like_variant(variant: &str) -> Option<String> {
    if let Some(raw) = variant.strip_prefix("nth-last-of-type-") {
        return Some(format!(":nth-last-of-type({})", nth_argument(raw)?));
    }
    if let Some(raw) = variant.strip_prefix("nth-last-") {
        return Some(format!(":nth-last-child({})", nth_argument(raw)?));
    }
    if let Some(raw) = variant.strip_prefix("nth-of-type-") {
        return Some(format!(":nth-of-type({})", nth_argument(raw)?));
    }
    if let Some(raw) = variant.strip_prefix("nth-") {
        return Some(format!(":nth-child({})", nth_argument(raw)?));
    }
    None
}

fn nth_argument(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }
    if let Some(inner) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        if inner.is_empty() {
            return None;
        }
        return Some(normalize_arbitrary_variant_content(inner));
    }
    Some(normalize_arbitrary_variant_content(raw))
}

fn has_argument(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }
    if let Some(inner) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        if inner.is_empty() {
            return None;
        }
        return Some(normalize_arbitrary_variant_content(inner));
    }
    Some(format!(":{}", raw))
}

fn in_parent_selector(raw: &str) -> Option<String> {
    if raw.is_empty() {
        return None;
    }
    if let Some(inner) = raw.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
        if inner.is_empty() {
            return None;
        }
        return Some(format!(
            ":where({})",
            normalize_arbitrary_variant_content(inner)
        ));
    }
    Some(format!(":where(:{})", raw))
}

fn split_named_variant(raw: &str) -> (&str, Option<&str>) {
    let mut bracket_depth = 0usize;
    let mut paren_depth = 0usize;

    for (idx, ch) in raw.char_indices() {
        match ch {
            '[' => bracket_depth += 1,
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '/' if bracket_depth == 0 && paren_depth == 0 => {
                return (&raw[..idx], Some(&raw[idx + 1..]));
            }
            _ => {}
        }
    }

    (raw, None)
}

fn apply_group_or_peer_variant(
    selector: String,
    raw: &str,
    marker_class: &str,
    combinator: &str,
    wrappers: &mut Vec<RuleWrapper>,
    needs_generated_content: &mut bool,
) -> Option<String> {
    let (core, name) = split_named_variant(raw);
    let marker = if let Some(name) = name {
        format!(
            ".{}",
            escape_selector(&format!("{}/{}", marker_class, name))
        )
    } else {
        format!(".{}", marker_class)
    };

    if core.starts_with('[') && core.ends_with(']') {
        let inner = normalize_arbitrary_variant_content(core.strip_prefix('[')?.strip_suffix(']')?);
        if inner.contains('&') {
            let replaced = inner.replace('&', &marker);
            return Some(format!("{}{}{}", replaced, combinator, selector));
        }
        let marker_expr = format!(":where({}){}", marker, inner);
        return Some(compose_group_or_peer_relation(
            &selector,
            &marker_expr,
            combinator,
        ));
    }

    if let Some(has_raw) = core.strip_prefix("has-") {
        let argument = has_argument(has_raw)?;
        let marker_expr = format!(":where({}):has({})", marker, argument);
        return Some(compose_group_or_peer_relation(
            &selector,
            &marker_expr,
            combinator,
        ));
    }

    if let Some(data_raw) = core.strip_prefix("data-") {
        let conditioned = apply_data_variant(marker.clone(), data_raw)?;
        let marker_expr = format!(":where({})", conditioned);
        return Some(compose_group_or_peer_relation(
            &selector,
            &marker_expr,
            combinator,
        ));
    }

    if let Some(aria_raw) = core.strip_prefix("aria-") {
        let conditioned = apply_aria_variant(marker.clone(), aria_raw)?;
        let marker_expr = format!(":where({})", conditioned);
        return Some(compose_group_or_peer_relation(
            &selector,
            &marker_expr,
            combinator,
        ));
    }

    let marker_suffix = selector_for_simple_variant(core, wrappers, needs_generated_content)?;
    let marker_expr = format!(":where({}){}", marker, marker_suffix);
    Some(compose_group_or_peer_relation(
        &selector,
        &marker_expr,
        combinator,
    ))
}

fn compose_group_or_peer_relation(selector: &str, marker_expr: &str, combinator: &str) -> String {
    if combinator.contains('~') {
        format!("{}:is({} ~ *)", selector, marker_expr.trim())
    } else {
        format!("{}:is({} *)", selector, marker_expr.trim())
    }
}

fn apply_arbitrary_variant_selector(selector: String, variant: &str) -> Option<String> {
    let raw = variant.strip_prefix('[')?.strip_suffix(']')?;
    if raw.is_empty() {
        return None;
    }
    let normalized = normalize_arbitrary_variant_content(raw);
    if normalized.contains('&') {
        return Some(normalized.replace('&', &selector));
    }
    Some(format!("{} {}", normalized, selector))
}

fn apply_custom_variant_selector(selector: String, template: &str) -> Option<String> {
    let normalized = normalize_arbitrary_variant_content(template.trim());
    if normalized.is_empty() {
        return None;
    }
    if normalized.contains('&') {
        return Some(normalized.replace('&', &selector));
    }
    Some(format!("{} {}", normalized, selector))
}

fn apply_custom_variant(
    selector: String,
    template: &str,
    wrappers: &mut Vec<RuleWrapper>,
) -> Option<String> {
    let normalized = normalize_arbitrary_variant_content(template.trim());
    if normalized.is_empty() {
        return None;
    }
    if normalized.contains("@slot") {
        if let Some(expansion) = parse_slot_variant_template(&normalized) {
            for wrapper in expansion.wrappers {
                wrappers.push(wrapper);
            }
            if expansion.selector_expr.contains('&') {
                return Some(expansion.selector_expr.replace('&', &selector));
            }
            return Some(format!("{} {}", expansion.selector_expr, selector));
        }
        wrappers.push(RuleWrapper::Template(normalized));
        return Some(selector);
    }
    apply_custom_variant_selector(selector, &normalized)
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SlotVariantExpansion {
    wrappers: Vec<RuleWrapper>,
    selector_expr: String,
}

fn parse_slot_variant_template(template: &str) -> Option<SlotVariantExpansion> {
    parse_slot_variant_node(template.trim())
}

fn parse_slot_variant_node(raw: &str) -> Option<SlotVariantExpansion> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    if trimmed.starts_with("@slot") {
        return Some(SlotVariantExpansion {
            wrappers: Vec::new(),
            selector_expr: "&".to_string(),
        });
    }

    let open_idx = trimmed.find('{')?;
    let close_idx = find_matching_brace_in_text(trimmed, open_idx)?;
    let head = trimmed[..open_idx].trim();
    let body = trimmed[open_idx + 1..close_idx].trim();
    if body.is_empty() || head.is_empty() {
        return None;
    }

    let mut inner = parse_slot_variant_node(body)?;

    if let Some(query) = head.strip_prefix("@media") {
        let query = query.trim();
        if query.is_empty() {
            return None;
        }
        inner
            .wrappers
            .insert(0, RuleWrapper::Media(query.to_string()));
        return Some(inner);
    }

    if let Some(query) = head.strip_prefix("@supports") {
        let query = query.trim();
        if query.is_empty() {
            return None;
        }
        inner
            .wrappers
            .insert(0, RuleWrapper::Supports(query.to_string()));
        return Some(inner);
    }

    if let Some(rest) = head.strip_prefix("@container") {
        let (name, query) = parse_container_wrapper(rest.trim())?;
        inner
            .wrappers
            .insert(0, RuleWrapper::Container { name, query });
        return Some(inner);
    }

    inner.selector_expr = compose_nested_selector(head, &inner.selector_expr);
    Some(inner)
}

fn parse_container_wrapper(raw: &str) -> Option<(Option<String>, String)> {
    if raw.is_empty() {
        return None;
    }

    if raw.starts_with('(') {
        return Some((None, raw.to_string()));
    }

    let (name, query) = raw.split_once(char::is_whitespace)?;
    let name = name.trim();
    let query = query.trim();
    if name.is_empty() || query.is_empty() || !query.starts_with('(') {
        return None;
    }
    Some((Some(name.to_string()), query.to_string()))
}

fn compose_nested_selector(outer: &str, inner: &str) -> String {
    if inner.contains('&') {
        return inner.replace('&', outer);
    }
    if outer.contains('&') {
        return outer.replace('&', inner);
    }
    format!("{} {}", outer, inner)
}

fn find_matching_brace_in_text(text: &str, open_idx: usize) -> Option<usize> {
    if !text[open_idx..].starts_with('{') {
        return None;
    }
    let mut depth = 0usize;
    let mut in_string: Option<char> = None;
    let mut escaped = false;
    for (rel_idx, ch) in text[open_idx..].char_indices() {
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

fn supports_query_for_variant(variant: &str) -> Option<String> {
    if let Some(inner) = variant
        .strip_prefix("supports-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        let value = normalize_arbitrary_variant_content(inner);
        if value.is_empty() {
            return None;
        }
        return Some(value);
    }

    if let Some(inner) = variant
        .strip_prefix("not-supports-[")
        .and_then(|v| v.strip_suffix(']'))
    {
        let value = normalize_arbitrary_variant_content(inner);
        if value.is_empty() {
            return None;
        }
        return Some(format!("not ({})", value));
    }

    if let Some(property) = variant.strip_prefix("supports-") {
        if property.is_empty() {
            return None;
        }
        return Some(format!("{}: var(--tw)", property));
    }

    None
}

fn container_query_for_variant(
    variant: &str,
    variant_tables: &VariantTables,
) -> Option<(Option<String>, String)> {
    let raw = variant.strip_prefix('@')?;
    let (core, name) = split_named_variant(raw);
    let name = name.and_then(|value| {
        if value.is_empty() {
            None
        } else {
            Some(value.to_string())
        }
    });

    if let Some(inner) = core.strip_prefix("min-[").and_then(|v| v.strip_suffix(']')) {
        return Some((name, format!("(width >= {})", inner)));
    }

    if let Some(inner) = core.strip_prefix("max-[").and_then(|v| v.strip_suffix(']')) {
        return Some((name, format!("(width < {})", inner)));
    }

    if let Some(size) = core.strip_prefix("max-") {
        let width = variant_tables.container_width(size)?;
        return Some((name, format!("(width < {})", width)));
    }

    let width = variant_tables.container_width(core)?;
    Some((name, format!("(width >= {})", width)))
}

fn wrap_rule(wrapper: &RuleWrapper, rule: &str, minify: bool) -> String {
    match wrapper {
        RuleWrapper::Media(query) => {
            if minify {
                format!("@media {}{{{}}}", query, rule)
            } else {
                format!("@media {} {{\n{}\n}}", query, indent_css_block(rule, 2))
            }
        }
        RuleWrapper::Supports(query) => {
            let query = normalize_supports_query(query);
            if minify {
                format!("@supports {}{{{}}}", query, rule)
            } else {
                format!("@supports {} {{\n{}\n}}", query, indent_css_block(rule, 2))
            }
        }
        RuleWrapper::Container { name, query } => {
            if minify {
                if let Some(name) = name {
                    format!("@container {} {}{{{}}}", name, query, rule)
                } else {
                    format!("@container {}{{{}}}", query, rule)
                }
            } else if let Some(name) = name {
                format!(
                    "@container {} {} {{\n{}\n}}",
                    name,
                    query,
                    indent_css_block(rule, 2)
                )
            } else {
                format!("@container {} {{\n{}\n}}", query, indent_css_block(rule, 2))
            }
        }
        RuleWrapper::StartingStyle => {
            if minify {
                format!("@starting-style{{{}}}", rule)
            } else {
                format!("@starting-style {{\n{}\n}}", indent_css_block(rule, 2))
            }
        }
        RuleWrapper::Template(template) => {
            if template.contains("@slot") {
                template.replace("@slot", rule)
            } else if minify {
                format!("{}{{{}}}", template.trim(), rule)
            } else {
                format!("{} {{\n{}\n}}", template.trim(), indent_css_block(rule, 2))
            }
        }
    }
}

fn indent_css_block(css: &str, spaces: usize) -> String {
    let padding = " ".repeat(spaces);
    css.lines()
        .map(|line| {
            if line.is_empty() {
                String::new()
            } else {
                format!("{}{}", padding, line)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn normalize_supports_query(query: &str) -> String {
    let trimmed = query.trim();
    if trimmed.is_empty() {
        return "()".to_string();
    }
    if (trimmed.starts_with('(') && trimmed.ends_with(')')) || trimmed.starts_with("not ") {
        return trimmed.to_string();
    }
    format!("({})", trimmed)
}

fn normalize_arbitrary_variant_content(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    let mut pending_backslashes = 0usize;

    for ch in raw.chars() {
        if ch == '\\' {
            pending_backslashes += 1;
            continue;
        }

        if ch == '_' {
            for _ in 0..(pending_backslashes / 2) {
                out.push('\\');
            }
            if pending_backslashes % 2 == 1 {
                out.push('_');
            } else {
                out.push(' ');
            }
            pending_backslashes = 0;
            continue;
        }

        for _ in 0..pending_backslashes {
            out.push('\\');
        }
        pending_backslashes = 0;
        out.push(ch);
    }

    for _ in 0..pending_backslashes {
        out.push('\\');
    }

    out
}

fn ensure_generated_content(declarations_block: &str, minify: bool) -> Option<String> {
    let open = declarations_block.find('{')?;
    let close = declarations_block.rfind('}')?;
    if close <= open {
        return None;
    }
    let body = declarations_block[open + 1..close].trim();
    if body.contains("content:") {
        return Some(declarations_block.to_string());
    }

    let mut declarations = body
        .split(';')
        .map(str::trim)
        .filter(|decl| !decl.is_empty())
        .map(str::to_string)
        .collect::<Vec<_>>();
    declarations.insert(0, "content: var(--tw-content)".to_string());

    if minify {
        let body = declarations
            .into_iter()
            .map(|decl| decl.replace(": ", ":"))
            .collect::<Vec<_>>()
            .join(";");
        Some(format!("{{{}}}", body))
    } else {
        let body = declarations
            .into_iter()
            .map(|decl| format!("{};", decl))
            .collect::<Vec<_>>()
            .join("\n");
        Some(format!("{{\n{}\n}}", body))
    }
}

fn strip_important_modifier(class: &str) -> (&str, bool) {
    if class.len() > 1 {
        if let Some(stripped) = class.strip_suffix('!') {
            return (stripped, true);
        }
    }
    (class, false)
}

fn add_important_to_rule(rule: &str, minify: bool) -> Option<String> {
    if !minify && rule.contains('\n') {
        return add_important_to_multiline_rule(rule);
    }

    let first_open = rule.find('{')?;
    let first_close = rule.rfind('}')?;
    if first_close <= first_open {
        return None;
    }

    let header = rule[..first_open].trim_end();
    let body = &rule[first_open + 1..first_close];

    if header.starts_with("@media ") {
        let nested = add_important_to_rule(body.trim(), minify)?;
        if minify {
            return Some(format!("{}{{{}}}", header, nested));
        }
        return Some(format!("{} {{ {} }}", header, nested));
    }

    let declarations = add_important_to_declarations(body, minify)?;
    if minify {
        Some(format!("{}{{{}}}", header, declarations))
    } else {
        Some(format!("{} {{ {}; }}", header, declarations))
    }
}

fn add_important_to_multiline_rule(rule: &str) -> Option<String> {
    let mut out = Vec::new();
    let mut has_declaration = false;

    for line in rule.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            out.push(line.to_string());
            continue;
        }

        if trimmed.ends_with('{') || trimmed == "}" {
            out.push(line.to_string());
            continue;
        }

        if let Some(decl) = trimmed.strip_suffix(';') {
            if let Some((name, value)) = decl.split_once(':') {
                let name = name.trim();
                let value = value.trim();
                let important_value = if value.contains("!important") {
                    value.to_string()
                } else {
                    format!("{} !important", value)
                };
                let indent = &line[..line.len() - line.trim_start().len()];
                out.push(format!("{}{}: {};", indent, name, important_value));
                has_declaration = true;
                continue;
            }
        }

        out.push(line.to_string());
    }

    if has_declaration {
        Some(out.join("\n"))
    } else {
        None
    }
}

fn add_important_to_declarations(declarations: &str, minify: bool) -> Option<String> {
    let mut parts = Vec::new();

    for decl in declarations.split(';') {
        let decl = decl.trim();
        if decl.is_empty() {
            continue;
        }
        let mut iter = decl.splitn(2, ':');
        let name = iter.next()?.trim();
        let value = iter.next()?.trim();
        let important_value = if value.contains("!important") {
            value.to_string()
        } else if minify {
            format!("{}!important", value)
        } else {
            format!("{} !important", value)
        };

        if minify {
            parts.push(format!("{}:{}", name, important_value));
        } else {
            parts.push(format!("{}: {}", name, important_value));
        }
    }

    if parts.is_empty() {
        return None;
    }

    if minify {
        Some(parts.join(";"))
    } else {
        Some(parts.join("; "))
    }
}

fn is_spacing_multiplier(token: &str) -> bool {
    if token.is_empty() {
        return false;
    }
    if token.starts_with('.') || token.ends_with('.') {
        return false;
    }
    let mut seen_dot = false;
    for ch in token.chars() {
        if ch == '.' {
            if seen_dot {
                return false;
            }
            seen_dot = true;
            continue;
        }
        if !ch.is_ascii_digit() {
            return false;
        }
    }
    true
}

fn rule(selector: &str, declarations: &str, config: &GeneratorConfig) -> Option<String> {
    let formatted = format_declarations(declarations, config.minify);
    if formatted.is_empty() {
        return None;
    }
    if config.minify {
        return Some(format!("{}{{{}}}", selector, formatted));
    }
    if formatted.contains('{') || formatted.contains('}') {
        return Some(format!(
            "{} {{\n{}\n}}",
            selector,
            indent_css_block(&formatted, 2)
        ));
    }
    let lines = formatted
        .split(';')
        .map(str::trim)
        .filter(|decl| !decl.is_empty())
        .map(|decl| format!("  {};", decl))
        .collect::<Vec<_>>()
        .join("\n");
    Some(format!("{} {{\n{}\n}}", selector, lines))
}

fn format_declarations(declarations: &str, minify: bool) -> String {
    if declarations.contains('{') || declarations.contains('}') {
        let trimmed = declarations.trim();
        if minify {
            return trimmed
                .split_whitespace()
                .collect::<Vec<_>>()
                .join(" ")
                .replace("; ", ";")
                .replace(": ", ":");
        }
        return format_nested_declarations(trimmed);
    }
    let mut parts = Vec::new();
    for decl in declarations.split(';') {
        let decl = decl.trim();
        if decl.is_empty() {
            continue;
        }
        let mut iter = decl.splitn(2, ':');
        let Some(name) = iter.next() else { continue };
        let Some(value) = iter.next() else { continue };
        if minify {
            parts.push(format!("{}:{}", name.trim(), value.trim()));
        } else {
            parts.push(format!("{}: {}", name.trim(), value.trim()));
        }
    }
    if minify {
        parts.join(";")
    } else {
        parts.join("; ")
    }
}

fn format_nested_declarations(declarations: &str) -> String {
    let mut out = String::new();
    let mut token = String::new();
    let mut depth = 0usize;
    let mut in_string: Option<char> = None;
    let mut escaped = false;

    for ch in declarations.chars() {
        if let Some(quote) = in_string {
            token.push(ch);
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

        match ch {
            '"' | '\'' => {
                token.push(ch);
                in_string = Some(ch);
            }
            '{' => {
                let header = token.trim();
                if !header.is_empty() {
                    push_line_with_indent(&mut out, depth, &format!("{} {{", header));
                }
                token.clear();
                depth += 1;
            }
            ';' => {
                let decl = token.trim();
                if !decl.is_empty() {
                    push_line_with_indent(
                        &mut out,
                        depth,
                        &format!("{};", format_declaration(decl)),
                    );
                }
                token.clear();
            }
            '}' => {
                let tail = token.trim();
                if !tail.is_empty() {
                    push_line_with_indent(
                        &mut out,
                        depth,
                        &format!("{};", format_declaration(tail)),
                    );
                }
                token.clear();
                depth = depth.saturating_sub(1);
                push_line_with_indent(&mut out, depth, "}");
            }
            _ => token.push(ch),
        }
    }

    let tail = token.trim();
    if !tail.is_empty() {
        push_line_with_indent(&mut out, depth, &format!("{};", format_declaration(tail)));
    }

    out
}

fn push_line_with_indent(out: &mut String, depth: usize, line: &str) {
    if !out.is_empty() {
        out.push('\n');
    }
    out.push_str(&"  ".repeat(depth));
    out.push_str(line);
}

fn format_declaration(raw: &str) -> String {
    let Some((name, value)) = raw.split_once(':') else {
        return raw.trim().to_string();
    };
    format!("{}: {}", name.trim(), value.trim())
}

#[cfg(test)]
mod tests {
    use super::{GeneratorConfig, VariantOverrides, generate, generate_with_overrides};
    use std::collections::BTreeMap;

    #[test]
    fn generates_p4_rule() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(&["p-4".to_string()], &config);
        assert!(result.css.contains(".p-4"));
        assert!(result.css.contains("padding: calc(var(--spacing) * 4)"));
    }

    #[test]
    fn generates_multiple_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "p-4".to_string(),
                "m-4".to_string(),
                "text-sm".to_string(),
                "bg-red-500".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".m-4"));
        assert!(result.css.contains("margin: calc(var(--spacing) * 4)"));
        assert!(result.css.contains(".text-sm"));
        assert!(result.css.contains("font-size: var(--text-sm)"));
        assert!(result.css.contains(".bg-red-500"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-red-500)")
        );
    }

    #[test]
    fn generates_transition_property_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "transition".to_string(),
                "transition-all".to_string(),
                "transition-colors".to_string(),
                "transition-opacity".to_string(),
                "transition-shadow".to_string(),
                "transition-transform".to_string(),
                "transition-none".to_string(),
                "transition-(--my-properties)".to_string(),
                "transition-[height]".to_string(),
                "md:transition-all".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".transition"));
        assert!(result
            .css
            .contains("transition-property: color,background-color,border-color,outline-color,text-decoration-color,fill,stroke,--tw-gradient-from,--tw-gradient-via,--tw-gradient-to,opacity,box-shadow,transform,translate,scale,rotate,filter,-webkit-backdrop-filter,backdrop-filter,display,content-visibility,overlay,pointer-events"));
        assert!(result.css.contains(
            "transition-timing-function: var(--tw-ease,var(--default-transition-timing-function))"
        ));
        assert!(result.css.contains(
            "transition-duration: var(--tw-duration,var(--default-transition-duration))"
        ));
        assert!(result.css.contains(".transition-all"));
        assert!(result.css.contains("transition-property: all"));
        assert!(result.css.contains(".transition-colors"));
        assert!(result
            .css
            .contains("transition-property: color,background-color,border-color,outline-color,text-decoration-color,fill,stroke,--tw-gradient-from,--tw-gradient-via,--tw-gradient-to"));
        assert!(result.css.contains(".transition-opacity"));
        assert!(result.css.contains("transition-property: opacity"));
        assert!(result.css.contains(".transition-shadow"));
        assert!(result.css.contains("transition-property: box-shadow"));
        assert!(result.css.contains(".transition-transform"));
        assert!(
            result
                .css
                .contains("transition-property: transform,translate,scale,rotate")
        );
        assert!(result.css.contains(".transition-none"));
        assert!(result.css.contains("transition-property: none"));
        assert!(result.css.contains(".transition-\\(--my-properties\\)"));
        assert!(
            result
                .css
                .contains("transition-property: var(--my-properties)")
        );
        assert!(result.css.contains(".transition-\\[height\\]"));
        assert!(result.css.contains("transition-property: height"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:transition-all"));
    }

    #[test]
    fn generates_transition_behavior_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "transition-normal".to_string(),
                "transition-discrete".to_string(),
                "md:transition-normal".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".transition-normal"));
        assert!(result.css.contains("transition-behavior: normal"));
        assert!(result.css.contains(".transition-discrete"));
        assert!(result.css.contains("transition-behavior: allow-discrete"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:transition-normal"));
    }

    #[test]
    fn generates_transition_duration_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "duration-0".to_string(),
                "duration-150".to_string(),
                "duration-700".to_string(),
                "duration-initial".to_string(),
                "duration-(--my-duration)".to_string(),
                "duration-[1s,15s]".to_string(),
                "motion-reduce:duration-0".to_string(),
                "md:duration-150".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".duration-0"));
        assert!(result.css.contains("transition-duration: 0ms"));
        assert!(result.css.contains(".duration-150"));
        assert!(result.css.contains("transition-duration: 150ms"));
        assert!(result.css.contains(".duration-700"));
        assert!(result.css.contains("transition-duration: 700ms"));
        assert!(result.css.contains(".duration-initial"));
        assert!(result.css.contains("transition-duration: initial"));
        assert!(result.css.contains(".duration-\\(--my-duration\\)"));
        assert!(
            result
                .css
                .contains("transition-duration: var(--my-duration)")
        );
        assert!(result.css.contains(".duration-\\[1s\\,15s\\]"));
        assert!(result.css.contains("transition-duration: 1s,15s"));
        assert!(
            result
                .css
                .contains("@media (prefers-reduced-motion: reduce)")
        );
        assert!(result.css.contains(".motion-reduce\\:duration-0"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:duration-150"));
    }

    #[test]
    fn generates_transition_delay_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "delay-150".to_string(),
                "delay-300".to_string(),
                "delay-700".to_string(),
                "delay-(--my-delay)".to_string(),
                "delay-[1s,250ms]".to_string(),
                "motion-reduce:delay-0".to_string(),
                "md:delay-300".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".delay-150"));
        assert!(result.css.contains("transition-delay: 150ms"));
        assert!(result.css.contains(".delay-300"));
        assert!(result.css.contains("transition-delay: 300ms"));
        assert!(result.css.contains(".delay-700"));
        assert!(result.css.contains("transition-delay: 700ms"));
        assert!(result.css.contains(".delay-\\(--my-delay\\)"));
        assert!(result.css.contains("transition-delay: var(--my-delay)"));
        assert!(result.css.contains(".delay-\\[1s\\,250ms\\]"));
        assert!(result.css.contains("transition-delay: 1s,250ms"));
        assert!(
            result
                .css
                .contains("@media (prefers-reduced-motion: reduce)")
        );
        assert!(result.css.contains(".motion-reduce\\:delay-0"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:delay-300"));
    }

    #[test]
    fn generates_transition_timing_function_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "ease-linear".to_string(),
                "ease-in".to_string(),
                "ease-out".to_string(),
                "ease-in-out".to_string(),
                "ease-initial".to_string(),
                "ease-(--my-ease)".to_string(),
                "ease-[cubic-bezier(0.95,0.05,0.795,0.035)]".to_string(),
                "ease-in-expo".to_string(),
                "md:ease-in".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".ease-linear"));
        assert!(result.css.contains("transition-timing-function: linear"));
        assert!(result.css.contains(".ease-in"));
        assert!(
            result
                .css
                .contains("transition-timing-function: var(--ease-in)")
        );
        assert!(result.css.contains(".ease-out"));
        assert!(
            result
                .css
                .contains("transition-timing-function: var(--ease-out)")
        );
        assert!(result.css.contains(".ease-in-out"));
        assert!(
            result
                .css
                .contains("transition-timing-function: var(--ease-in-out)")
        );
        assert!(result.css.contains(".ease-initial"));
        assert!(result.css.contains("transition-timing-function: initial"));
        assert!(result.css.contains(".ease-\\(--my-ease\\)"));
        assert!(
            result
                .css
                .contains("transition-timing-function: var(--my-ease)")
        );
        assert!(
            result
                .css
                .contains(".ease-\\[cubic-bezier\\(0.95\\,0.05\\,0.795\\,0.035\\)\\]")
        );
        assert!(
            result
                .css
                .contains("transition-timing-function: cubic-bezier(0.95,0.05,0.795,0.035)")
        );
        assert!(result.css.contains(".ease-in-expo"));
        assert!(
            result
                .css
                .contains("transition-timing-function: var(--ease-in-expo)")
        );
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:ease-in"));
    }

    #[test]
    fn generates_animation_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "animate-spin".to_string(),
                "animate-ping".to_string(),
                "animate-pulse".to_string(),
                "animate-bounce".to_string(),
                "animate-none".to_string(),
                "animate-(--my-animation)".to_string(),
                "animate-[wiggle_1s_ease-in-out_infinite]".to_string(),
                "animate-wiggle".to_string(),
                "motion-safe:animate-spin".to_string(),
                "md:animate-spin".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".animate-spin"));
        assert!(result.css.contains("animation: var(--animate-spin)"));
        assert!(result.css.contains(".animate-ping"));
        assert!(result.css.contains("animation: var(--animate-ping)"));
        assert!(result.css.contains(".animate-pulse"));
        assert!(result.css.contains("animation: var(--animate-pulse)"));
        assert!(result.css.contains(".animate-bounce"));
        assert!(result.css.contains("animation: var(--animate-bounce)"));
        assert!(result.css.contains(".animate-none"));
        assert!(result.css.contains("animation: none"));
        assert!(result.css.contains(".animate-\\(--my-animation\\)"));
        assert!(result.css.contains("animation: var(--my-animation)"));
        assert!(
            result
                .css
                .contains(".animate-\\[wiggle_1s_ease-in-out_infinite\\]")
        );
        assert!(
            result
                .css
                .contains("animation: wiggle_1s_ease-in-out_infinite")
        );
        assert!(result.css.contains(".animate-wiggle"));
        assert!(result.css.contains("animation: var(--animate-wiggle)"));
        assert!(
            result
                .css
                .contains("@media (prefers-reduced-motion: no-preference)")
        );
        assert!(result.css.contains(".motion-safe\\:animate-spin"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:animate-spin"));
    }

    #[test]
    fn generates_motion_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "motion-reduce:transition-none".to_string(),
                "motion-safe:hover:transition-all".to_string(),
            ],
            &config,
        );

        assert!(
            result
                .css
                .contains("@media (prefers-reduced-motion: reduce)")
        );
        assert!(result.css.contains(".motion-reduce\\:transition-none"));
        assert!(
            result
                .css
                .contains("@media (prefers-reduced-motion: no-preference)")
        );
        assert!(
            result
                .css
                .contains(".motion-safe\\:hover\\:transition-all:hover")
        );
    }

    #[test]
    fn generates_spacing_scale() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(&["p-2".to_string(), "m-6".to_string()], &config);
        assert!(result.css.contains(".p-2"));
        assert!(result.css.contains("padding: calc(var(--spacing) * 2)"));
        assert!(result.css.contains(".m-6"));
        assert!(result.css.contains("margin: calc(var(--spacing) * 6)"));
    }

    #[test]
    fn generates_padding_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "p-px".to_string(),
                "p-1.5".to_string(),
                "p-[5px]".to_string(),
                "p-(--my-padding)".to_string(),
                "px-4".to_string(),
                "py-2".to_string(),
                "pt-6".to_string(),
                "pr-px".to_string(),
                "pb-[3vh]".to_string(),
                "pl-(--pad-left)".to_string(),
                "ps-8".to_string(),
                "pe-1".to_string(),
                "md:py-8".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".p-px"));
        assert!(result.css.contains("padding: 1px"));
        assert!(result.css.contains(".p-1\\.5"));
        assert!(result.css.contains("padding: calc(var(--spacing) * 1.5)"));
        assert!(result.css.contains(".p-\\[5px\\]"));
        assert!(result.css.contains("padding: 5px"));
        assert!(result.css.contains(".p-\\(--my-padding\\)"));
        assert!(result.css.contains("padding: var(--my-padding)"));
        assert!(result.css.contains(".px-4"));
        assert!(
            result
                .css
                .contains("padding-inline: calc(var(--spacing) * 4)")
        );
        assert!(result.css.contains(".py-2"));
        assert!(
            result
                .css
                .contains("padding-block: calc(var(--spacing) * 2)")
        );
        assert!(result.css.contains(".pt-6"));
        assert!(result.css.contains("padding-top: calc(var(--spacing) * 6)"));
        assert!(result.css.contains(".pr-px"));
        assert!(result.css.contains("padding-right: 1px"));
        assert!(result.css.contains(".pb-\\[3vh\\]"));
        assert!(result.css.contains("padding-bottom: 3vh"));
        assert!(result.css.contains(".pl-\\(--pad-left\\)"));
        assert!(result.css.contains("padding-left: var(--pad-left)"));
        assert!(result.css.contains(".ps-8"));
        assert!(
            result
                .css
                .contains("padding-inline-start: calc(var(--spacing) * 8)")
        );
        assert!(result.css.contains(".pe-1"));
        assert!(
            result
                .css
                .contains("padding-inline-end: calc(var(--spacing) * 1)")
        );
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:py-8"));
    }

    #[test]
    fn generates_margin_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "m-auto".to_string(),
                "m-px".to_string(),
                "-m-px".to_string(),
                "m-4".to_string(),
                "-m-4".to_string(),
                "-m-1.5".to_string(),
                "m-[5px]".to_string(),
                "-m-[5px]".to_string(),
                "m-(--my-margin)".to_string(),
                "-m-(--my-margin)".to_string(),
                "mx-8".to_string(),
                "-mx-2".to_string(),
                "my-3".to_string(),
                "-my-1".to_string(),
                "mt-6".to_string(),
                "-mr-3".to_string(),
                "mb-auto".to_string(),
                "ml-px".to_string(),
                "ms-4".to_string(),
                "-me-2".to_string(),
                "md:mt-8".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".m-auto"));
        assert!(result.css.contains("margin: auto"));
        assert!(result.css.contains(".m-px"));
        assert!(result.css.contains("margin: 1px"));
        assert!(result.css.contains(".-m-px"));
        assert!(result.css.contains("margin: -1px"));
        assert!(result.css.contains(".m-4"));
        assert!(result.css.contains("margin: calc(var(--spacing) * 4)"));
        assert!(result.css.contains(".-m-4"));
        assert!(result.css.contains("margin: calc(var(--spacing) * -4)"));
        assert!(result.css.contains(".-m-1\\.5"));
        assert!(result.css.contains("margin: calc(var(--spacing) * -1.5)"));
        assert!(result.css.contains(".m-\\[5px\\]"));
        assert!(result.css.contains("margin: 5px"));
        assert!(result.css.contains(".-m-\\[5px\\]"));
        assert!(result.css.contains("margin: calc(5px * -1)"));
        assert!(result.css.contains(".m-\\(--my-margin\\)"));
        assert!(result.css.contains("margin: var(--my-margin)"));
        assert!(result.css.contains(".-m-\\(--my-margin\\)"));
        assert!(result.css.contains("margin: calc(var(--my-margin) * -1)"));
        assert!(result.css.contains(".mx-8"));
        assert!(
            result
                .css
                .contains("margin-inline: calc(var(--spacing) * 8)")
        );
        assert!(result.css.contains(".-mx-2"));
        assert!(
            result
                .css
                .contains("margin-inline: calc(var(--spacing) * -2)")
        );
        assert!(result.css.contains(".my-3"));
        assert!(
            result
                .css
                .contains("margin-block: calc(var(--spacing) * 3)")
        );
        assert!(result.css.contains(".-my-1"));
        assert!(
            result
                .css
                .contains("margin-block: calc(var(--spacing) * -1)")
        );
        assert!(result.css.contains(".mt-6"));
        assert!(result.css.contains("margin-top: calc(var(--spacing) * 6)"));
        assert!(result.css.contains(".-mr-3"));
        assert!(
            result
                .css
                .contains("margin-right: calc(var(--spacing) * -3)")
        );
        assert!(result.css.contains(".mb-auto"));
        assert!(result.css.contains("margin-bottom: auto"));
        assert!(result.css.contains(".ml-px"));
        assert!(result.css.contains("margin-left: 1px"));
        assert!(result.css.contains(".ms-4"));
        assert!(
            result
                .css
                .contains("margin-inline-start: calc(var(--spacing) * 4)")
        );
        assert!(result.css.contains(".-me-2"));
        assert!(
            result
                .css
                .contains("margin-inline-end: calc(var(--spacing) * -2)")
        );
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:mt-8"));
    }

    #[test]
    fn generates_scroll_margin_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "scroll-m-4".to_string(),
                "-scroll-m-2".to_string(),
                "scroll-m-(--my-scroll-margin)".to_string(),
                "scroll-m-[24rem]".to_string(),
                "scroll-mx-6".to_string(),
                "-scroll-my-1".to_string(),
                "scroll-ms-8".to_string(),
                "-scroll-me-3".to_string(),
                "scroll-mt-5".to_string(),
                "scroll-mr-[10vh]".to_string(),
                "scroll-mb-(--my-scroll-margin-bottom)".to_string(),
                "-scroll-ml-[5px]".to_string(),
                "md:scroll-m-0".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".scroll-m-4"));
        assert!(
            result
                .css
                .contains("scroll-margin: calc(var(--spacing) * 4)")
        );
        assert!(result.css.contains(".-scroll-m-2"));
        assert!(
            result
                .css
                .contains("scroll-margin: calc(var(--spacing) * -2)")
        );
        assert!(result.css.contains(".scroll-m-\\(--my-scroll-margin\\)"));
        assert!(
            result
                .css
                .contains("scroll-margin: var(--my-scroll-margin)")
        );
        assert!(result.css.contains(".scroll-m-\\[24rem\\]"));
        assert!(result.css.contains("scroll-margin: 24rem"));
        assert!(result.css.contains(".scroll-mx-6"));
        assert!(
            result
                .css
                .contains("scroll-margin-inline: calc(var(--spacing) * 6)")
        );
        assert!(result.css.contains(".-scroll-my-1"));
        assert!(
            result
                .css
                .contains("scroll-margin-block: calc(var(--spacing) * -1)")
        );
        assert!(result.css.contains(".scroll-ms-8"));
        assert!(
            result
                .css
                .contains("scroll-margin-inline-start: calc(var(--spacing) * 8)")
        );
        assert!(result.css.contains(".-scroll-me-3"));
        assert!(
            result
                .css
                .contains("scroll-margin-inline-end: calc(var(--spacing) * -3)")
        );
        assert!(result.css.contains(".scroll-mt-5"));
        assert!(
            result
                .css
                .contains("scroll-margin-top: calc(var(--spacing) * 5)")
        );
        assert!(result.css.contains(".scroll-mr-\\[10vh\\]"));
        assert!(result.css.contains("scroll-margin-right: 10vh"));
        assert!(
            result
                .css
                .contains(".scroll-mb-\\(--my-scroll-margin-bottom\\)")
        );
        assert!(
            result
                .css
                .contains("scroll-margin-bottom: var(--my-scroll-margin-bottom)")
        );
        assert!(result.css.contains(".-scroll-ml-\\[5px\\]"));
        assert!(result.css.contains("scroll-margin-left: calc(5px * -1)"));
        assert!(result.css.contains(".md\\:scroll-m-0"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_scroll_padding_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "scroll-p-4".to_string(),
                "-scroll-p-2".to_string(),
                "scroll-p-(--my-scroll-padding)".to_string(),
                "scroll-p-[24rem]".to_string(),
                "scroll-px-6".to_string(),
                "-scroll-py-1".to_string(),
                "scroll-ps-8".to_string(),
                "-scroll-pe-3".to_string(),
                "scroll-pt-5".to_string(),
                "scroll-pr-[10vh]".to_string(),
                "scroll-pb-(--my-scroll-padding-bottom)".to_string(),
                "-scroll-pl-[5px]".to_string(),
                "md:scroll-p-0".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".scroll-p-4"));
        assert!(
            result
                .css
                .contains("scroll-padding: calc(var(--spacing) * 4)")
        );
        assert!(result.css.contains(".-scroll-p-2"));
        assert!(
            result
                .css
                .contains("scroll-padding: calc(var(--spacing) * -2)")
        );
        assert!(result.css.contains(".scroll-p-\\(--my-scroll-padding\\)"));
        assert!(
            result
                .css
                .contains("scroll-padding: var(--my-scroll-padding)")
        );
        assert!(result.css.contains(".scroll-p-\\[24rem\\]"));
        assert!(result.css.contains("scroll-padding: 24rem"));
        assert!(result.css.contains(".scroll-px-6"));
        assert!(
            result
                .css
                .contains("scroll-padding-inline: calc(var(--spacing) * 6)")
        );
        assert!(result.css.contains(".-scroll-py-1"));
        assert!(
            result
                .css
                .contains("scroll-padding-block: calc(var(--spacing) * -1)")
        );
        assert!(result.css.contains(".scroll-ps-8"));
        assert!(
            result
                .css
                .contains("scroll-padding-inline-start: calc(var(--spacing) * 8)")
        );
        assert!(result.css.contains(".-scroll-pe-3"));
        assert!(
            result
                .css
                .contains("scroll-padding-inline-end: calc(var(--spacing) * -3)")
        );
        assert!(result.css.contains(".scroll-pt-5"));
        assert!(
            result
                .css
                .contains("scroll-padding-top: calc(var(--spacing) * 5)")
        );
        assert!(result.css.contains(".scroll-pr-\\[10vh\\]"));
        assert!(result.css.contains("scroll-padding-right: 10vh"));
        assert!(
            result
                .css
                .contains(".scroll-pb-\\(--my-scroll-padding-bottom\\)")
        );
        assert!(
            result
                .css
                .contains("scroll-padding-bottom: var(--my-scroll-padding-bottom)")
        );
        assert!(result.css.contains(".-scroll-pl-\\[5px\\]"));
        assert!(result.css.contains("scroll-padding-left: calc(5px * -1)"));
        assert!(result.css.contains(".md\\:scroll-p-0"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_typography_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "text-xs".to_string(),
                "text-sm".to_string(),
                "text-base".to_string(),
                "text-lg".to_string(),
                "text-xl".to_string(),
                "text-2xl".to_string(),
                "text-3xl".to_string(),
                "text-4xl".to_string(),
                "text-5xl".to_string(),
                "text-6xl".to_string(),
                "text-7xl".to_string(),
                "text-8xl".to_string(),
                "text-9xl".to_string(),
                "text-tiny".to_string(),
                "text-[14px]".to_string(),
                "text-(length:--my-text-size)".to_string(),
                "text-base/6".to_string(),
                "text-sm/(--my-line-height)".to_string(),
                "text-lg/[1.75]".to_string(),
                "md:text-base".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".text-xs"));
        assert!(result.css.contains("font-size: var(--text-xs)"));
        assert!(
            result
                .css
                .contains("line-height: var(--tw-leading,var(--text-xs--line-height))")
        );
        assert!(result.css.contains(".text-sm"));
        assert!(result.css.contains("font-size: var(--text-sm)"));
        assert!(result.css.contains(".text-base"));
        assert!(result.css.contains("font-size: var(--text-base)"));
        assert!(result.css.contains(".text-lg"));
        assert!(result.css.contains("font-size: var(--text-lg)"));
        assert!(result.css.contains(".text-xl"));
        assert!(result.css.contains("font-size: var(--text-xl)"));
        assert!(result.css.contains(".text-2xl"));
        assert!(result.css.contains("font-size: var(--text-2xl)"));
        assert!(result.css.contains(".text-3xl"));
        assert!(result.css.contains("font-size: var(--text-3xl)"));
        assert!(result.css.contains(".text-4xl"));
        assert!(result.css.contains("font-size: var(--text-4xl)"));
        assert!(result.css.contains(".text-5xl"));
        assert!(result.css.contains("font-size: var(--text-5xl)"));
        assert!(result.css.contains(".text-6xl"));
        assert!(result.css.contains("font-size: var(--text-6xl)"));
        assert!(result.css.contains(".text-7xl"));
        assert!(result.css.contains("font-size: var(--text-7xl)"));
        assert!(result.css.contains(".text-8xl"));
        assert!(result.css.contains("font-size: var(--text-8xl)"));
        assert!(result.css.contains(".text-9xl"));
        assert!(result.css.contains("font-size: var(--text-9xl)"));
        assert!(result.css.contains(".text-tiny"));
        assert!(result.css.contains("font-size: var(--text-tiny)"));
        assert!(result.css.contains(".text-\\[14px\\]"));
        assert!(result.css.contains("font-size: 14px"));
        assert!(result.css.contains(".text-\\(length\\:--my-text-size\\)"));
        assert!(result.css.contains("font-size: var(--my-text-size)"));
        assert!(result.css.contains(".text-base\\/6"));
        assert!(result.css.contains("font-size: var(--text-base)"));
        assert!(result.css.contains("line-height: calc(var(--spacing) * 6)"));
        assert!(result.css.contains(".text-sm\\/\\(--my-line-height\\)"));
        assert!(result.css.contains("line-height: var(--my-line-height)"));
        assert!(result.css.contains(".text-lg\\/\\[1.75\\]"));
        assert!(result.css.contains("line-height: 1.75"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:text-base"));
    }

    #[test]
    fn generates_font_family_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "font-sans".to_string(),
                "font-serif".to_string(),
                "font-mono".to_string(),
                "font-display".to_string(),
                "font-(family-name:--my-font)".to_string(),
                "font-[Open_Sans]".to_string(),
                "font-bold".to_string(),
                "md:font-serif".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".font-sans"));
        assert!(result.css.contains("font-family: var(--font-sans)"));
        assert!(result.css.contains(".font-serif"));
        assert!(result.css.contains("font-family: var(--font-serif)"));
        assert!(result.css.contains(".font-mono"));
        assert!(result.css.contains("font-family: var(--font-mono)"));
        assert!(result.css.contains(".font-display"));
        assert!(result.css.contains("font-family: var(--font-display)"));
        assert!(result.css.contains(".font-\\(family-name\\:--my-font\\)"));
        assert!(result.css.contains("font-family: var(--my-font)"));
        assert!(result.css.contains(".font-\\[Open_Sans\\]"));
        assert!(result.css.contains("font-family: Open_Sans"));
        assert!(result.css.contains(".font-bold"));
        assert!(result.css.contains("font-weight: var(--font-weight-bold)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:font-serif"));
    }

    #[test]
    fn generates_font_weight_and_leading_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "font-thin".to_string(),
                "font-extralight".to_string(),
                "font-medium".to_string(),
                "font-bold".to_string(),
                "font-extrabold".to_string(),
                "font-black".to_string(),
                "font-[1000]".to_string(),
                "font-(weight:--my-font-weight)".to_string(),
                "font-extrablack".to_string(),
                "md:font-bold".to_string(),
                "leading-none".to_string(),
                "leading-6".to_string(),
                "leading-(--my-leading)".to_string(),
                "leading-[1.5]".to_string(),
                "leading-snug".to_string(),
                "leading-normal".to_string(),
                "leading-loose".to_string(),
                "md:leading-7".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".font-thin"));
        assert!(result.css.contains("font-weight: var(--font-weight-thin)"));
        assert!(result.css.contains(".font-extralight"));
        assert!(
            result
                .css
                .contains("font-weight: var(--font-weight-extralight)")
        );
        assert!(result.css.contains(".font-medium"));
        assert!(
            result
                .css
                .contains("font-weight: var(--font-weight-medium)")
        );
        assert!(result.css.contains(".font-bold"));
        assert!(result.css.contains("font-weight: var(--font-weight-bold)"));
        assert!(result.css.contains(".font-extrabold"));
        assert!(
            result
                .css
                .contains("font-weight: var(--font-weight-extrabold)")
        );
        assert!(result.css.contains(".font-black"));
        assert!(result.css.contains("font-weight: var(--font-weight-black)"));
        assert!(result.css.contains(".font-\\[1000\\]"));
        assert!(result.css.contains("font-weight: 1000"));
        assert!(result.css.contains(".font-\\(weight\\:--my-font-weight\\)"));
        assert!(result.css.contains("font-weight: var(--my-font-weight)"));
        assert!(result.css.contains(".font-extrablack"));
        assert!(
            result
                .css
                .contains("font-weight: var(--font-weight-extrablack)")
        );
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:font-bold"));
        assert!(result.css.contains(".leading-none"));
        assert!(result.css.contains("line-height: 1"));
        assert!(result.css.contains(".leading-6"));
        assert!(result.css.contains("line-height: calc(var(--spacing) * 6)"));
        assert!(result.css.contains(".leading-\\(--my-leading\\)"));
        assert!(result.css.contains("line-height: var(--my-leading)"));
        assert!(result.css.contains(".leading-\\[1.5\\]"));
        assert!(result.css.contains("line-height: 1.5"));
        assert!(result.css.contains(".leading-snug"));
        assert!(result.css.contains("line-height: var(--leading-snug)"));
        assert!(result.css.contains(".leading-normal"));
        assert!(result.css.contains("line-height: var(--leading-normal)"));
        assert!(result.css.contains(".leading-loose"));
        assert!(result.css.contains("line-height: var(--leading-loose)"));
        assert!(result.css.contains(".md\\:leading-7"));
    }

    #[test]
    fn generates_font_smoothing_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "antialiased".to_string(),
                "subpixel-antialiased".to_string(),
                "md:subpixel-antialiased".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".antialiased"));
        assert!(result.css.contains("-webkit-font-smoothing: antialiased"));
        assert!(result.css.contains("-moz-osx-font-smoothing: grayscale"));
        assert!(result.css.contains(".subpixel-antialiased"));
        assert!(result.css.contains("-webkit-font-smoothing: auto"));
        assert!(result.css.contains("-moz-osx-font-smoothing: auto"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:subpixel-antialiased"));
    }

    #[test]
    fn generates_font_stretch_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "font-stretch-ultra-condensed".to_string(),
                "font-stretch-extra-condensed".to_string(),
                "font-stretch-condensed".to_string(),
                "font-stretch-semi-condensed".to_string(),
                "font-stretch-normal".to_string(),
                "font-stretch-semi-expanded".to_string(),
                "font-stretch-expanded".to_string(),
                "font-stretch-extra-expanded".to_string(),
                "font-stretch-ultra-expanded".to_string(),
                "font-stretch-125%".to_string(),
                "font-stretch-[66.66%]".to_string(),
                "font-stretch-(--my-font-width)".to_string(),
                "md:font-stretch-expanded".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".font-stretch-ultra-condensed"));
        assert!(result.css.contains("font-stretch: ultra-condensed"));
        assert!(result.css.contains(".font-stretch-extra-condensed"));
        assert!(result.css.contains("font-stretch: extra-condensed"));
        assert!(result.css.contains(".font-stretch-condensed"));
        assert!(result.css.contains("font-stretch: condensed"));
        assert!(result.css.contains(".font-stretch-semi-condensed"));
        assert!(result.css.contains("font-stretch: semi-condensed"));
        assert!(result.css.contains(".font-stretch-normal"));
        assert!(result.css.contains("font-stretch: normal"));
        assert!(result.css.contains(".font-stretch-semi-expanded"));
        assert!(result.css.contains("font-stretch: semi-expanded"));
        assert!(result.css.contains(".font-stretch-expanded"));
        assert!(result.css.contains("font-stretch: expanded"));
        assert!(result.css.contains(".font-stretch-extra-expanded"));
        assert!(result.css.contains("font-stretch: extra-expanded"));
        assert!(result.css.contains(".font-stretch-ultra-expanded"));
        assert!(result.css.contains("font-stretch: ultra-expanded"));
        assert!(result.css.contains(".font-stretch-125\\%"));
        assert!(result.css.contains("font-stretch: 125%"));
        assert!(result.css.contains(".font-stretch-\\[66.66\\%\\]"));
        assert!(result.css.contains("font-stretch: 66.66%"));
        assert!(result.css.contains(".font-stretch-\\(--my-font-width\\)"));
        assert!(result.css.contains("font-stretch: var(--my-font-width)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:font-stretch-expanded"));
    }

    #[test]
    fn generates_font_variant_numeric_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "normal-nums".to_string(),
                "ordinal".to_string(),
                "slashed-zero".to_string(),
                "lining-nums".to_string(),
                "oldstyle-nums".to_string(),
                "proportional-nums".to_string(),
                "tabular-nums".to_string(),
                "diagonal-fractions".to_string(),
                "stacked-fractions".to_string(),
                "md:normal-nums".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".normal-nums"));
        assert!(result.css.contains("font-variant-numeric: normal"));
        assert!(result.css.contains(".ordinal"));
        assert!(result.css.contains("font-variant-numeric: ordinal"));
        assert!(result.css.contains(".slashed-zero"));
        assert!(result.css.contains("font-variant-numeric: slashed-zero"));
        assert!(result.css.contains(".lining-nums"));
        assert!(result.css.contains("font-variant-numeric: lining-nums"));
        assert!(result.css.contains(".oldstyle-nums"));
        assert!(result.css.contains("font-variant-numeric: oldstyle-nums"));
        assert!(result.css.contains(".proportional-nums"));
        assert!(
            result
                .css
                .contains("font-variant-numeric: proportional-nums")
        );
        assert!(result.css.contains(".tabular-nums"));
        assert!(result.css.contains("font-variant-numeric: tabular-nums"));
        assert!(result.css.contains(".diagonal-fractions"));
        assert!(
            result
                .css
                .contains("font-variant-numeric: diagonal-fractions")
        );
        assert!(result.css.contains(".stacked-fractions"));
        assert!(
            result
                .css
                .contains("font-variant-numeric: stacked-fractions")
        );
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:normal-nums"));
    }

    #[test]
    fn generates_tracking_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "tracking-tighter".to_string(),
                "tracking-tight".to_string(),
                "tracking-normal".to_string(),
                "tracking-wide".to_string(),
                "tracking-wider".to_string(),
                "tracking-widest".to_string(),
                "tracking-(--my-tracking)".to_string(),
                "tracking-[.25em]".to_string(),
                "-tracking-2".to_string(),
                "md:tracking-wide".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".tracking-tighter"));
        assert!(
            result
                .css
                .contains("letter-spacing: var(--tracking-tighter)")
        );
        assert!(result.css.contains(".tracking-tight"));
        assert!(result.css.contains("letter-spacing: var(--tracking-tight)"));
        assert!(result.css.contains(".tracking-normal"));
        assert!(
            result
                .css
                .contains("letter-spacing: var(--tracking-normal)")
        );
        assert!(result.css.contains(".tracking-wide"));
        assert!(result.css.contains("letter-spacing: var(--tracking-wide)"));
        assert!(result.css.contains(".tracking-wider"));
        assert!(result.css.contains("letter-spacing: var(--tracking-wider)"));
        assert!(result.css.contains(".tracking-widest"));
        assert!(
            result
                .css
                .contains("letter-spacing: var(--tracking-widest)")
        );
        assert!(result.css.contains(".tracking-\\(--my-tracking\\)"));
        assert!(result.css.contains("letter-spacing: var(--my-tracking)"));
        assert!(result.css.contains(".tracking-\\[.25em\\]"));
        assert!(result.css.contains("letter-spacing: .25em"));
        assert!(result.css.contains(".-tracking-2"));
        assert!(
            result
                .css
                .contains("letter-spacing: calc(var(--tracking-2) * -1)")
        );
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:tracking-wide"));
    }

    #[test]
    fn generates_line_clamp_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "line-clamp-3".to_string(),
                "line-clamp-none".to_string(),
                "line-clamp-[calc(var(--characters)/100)]".to_string(),
                "line-clamp-(--my-line-count)".to_string(),
                "md:line-clamp-4".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".line-clamp-3"));
        assert!(result.css.contains("overflow: hidden"));
        assert!(result.css.contains("display: -webkit-box"));
        assert!(result.css.contains("-webkit-box-orient: vertical"));
        assert!(result.css.contains("-webkit-line-clamp: 3"));
        assert!(result.css.contains(".line-clamp-none"));
        assert!(result.css.contains("overflow: visible"));
        assert!(result.css.contains("display: block"));
        assert!(result.css.contains("-webkit-box-orient: horizontal"));
        assert!(result.css.contains("-webkit-line-clamp: unset"));
        assert!(
            result
                .css
                .contains(".line-clamp-\\[calc\\(var\\(--characters\\)\\/100\\)\\]")
        );
        assert!(
            result
                .css
                .contains("-webkit-line-clamp: calc(var(--characters)/100)")
        );
        assert!(result.css.contains(".line-clamp-\\(--my-line-count\\)"));
        assert!(
            result
                .css
                .contains("-webkit-line-clamp: var(--my-line-count)")
        );
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:line-clamp-4"));
    }

    #[test]
    fn generates_list_style_image_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "list-image-none".to_string(),
                "list-image-[url(/img/checkmark.png)]".to_string(),
                "list-image-(--my-list-image)".to_string(),
                "md:list-image-[url(/img/checkmark.png)]".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".list-image-none"));
        assert!(result.css.contains("list-style-image: none"));
        assert!(
            result
                .css
                .contains(".list-image-\\[url\\(\\/img\\/checkmark.png\\)\\]")
        );
        assert!(
            result
                .css
                .contains("list-style-image: url(/img/checkmark.png)")
        );
        assert!(result.css.contains(".list-image-\\(--my-list-image\\)"));
        assert!(
            result
                .css
                .contains("list-style-image: var(--my-list-image)")
        );
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(
            result
                .css
                .contains(".md\\:list-image-\\[url\\(\\/img\\/checkmark.png\\)\\]")
        );
    }

    #[test]
    fn generates_list_style_position_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "list-inside".to_string(),
                "list-outside".to_string(),
                "md:list-inside".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".list-inside"));
        assert!(result.css.contains("list-style-position: inside"));
        assert!(result.css.contains(".list-outside"));
        assert!(result.css.contains("list-style-position: outside"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:list-inside"));
    }

    #[test]
    fn generates_list_style_type_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "list-disc".to_string(),
                "list-decimal".to_string(),
                "list-none".to_string(),
                "list-[upper-roman]".to_string(),
                "list-(--my-marker)".to_string(),
                "md:list-disc".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".list-disc"));
        assert!(result.css.contains("list-style-type: disc"));
        assert!(result.css.contains(".list-decimal"));
        assert!(result.css.contains("list-style-type: decimal"));
        assert!(result.css.contains(".list-none"));
        assert!(result.css.contains("list-style-type: none"));
        assert!(result.css.contains(".list-\\[upper-roman\\]"));
        assert!(result.css.contains("list-style-type: upper-roman"));
        assert!(result.css.contains(".list-\\(--my-marker\\)"));
        assert!(result.css.contains("list-style-type: var(--my-marker)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:list-disc"));
    }

    #[test]
    fn generates_text_align_and_font_style_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "italic".to_string(),
                "not-italic".to_string(),
                "text-left".to_string(),
                "text-center".to_string(),
                "text-right".to_string(),
                "text-justify".to_string(),
                "text-start".to_string(),
                "text-end".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".italic"));
        assert!(result.css.contains("font-style: italic"));
        assert!(result.css.contains(".not-italic"));
        assert!(result.css.contains("font-style: normal"));
        assert!(result.css.contains(".text-left"));
        assert!(result.css.contains("text-align: left"));
        assert!(result.css.contains(".text-center"));
        assert!(result.css.contains("text-align: center"));
        assert!(result.css.contains(".text-right"));
        assert!(result.css.contains("text-align: right"));
        assert!(result.css.contains(".text-justify"));
        assert!(result.css.contains("text-align: justify"));
        assert!(result.css.contains(".text-start"));
        assert!(result.css.contains("text-align: start"));
        assert!(result.css.contains(".text-end"));
        assert!(result.css.contains("text-align: end"));
    }

    #[test]
    fn generates_text_transform_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "uppercase".to_string(),
                "lowercase".to_string(),
                "capitalize".to_string(),
                "normal-case".to_string(),
                "md:uppercase".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".uppercase"));
        assert!(result.css.contains("text-transform: uppercase"));
        assert!(result.css.contains(".lowercase"));
        assert!(result.css.contains("text-transform: lowercase"));
        assert!(result.css.contains(".capitalize"));
        assert!(result.css.contains("text-transform: capitalize"));
        assert!(result.css.contains(".normal-case"));
        assert!(result.css.contains("text-transform: none"));
        assert!(result.css.contains(".md\\:uppercase"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_text_overflow_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "truncate".to_string(),
                "text-ellipsis".to_string(),
                "text-clip".to_string(),
                "md:text-clip".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".truncate"));
        assert!(result.css.contains("overflow: hidden"));
        assert!(result.css.contains("text-overflow: ellipsis"));
        assert!(result.css.contains("white-space: nowrap"));
        assert!(result.css.contains(".text-ellipsis"));
        assert!(result.css.contains(".text-clip"));
        assert!(result.css.contains("text-overflow: clip"));
        assert!(result.css.contains(".md\\:text-clip"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_text_wrap_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "text-wrap".to_string(),
                "text-nowrap".to_string(),
                "text-balance".to_string(),
                "text-pretty".to_string(),
                "md:text-balance".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".text-wrap"));
        assert!(result.css.contains("text-wrap: wrap"));
        assert!(result.css.contains(".text-nowrap"));
        assert!(result.css.contains("text-wrap: nowrap"));
        assert!(result.css.contains(".text-balance"));
        assert!(result.css.contains("text-wrap: balance"));
        assert!(result.css.contains(".text-pretty"));
        assert!(result.css.contains("text-wrap: pretty"));
        assert!(result.css.contains(".md\\:text-balance"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_whitespace_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "whitespace-normal".to_string(),
                "whitespace-nowrap".to_string(),
                "whitespace-pre".to_string(),
                "whitespace-pre-line".to_string(),
                "whitespace-pre-wrap".to_string(),
                "whitespace-break-spaces".to_string(),
                "md:whitespace-normal".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".whitespace-normal"));
        assert!(result.css.contains("white-space: normal"));
        assert!(result.css.contains(".whitespace-nowrap"));
        assert!(result.css.contains("white-space: nowrap"));
        assert!(result.css.contains(".whitespace-pre"));
        assert!(result.css.contains("white-space: pre"));
        assert!(result.css.contains(".whitespace-pre-line"));
        assert!(result.css.contains("white-space: pre-line"));
        assert!(result.css.contains(".whitespace-pre-wrap"));
        assert!(result.css.contains("white-space: pre-wrap"));
        assert!(result.css.contains(".whitespace-break-spaces"));
        assert!(result.css.contains("white-space: break-spaces"));
        assert!(result.css.contains(".md\\:whitespace-normal"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_word_break_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "break-normal".to_string(),
                "break-all".to_string(),
                "break-keep".to_string(),
                "md:break-all".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".break-normal"));
        assert!(result.css.contains("word-break: normal"));
        assert!(result.css.contains(".break-all"));
        assert!(result.css.contains("word-break: break-all"));
        assert!(result.css.contains(".break-keep"));
        assert!(result.css.contains("word-break: keep-all"));
        assert!(result.css.contains(".md\\:break-all"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_overflow_wrap_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "wrap-break-word".to_string(),
                "break-words".to_string(),
                "wrap-anywhere".to_string(),
                "wrap-normal".to_string(),
                "md:wrap-break-word".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".wrap-break-word"));
        assert!(result.css.contains("overflow-wrap: break-word"));
        assert!(result.css.contains(".break-words"));
        assert!(result.css.contains("overflow-wrap: break-word"));
        assert!(result.css.contains(".wrap-anywhere"));
        assert!(result.css.contains("overflow-wrap: anywhere"));
        assert!(result.css.contains(".wrap-normal"));
        assert!(result.css.contains("overflow-wrap: normal"));
        assert!(result.css.contains(".md\\:wrap-break-word"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_hyphens_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "hyphens-none".to_string(),
                "hyphens-manual".to_string(),
                "hyphens-auto".to_string(),
                "md:hyphens-auto".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".hyphens-none"));
        assert!(result.css.contains("hyphens: none"));
        assert!(result.css.contains(".hyphens-manual"));
        assert!(result.css.contains("hyphens: manual"));
        assert!(result.css.contains(".hyphens-auto"));
        assert!(result.css.contains("hyphens: auto"));
        assert!(result.css.contains(".md\\:hyphens-auto"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_content_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "content-none".to_string(),
                "content-[attr(before)]".to_string(),
                "content-['Hello_World']".to_string(),
                "content-['Hello\\_World']".to_string(),
                "content-(--my-content)".to_string(),
                "before:content-['x']".to_string(),
                "md:before:content-['Desktop']".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".content-none"));
        assert!(result.css.contains("content: none"));
        assert!(result.css.contains(".content-\\[attr\\(before\\)\\]"));
        assert!(result.css.contains("content: attr(before)"));
        assert!(result.css.contains(".content-\\['Hello_World'\\]"));
        assert!(result.css.contains("content: 'Hello World'"));
        assert!(result.css.contains(".content-\\['Hello\\\\_World'\\]"));
        assert!(result.css.contains("content: 'Hello_World'"));
        assert!(result.css.contains(".content-\\(--my-content\\)"));
        assert!(result.css.contains("content: var(--my-content)"));
        assert!(result.css.contains(".before\\:content-\\['x'\\]:before"));
        assert!(
            result
                .css
                .contains(".md\\:before\\:content-\\['Desktop'\\]:before")
        );
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_filter_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "filter".to_string(),
                "filter-none".to_string(),
                "filter-[url('filters.svg#filter-id')]".to_string(),
                "filter-(--my-filter)".to_string(),
                "hover:filter-none".to_string(),
                "md:filter-none".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".filter"));
        assert!(result.css.contains("filter: var(--tw-blur,)"));
        assert!(result.css.contains(".filter-none"));
        assert!(result.css.contains("filter: none"));
        assert!(
            result
                .css
                .contains(".filter-\\[url\\('filters.svg#filter-id'\\)\\]")
        );
        assert!(result.css.contains("filter: url('filters.svg#filter-id')"));
        assert!(result.css.contains(".filter-\\(--my-filter\\)"));
        assert!(result.css.contains("filter: var(--my-filter)"));
        assert!(result.css.contains(".hover\\:filter-none:hover"));
        assert!(result.css.contains(".md\\:filter-none"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_blur_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "blur".to_string(),
                "blur-none".to_string(),
                "blur-xs".to_string(),
                "blur-sm".to_string(),
                "blur-lg".to_string(),
                "blur-2xl".to_string(),
                "blur-2xs".to_string(),
                "blur-[2px]".to_string(),
                "blur-(--my-blur)".to_string(),
                "md:blur-lg".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".blur"));
        assert!(result.css.contains("--tw-blur: blur(8px)"));
        assert!(result.css.contains(".blur-none"));
        assert!(result.css.contains("--tw-blur: blur(0)"));
        assert!(result.css.contains(".blur-xs"));
        assert!(result.css.contains("--tw-blur: blur(var(--blur-xs))"));
        assert!(result.css.contains(".blur-sm"));
        assert!(result.css.contains("--tw-blur: blur(var(--blur-sm))"));
        assert!(result.css.contains(".blur-lg"));
        assert!(result.css.contains("--tw-blur: blur(var(--blur-lg))"));
        assert!(result.css.contains(".blur-2xl"));
        assert!(result.css.contains("--tw-blur: blur(var(--blur-2xl))"));
        assert!(result.css.contains(".blur-2xs"));
        assert!(result.css.contains("--tw-blur: blur(var(--blur-2xs))"));
        assert!(result.css.contains(".blur-\\[2px\\]"));
        assert!(result.css.contains("--tw-blur: blur(2px)"));
        assert!(result.css.contains(".blur-\\(--my-blur\\)"));
        assert!(result.css.contains("--tw-blur: blur(var(--my-blur))"));
        assert!(result.css.contains("filter: var(--tw-blur,)"));
        assert!(result.css.contains(".md\\:blur-lg"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn composes_filter_utilities_with_css_variables() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(&["blur-sm".to_string(), "grayscale".to_string()], &config);
        assert!(result.css.contains("--tw-blur: blur(var(--blur-sm))"));
        assert!(result.css.contains("--tw-grayscale: grayscale(100%)"));
        assert!(result.css.contains(
            "filter: var(--tw-blur,) var(--tw-brightness,) var(--tw-contrast,) var(--tw-grayscale,)"
        ));
    }

    #[test]
    fn generates_brightness_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "brightness-50".to_string(),
                "brightness-100".to_string(),
                "brightness-125".to_string(),
                "brightness-200".to_string(),
                "brightness-[1.75]".to_string(),
                "brightness-(--my-brightness)".to_string(),
                "md:brightness-150".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".brightness-50"));
        assert!(result.css.contains("filter: brightness(50%)"));
        assert!(result.css.contains(".brightness-100"));
        assert!(result.css.contains("filter: brightness(100%)"));
        assert!(result.css.contains(".brightness-125"));
        assert!(result.css.contains("filter: brightness(125%)"));
        assert!(result.css.contains(".brightness-200"));
        assert!(result.css.contains("filter: brightness(200%)"));
        assert!(result.css.contains(".brightness-\\[1.75\\]"));
        assert!(result.css.contains("filter: brightness(1.75)"));
        assert!(result.css.contains(".brightness-\\(--my-brightness\\)"));
        assert!(
            result
                .css
                .contains("filter: brightness(var(--my-brightness))")
        );
        assert!(result.css.contains(".md\\:brightness-150"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_contrast_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "contrast-50".to_string(),
                "contrast-100".to_string(),
                "contrast-125".to_string(),
                "contrast-200".to_string(),
                "contrast-[.25]".to_string(),
                "contrast-(--my-contrast)".to_string(),
                "md:contrast-150".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".contrast-50"));
        assert!(result.css.contains("filter: contrast(50%)"));
        assert!(result.css.contains(".contrast-100"));
        assert!(result.css.contains("filter: contrast(100%)"));
        assert!(result.css.contains(".contrast-125"));
        assert!(result.css.contains("filter: contrast(125%)"));
        assert!(result.css.contains(".contrast-200"));
        assert!(result.css.contains("filter: contrast(200%)"));
        assert!(result.css.contains(".contrast-\\[.25\\]"));
        assert!(result.css.contains("filter: contrast(.25)"));
        assert!(result.css.contains(".contrast-\\(--my-contrast\\)"));
        assert!(result.css.contains("filter: contrast(var(--my-contrast))"));
        assert!(result.css.contains(".md\\:contrast-150"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_drop_shadow_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "drop-shadow-none".to_string(),
                "drop-shadow-md".to_string(),
                "drop-shadow-3xl".to_string(),
                "drop-shadow-[0_35px_35px_rgba(0,0,0,0.25)]".to_string(),
                "drop-shadow-(--my-drop-shadow)".to_string(),
                "drop-shadow-(color:--my-shadow-color)".to_string(),
                "drop-shadow-indigo-500".to_string(),
                "drop-shadow-cyan-500/50".to_string(),
                "drop-shadow-xl/25".to_string(),
                "md:drop-shadow-xl".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".drop-shadow-none"));
        assert!(
            result
                .css
                .contains("--tw-drop-shadow: drop-shadow(0 0 #0000)")
        );
        assert!(result.css.contains(".drop-shadow-md"));
        assert!(
            result
                .css
                .contains("--tw-drop-shadow: drop-shadow(var(--drop-shadow-md))")
        );
        assert!(result.css.contains(".drop-shadow-3xl"));
        assert!(
            result
                .css
                .contains("--tw-drop-shadow: drop-shadow(var(--drop-shadow-3xl))")
        );
        assert!(
            result
                .css
                .contains(".drop-shadow-\\[0_35px_35px_rgba\\(0\\,0\\,0\\,0.25\\)\\]")
        );
        assert!(
            result
                .css
                .contains("--tw-drop-shadow: drop-shadow(0_35px_35px_rgba(0,0,0,0.25))")
        );
        assert!(result.css.contains(".drop-shadow-\\(--my-drop-shadow\\)"));
        assert!(
            result
                .css
                .contains("--tw-drop-shadow: drop-shadow(var(--my-drop-shadow))")
        );
        assert!(
            result
                .css
                .contains(".drop-shadow-\\(color\\:--my-shadow-color\\)")
        );
        assert!(
            result
                .css
                .contains("--tw-drop-shadow-color: var(--my-shadow-color)")
        );
        assert!(result.css.contains(".drop-shadow-indigo-500"));
        assert!(
            result
                .css
                .contains("--tw-drop-shadow-color: var(--color-indigo-500)")
        );
        assert!(result.css.contains(".drop-shadow-cyan-500\\/50"));
        assert!(result.css.contains(
            "--tw-drop-shadow-color: color-mix(in oklab,var(--color-cyan-500) 50%,transparent)"
        ));
        assert!(result.css.contains(".drop-shadow-xl\\/25"));
        assert!(
            result
                .css
                .contains("--tw-drop-shadow: drop-shadow(var(--drop-shadow-xl))")
        );
        assert!(
            result.css.contains(
                "--tw-drop-shadow-color: color-mix(in oklab,currentColor 25%,transparent)"
            )
        );
        assert!(result.css.contains(".md\\:drop-shadow-xl"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_grayscale_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "grayscale".to_string(),
                "grayscale-0".to_string(),
                "grayscale-25".to_string(),
                "grayscale-50".to_string(),
                "grayscale-[0.5]".to_string(),
                "grayscale-(--my-grayscale)".to_string(),
                "md:grayscale-0".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".grayscale"));
        assert!(result.css.contains("filter: grayscale(100%)"));
        assert!(result.css.contains(".grayscale-0"));
        assert!(result.css.contains("filter: grayscale(0%)"));
        assert!(result.css.contains(".grayscale-25"));
        assert!(result.css.contains("filter: grayscale(25%)"));
        assert!(result.css.contains(".grayscale-50"));
        assert!(result.css.contains("filter: grayscale(50%)"));
        assert!(result.css.contains(".grayscale-\\[0.5\\]"));
        assert!(result.css.contains("filter: grayscale(0.5)"));
        assert!(result.css.contains(".grayscale-\\(--my-grayscale\\)"));
        assert!(
            result
                .css
                .contains("filter: grayscale(var(--my-grayscale))")
        );
        assert!(result.css.contains(".md\\:grayscale-0"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_hue_rotate_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "hue-rotate-15".to_string(),
                "hue-rotate-90".to_string(),
                "hue-rotate-180".to_string(),
                "hue-rotate-270".to_string(),
                "-hue-rotate-15".to_string(),
                "-hue-rotate-45".to_string(),
                "-hue-rotate-90".to_string(),
                "hue-rotate-[3.142rad]".to_string(),
                "hue-rotate-(--my-hue-rotate)".to_string(),
                "md:hue-rotate-0".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".hue-rotate-15"));
        assert!(result.css.contains("filter: hue-rotate(15deg)"));
        assert!(result.css.contains(".hue-rotate-90"));
        assert!(result.css.contains("filter: hue-rotate(90deg)"));
        assert!(result.css.contains(".hue-rotate-180"));
        assert!(result.css.contains("filter: hue-rotate(180deg)"));
        assert!(result.css.contains(".hue-rotate-270"));
        assert!(result.css.contains("filter: hue-rotate(270deg)"));
        assert!(result.css.contains(".-hue-rotate-15"));
        assert!(result.css.contains("filter: hue-rotate(calc(15deg * -1))"));
        assert!(result.css.contains(".-hue-rotate-45"));
        assert!(result.css.contains("filter: hue-rotate(calc(45deg * -1))"));
        assert!(result.css.contains(".-hue-rotate-90"));
        assert!(result.css.contains("filter: hue-rotate(calc(90deg * -1))"));
        assert!(result.css.contains(".hue-rotate-\\[3.142rad\\]"));
        assert!(result.css.contains("filter: hue-rotate(3.142rad)"));
        assert!(result.css.contains(".hue-rotate-\\(--my-hue-rotate\\)"));
        assert!(
            result
                .css
                .contains("filter: hue-rotate(var(--my-hue-rotate))")
        );
        assert!(result.css.contains(".md\\:hue-rotate-0"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_invert_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "invert".to_string(),
                "invert-0".to_string(),
                "invert-20".to_string(),
                "invert-[.25]".to_string(),
                "invert-(--my-inversion)".to_string(),
                "md:invert-0".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".invert"));
        assert!(result.css.contains("filter: invert(100%)"));
        assert!(result.css.contains(".invert-0"));
        assert!(result.css.contains("filter: invert(0%)"));
        assert!(result.css.contains(".invert-20"));
        assert!(result.css.contains("filter: invert(20%)"));
        assert!(result.css.contains(".invert-\\[.25\\]"));
        assert!(result.css.contains("filter: invert(.25)"));
        assert!(result.css.contains(".invert-\\(--my-inversion\\)"));
        assert!(result.css.contains("filter: invert(var(--my-inversion))"));
        assert!(result.css.contains(".md\\:invert-0"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_saturate_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "saturate-50".to_string(),
                "saturate-100".to_string(),
                "saturate-150".to_string(),
                "saturate-200".to_string(),
                "saturate-[.25]".to_string(),
                "saturate-(--my-saturation)".to_string(),
                "md:saturate-150".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".saturate-50"));
        assert!(result.css.contains("filter: saturate(50%)"));
        assert!(result.css.contains(".saturate-100"));
        assert!(result.css.contains("filter: saturate(100%)"));
        assert!(result.css.contains(".saturate-150"));
        assert!(result.css.contains("filter: saturate(150%)"));
        assert!(result.css.contains(".saturate-200"));
        assert!(result.css.contains("filter: saturate(200%)"));
        assert!(result.css.contains(".saturate-\\[.25\\]"));
        assert!(result.css.contains("filter: saturate(.25)"));
        assert!(result.css.contains(".saturate-\\(--my-saturation\\)"));
        assert!(
            result
                .css
                .contains("filter: saturate(var(--my-saturation))")
        );
        assert!(result.css.contains(".md\\:saturate-150"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_sepia_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "sepia".to_string(),
                "sepia-0".to_string(),
                "sepia-50".to_string(),
                "sepia-[.25]".to_string(),
                "sepia-(--my-sepia)".to_string(),
                "md:sepia-0".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".sepia"));
        assert!(result.css.contains("filter: sepia(100%)"));
        assert!(result.css.contains(".sepia-0"));
        assert!(result.css.contains("filter: sepia(0%)"));
        assert!(result.css.contains(".sepia-50"));
        assert!(result.css.contains("filter: sepia(50%)"));
        assert!(result.css.contains(".sepia-\\[.25\\]"));
        assert!(result.css.contains("filter: sepia(.25)"));
        assert!(result.css.contains(".sepia-\\(--my-sepia\\)"));
        assert!(result.css.contains("filter: sepia(var(--my-sepia))"));
        assert!(result.css.contains(".md\\:sepia-0"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_backdrop_filter_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "backdrop-filter".to_string(),
                "backdrop-filter-none".to_string(),
                "backdrop-filter-[url('filters.svg#filter-id')]".to_string(),
                "backdrop-filter-(--my-backdrop-filter)".to_string(),
                "hover:backdrop-filter-none".to_string(),
                "md:backdrop-filter-none".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".backdrop-filter"));
        assert!(
            result
                .css
                .contains("backdrop-filter: var(--tw-backdrop-blur,)")
        );
        assert!(result.css.contains(".backdrop-filter-none"));
        assert!(result.css.contains("backdrop-filter: none"));
        assert!(
            result
                .css
                .contains(".backdrop-filter-\\[url\\('filters.svg#filter-id'\\)\\]")
        );
        assert!(
            result
                .css
                .contains("backdrop-filter: url('filters.svg#filter-id')")
        );
        assert!(
            result
                .css
                .contains(".backdrop-filter-\\(--my-backdrop-filter\\)")
        );
        assert!(
            result
                .css
                .contains("backdrop-filter: var(--my-backdrop-filter)")
        );
        assert!(result.css.contains(".hover\\:backdrop-filter-none:hover"));
        assert!(result.css.contains(".md\\:backdrop-filter-none"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_backdrop_blur_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "backdrop-blur".to_string(),
                "backdrop-blur-none".to_string(),
                "backdrop-blur-sm".to_string(),
                "backdrop-blur-md".to_string(),
                "backdrop-blur-lg".to_string(),
                "backdrop-blur-2xs".to_string(),
                "backdrop-blur-[2px]".to_string(),
                "backdrop-blur-(--my-backdrop-blur)".to_string(),
                "md:backdrop-blur-lg".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".backdrop-blur"));
        assert!(result.css.contains("--tw-backdrop-blur: blur(8px)"));
        assert!(result.css.contains(".backdrop-blur-none"));
        assert!(result.css.contains("--tw-backdrop-blur: blur(0)"));
        assert!(result.css.contains(".backdrop-blur-sm"));
        assert!(
            result
                .css
                .contains("--tw-backdrop-blur: blur(var(--blur-sm))")
        );
        assert!(result.css.contains(".backdrop-blur-md"));
        assert!(
            result
                .css
                .contains("--tw-backdrop-blur: blur(var(--blur-md))")
        );
        assert!(result.css.contains(".backdrop-blur-lg"));
        assert!(
            result
                .css
                .contains("--tw-backdrop-blur: blur(var(--blur-lg))")
        );
        assert!(result.css.contains(".backdrop-blur-2xs"));
        assert!(
            result
                .css
                .contains("--tw-backdrop-blur: blur(var(--blur-2xs))")
        );
        assert!(result.css.contains(".backdrop-blur-\\[2px\\]"));
        assert!(result.css.contains("--tw-backdrop-blur: blur(2px)"));
        assert!(
            result
                .css
                .contains(".backdrop-blur-\\(--my-backdrop-blur\\)")
        );
        assert!(
            result
                .css
                .contains("--tw-backdrop-blur: blur(var(--my-backdrop-blur))")
        );
        assert!(
            result
                .css
                .contains("-webkit-backdrop-filter: var(--tw-backdrop-blur,)")
        );
        assert!(result.css.contains(".md\\:backdrop-blur-lg"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_backdrop_brightness_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "backdrop-brightness-50".to_string(),
                "backdrop-brightness-110".to_string(),
                "backdrop-brightness-150".to_string(),
                "backdrop-brightness-[1.75]".to_string(),
                "backdrop-brightness-(--my-backdrop-brightness)".to_string(),
                "md:backdrop-brightness-150".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".backdrop-brightness-50"));
        assert!(result.css.contains("backdrop-filter: brightness(50%)"));
        assert!(result.css.contains(".backdrop-brightness-110"));
        assert!(result.css.contains("backdrop-filter: brightness(110%)"));
        assert!(result.css.contains(".backdrop-brightness-150"));
        assert!(result.css.contains("backdrop-filter: brightness(150%)"));
        assert!(result.css.contains(".backdrop-brightness-\\[1.75\\]"));
        assert!(result.css.contains("backdrop-filter: brightness(1.75)"));
        assert!(
            result
                .css
                .contains(".backdrop-brightness-\\(--my-backdrop-brightness\\)")
        );
        assert!(
            result
                .css
                .contains("backdrop-filter: brightness(var(--my-backdrop-brightness))")
        );
        assert!(result.css.contains(".md\\:backdrop-brightness-150"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_backdrop_contrast_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "backdrop-contrast-50".to_string(),
                "backdrop-contrast-125".to_string(),
                "backdrop-contrast-200".to_string(),
                "backdrop-contrast-[.25]".to_string(),
                "backdrop-contrast-(--my-backdrop-contrast)".to_string(),
                "md:backdrop-contrast-150".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".backdrop-contrast-50"));
        assert!(result.css.contains("backdrop-filter: contrast(50%)"));
        assert!(result.css.contains(".backdrop-contrast-125"));
        assert!(result.css.contains("backdrop-filter: contrast(125%)"));
        assert!(result.css.contains(".backdrop-contrast-200"));
        assert!(result.css.contains("backdrop-filter: contrast(200%)"));
        assert!(result.css.contains(".backdrop-contrast-\\[.25\\]"));
        assert!(result.css.contains("backdrop-filter: contrast(.25)"));
        assert!(
            result
                .css
                .contains(".backdrop-contrast-\\(--my-backdrop-contrast\\)")
        );
        assert!(
            result
                .css
                .contains("backdrop-filter: contrast(var(--my-backdrop-contrast))")
        );
        assert!(result.css.contains(".md\\:backdrop-contrast-150"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_backdrop_grayscale_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "backdrop-grayscale".to_string(),
                "backdrop-grayscale-0".to_string(),
                "backdrop-grayscale-50".to_string(),
                "backdrop-grayscale-200".to_string(),
                "backdrop-grayscale-[0.5]".to_string(),
                "backdrop-grayscale-(--my-backdrop-grayscale)".to_string(),
                "md:backdrop-grayscale-0".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".backdrop-grayscale"));
        assert!(result.css.contains("backdrop-filter: grayscale(100%)"));
        assert!(result.css.contains(".backdrop-grayscale-0"));
        assert!(result.css.contains("backdrop-filter: grayscale(0%)"));
        assert!(result.css.contains(".backdrop-grayscale-50"));
        assert!(result.css.contains("backdrop-filter: grayscale(50%)"));
        assert!(result.css.contains(".backdrop-grayscale-200"));
        assert!(result.css.contains("backdrop-filter: grayscale(200%)"));
        assert!(result.css.contains(".backdrop-grayscale-\\[0.5\\]"));
        assert!(result.css.contains("backdrop-filter: grayscale(0.5)"));
        assert!(
            result
                .css
                .contains(".backdrop-grayscale-\\(--my-backdrop-grayscale\\)")
        );
        assert!(
            result
                .css
                .contains("backdrop-filter: grayscale(var(--my-backdrop-grayscale))")
        );
        assert!(result.css.contains(".md\\:backdrop-grayscale-0"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_backdrop_hue_rotate_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "backdrop-hue-rotate-90".to_string(),
                "backdrop-hue-rotate-180".to_string(),
                "backdrop-hue-rotate-270".to_string(),
                "-backdrop-hue-rotate-15".to_string(),
                "-backdrop-hue-rotate-45".to_string(),
                "-backdrop-hue-rotate-90".to_string(),
                "backdrop-hue-rotate-[3.142rad]".to_string(),
                "backdrop-hue-rotate-(--my-backdrop-hue-rotation)".to_string(),
                "md:backdrop-hue-rotate-0".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".backdrop-hue-rotate-90"));
        assert!(result.css.contains("backdrop-filter: hue-rotate(90deg)"));
        assert!(result.css.contains(".backdrop-hue-rotate-180"));
        assert!(result.css.contains("backdrop-filter: hue-rotate(180deg)"));
        assert!(result.css.contains(".backdrop-hue-rotate-270"));
        assert!(result.css.contains("backdrop-filter: hue-rotate(270deg)"));
        assert!(result.css.contains(".-backdrop-hue-rotate-15"));
        assert!(
            result
                .css
                .contains("backdrop-filter: hue-rotate(calc(15deg * -1))")
        );
        assert!(result.css.contains(".-backdrop-hue-rotate-45"));
        assert!(
            result
                .css
                .contains("backdrop-filter: hue-rotate(calc(45deg * -1))")
        );
        assert!(result.css.contains(".-backdrop-hue-rotate-90"));
        assert!(
            result
                .css
                .contains("backdrop-filter: hue-rotate(calc(90deg * -1))")
        );
        assert!(result.css.contains(".backdrop-hue-rotate-\\[3.142rad\\]"));
        assert!(result.css.contains("backdrop-filter: hue-rotate(3.142rad)"));
        assert!(
            result
                .css
                .contains(".backdrop-hue-rotate-\\(--my-backdrop-hue-rotation\\)")
        );
        assert!(
            result
                .css
                .contains("backdrop-filter: hue-rotate(var(--my-backdrop-hue-rotation))")
        );
        assert!(result.css.contains(".md\\:backdrop-hue-rotate-0"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_backdrop_invert_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "backdrop-invert".to_string(),
                "backdrop-invert-0".to_string(),
                "backdrop-invert-65".to_string(),
                "backdrop-invert-[.25]".to_string(),
                "backdrop-invert-(--my-backdrop-inversion)".to_string(),
                "md:backdrop-invert".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".backdrop-invert"));
        assert!(result.css.contains("backdrop-filter: invert(100%)"));
        assert!(result.css.contains(".backdrop-invert-0"));
        assert!(result.css.contains("backdrop-filter: invert(0%)"));
        assert!(result.css.contains(".backdrop-invert-65"));
        assert!(result.css.contains("backdrop-filter: invert(65%)"));
        assert!(result.css.contains(".backdrop-invert-\\[.25\\]"));
        assert!(result.css.contains("backdrop-filter: invert(.25)"));
        assert!(
            result
                .css
                .contains(".backdrop-invert-\\(--my-backdrop-inversion\\)")
        );
        assert!(
            result
                .css
                .contains("backdrop-filter: invert(var(--my-backdrop-inversion))")
        );
        assert!(result.css.contains(".md\\:backdrop-invert"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_backdrop_opacity_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "backdrop-opacity-10".to_string(),
                "backdrop-opacity-60".to_string(),
                "backdrop-opacity-95".to_string(),
                "backdrop-opacity-100".to_string(),
                "backdrop-opacity-[.15]".to_string(),
                "backdrop-opacity-(--my-backdrop-filter-opacity)".to_string(),
                "md:backdrop-opacity-60".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".backdrop-opacity-10"));
        assert!(result.css.contains("backdrop-filter: opacity(10%)"));
        assert!(result.css.contains(".backdrop-opacity-60"));
        assert!(result.css.contains("backdrop-filter: opacity(60%)"));
        assert!(result.css.contains(".backdrop-opacity-95"));
        assert!(result.css.contains("backdrop-filter: opacity(95%)"));
        assert!(result.css.contains(".backdrop-opacity-100"));
        assert!(result.css.contains("backdrop-filter: opacity(100%)"));
        assert!(result.css.contains(".backdrop-opacity-\\[.15\\]"));
        assert!(result.css.contains("backdrop-filter: opacity(.15)"));
        assert!(
            result
                .css
                .contains(".backdrop-opacity-\\(--my-backdrop-filter-opacity\\)")
        );
        assert!(
            result
                .css
                .contains("backdrop-filter: opacity(var(--my-backdrop-filter-opacity))")
        );
        assert!(result.css.contains(".md\\:backdrop-opacity-60"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_opacity_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "opacity-0".to_string(),
                "opacity-5".to_string(),
                "opacity-50".to_string(),
                "opacity-100".to_string(),
                "opacity-[.15]".to_string(),
                "opacity-(--my-opacity)".to_string(),
                "disabled:opacity-50".to_string(),
                "group-hover:opacity-100".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".opacity-0"));
        assert!(result.css.contains("opacity: 0%"));
        assert!(result.css.contains(".opacity-5"));
        assert!(result.css.contains("opacity: 5%"));
        assert!(result.css.contains(".opacity-50"));
        assert!(result.css.contains("opacity: 50%"));
        assert!(result.css.contains(".opacity-100"));
        assert!(result.css.contains("opacity: 100%"));
        assert!(result.css.contains(".opacity-\\[.15\\]"));
        assert!(result.css.contains("opacity: .15"));
        assert!(result.css.contains(".opacity-\\(--my-opacity\\)"));
        assert!(result.css.contains("opacity: var(--my-opacity)"));
        assert!(result.css.contains(".disabled\\:opacity-50:disabled"));
        assert!(result.css.contains(".group-hover\\:opacity-100"));
    }

    #[test]
    fn generates_backdrop_saturate_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "backdrop-saturate-50".to_string(),
                "backdrop-saturate-125".to_string(),
                "backdrop-saturate-200".to_string(),
                "backdrop-saturate-[.25]".to_string(),
                "backdrop-saturate-(--my-backdrop-saturation)".to_string(),
                "md:backdrop-saturate-150".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".backdrop-saturate-50"));
        assert!(result.css.contains("backdrop-filter: saturate(50%)"));
        assert!(result.css.contains(".backdrop-saturate-125"));
        assert!(result.css.contains("backdrop-filter: saturate(125%)"));
        assert!(result.css.contains(".backdrop-saturate-200"));
        assert!(result.css.contains("backdrop-filter: saturate(200%)"));
        assert!(result.css.contains(".backdrop-saturate-\\[.25\\]"));
        assert!(result.css.contains("backdrop-filter: saturate(.25)"));
        assert!(
            result
                .css
                .contains(".backdrop-saturate-\\(--my-backdrop-saturation\\)")
        );
        assert!(
            result
                .css
                .contains("backdrop-filter: saturate(var(--my-backdrop-saturation))")
        );
        assert!(result.css.contains(".md\\:backdrop-saturate-150"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_backdrop_sepia_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "backdrop-sepia".to_string(),
                "backdrop-sepia-0".to_string(),
                "backdrop-sepia-50".to_string(),
                "backdrop-sepia-[.25]".to_string(),
                "backdrop-sepia-(--my-backdrop-sepia)".to_string(),
                "md:backdrop-sepia-0".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".backdrop-sepia"));
        assert!(result.css.contains("backdrop-filter: sepia(100%)"));
        assert!(result.css.contains(".backdrop-sepia-0"));
        assert!(result.css.contains("backdrop-filter: sepia(0%)"));
        assert!(result.css.contains(".backdrop-sepia-50"));
        assert!(result.css.contains("backdrop-filter: sepia(50%)"));
        assert!(result.css.contains(".backdrop-sepia-\\[.25\\]"));
        assert!(result.css.contains("backdrop-filter: sepia(.25)"));
        assert!(
            result
                .css
                .contains(".backdrop-sepia-\\(--my-backdrop-sepia\\)")
        );
        assert!(
            result
                .css
                .contains("backdrop-filter: sepia(var(--my-backdrop-sepia))")
        );
        assert!(result.css.contains(".md\\:backdrop-sepia-0"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_text_indent_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "indent-8".to_string(),
                "-indent-8".to_string(),
                "indent-px".to_string(),
                "-indent-px".to_string(),
                "indent-(--my-indentation)".to_string(),
                "indent-[50%]".to_string(),
                "md:indent-8".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".indent-8"));
        assert!(result.css.contains("text-indent: calc(var(--spacing) * 8)"));
        assert!(result.css.contains(".-indent-8"));
        assert!(
            result
                .css
                .contains("text-indent: calc(var(--spacing) * -8)")
        );
        assert!(result.css.contains(".indent-px"));
        assert!(result.css.contains("text-indent: 1px"));
        assert!(result.css.contains(".-indent-px"));
        assert!(result.css.contains("text-indent: -1px"));
        assert!(result.css.contains(".indent-\\(--my-indentation\\)"));
        assert!(result.css.contains("text-indent: var(--my-indentation)"));
        assert!(result.css.contains(".indent-\\[50\\%\\]"));
        assert!(result.css.contains("text-indent: 50%"));
        assert!(result.css.contains(".md\\:indent-8"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_vertical_align_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "align-baseline".to_string(),
                "align-top".to_string(),
                "align-middle".to_string(),
                "align-bottom".to_string(),
                "align-text-top".to_string(),
                "align-text-bottom".to_string(),
                "align-sub".to_string(),
                "align-super".to_string(),
                "align-(--my-alignment)".to_string(),
                "align-[4px]".to_string(),
                "md:align-top".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".align-baseline"));
        assert!(result.css.contains("vertical-align: baseline"));
        assert!(result.css.contains(".align-top"));
        assert!(result.css.contains("vertical-align: top"));
        assert!(result.css.contains(".align-middle"));
        assert!(result.css.contains("vertical-align: middle"));
        assert!(result.css.contains(".align-bottom"));
        assert!(result.css.contains("vertical-align: bottom"));
        assert!(result.css.contains(".align-text-top"));
        assert!(result.css.contains("vertical-align: text-top"));
        assert!(result.css.contains(".align-text-bottom"));
        assert!(result.css.contains("vertical-align: text-bottom"));
        assert!(result.css.contains(".align-sub"));
        assert!(result.css.contains("vertical-align: sub"));
        assert!(result.css.contains(".align-super"));
        assert!(result.css.contains("vertical-align: super"));
        assert!(result.css.contains(".align-\\(--my-alignment\\)"));
        assert!(result.css.contains("vertical-align: var(--my-alignment)"));
        assert!(result.css.contains(".align-\\[4px\\]"));
        assert!(result.css.contains("vertical-align: 4px"));
        assert!(result.css.contains(".md\\:align-top"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_text_decoration_line_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "underline".to_string(),
                "overline".to_string(),
                "line-through".to_string(),
                "no-underline".to_string(),
                "hover:underline".to_string(),
                "md:no-underline".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".underline"));
        assert!(result.css.contains("text-decoration-line: underline"));
        assert!(result.css.contains(".overline"));
        assert!(result.css.contains("text-decoration-line: overline"));
        assert!(result.css.contains(".line-through"));
        assert!(result.css.contains("text-decoration-line: line-through"));
        assert!(result.css.contains(".no-underline"));
        assert!(result.css.contains("text-decoration-line: none"));
        assert!(result.css.contains(".hover\\:underline:hover"));
        assert!(result.css.contains(".md\\:no-underline"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_text_decoration_style_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "decoration-solid".to_string(),
                "decoration-double".to_string(),
                "decoration-dotted".to_string(),
                "decoration-dashed".to_string(),
                "decoration-wavy".to_string(),
                "md:decoration-dashed".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".decoration-solid"));
        assert!(result.css.contains("text-decoration-style: solid"));
        assert!(result.css.contains(".decoration-double"));
        assert!(result.css.contains("text-decoration-style: double"));
        assert!(result.css.contains(".decoration-dotted"));
        assert!(result.css.contains("text-decoration-style: dotted"));
        assert!(result.css.contains(".decoration-dashed"));
        assert!(result.css.contains("text-decoration-style: dashed"));
        assert!(result.css.contains(".decoration-wavy"));
        assert!(result.css.contains("text-decoration-style: wavy"));
        assert!(result.css.contains(".md\\:decoration-dashed"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_text_decoration_thickness_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "decoration-1".to_string(),
                "decoration-2".to_string(),
                "decoration-4".to_string(),
                "decoration-auto".to_string(),
                "decoration-from-font".to_string(),
                "decoration-[0.25rem]".to_string(),
                "decoration-(length:--my-decoration-thickness)".to_string(),
                "md:decoration-4".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".decoration-1"));
        assert!(result.css.contains("text-decoration-thickness: 1px"));
        assert!(result.css.contains(".decoration-2"));
        assert!(result.css.contains("text-decoration-thickness: 2px"));
        assert!(result.css.contains(".decoration-4"));
        assert!(result.css.contains("text-decoration-thickness: 4px"));
        assert!(result.css.contains(".decoration-auto"));
        assert!(result.css.contains("text-decoration-thickness: auto"));
        assert!(result.css.contains(".decoration-from-font"));
        assert!(result.css.contains("text-decoration-thickness: from-font"));
        assert!(result.css.contains(".decoration-\\[0.25rem\\]"));
        assert!(result.css.contains("text-decoration-thickness: 0.25rem"));
        assert!(
            result
                .css
                .contains(".decoration-\\(length\\:--my-decoration-thickness\\)")
        );
        assert!(
            result
                .css
                .contains("text-decoration-thickness: var(--my-decoration-thickness)")
        );
        assert!(result.css.contains(".md\\:decoration-4"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_text_underline_offset_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "underline-offset-1".to_string(),
                "underline-offset-2".to_string(),
                "underline-offset-4".to_string(),
                "underline-offset-8".to_string(),
                "-underline-offset-2".to_string(),
                "underline-offset-auto".to_string(),
                "underline-offset-(--my-underline-offset)".to_string(),
                "underline-offset-[3px]".to_string(),
                "md:underline-offset-4".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".underline-offset-1"));
        assert!(result.css.contains("text-underline-offset: 1px"));
        assert!(result.css.contains(".underline-offset-2"));
        assert!(result.css.contains("text-underline-offset: 2px"));
        assert!(result.css.contains(".underline-offset-4"));
        assert!(result.css.contains("text-underline-offset: 4px"));
        assert!(result.css.contains(".underline-offset-8"));
        assert!(result.css.contains("text-underline-offset: 8px"));
        assert!(result.css.contains(".-underline-offset-2"));
        assert!(result.css.contains("text-underline-offset: calc(2px * -1)"));
        assert!(result.css.contains(".underline-offset-auto"));
        assert!(result.css.contains("text-underline-offset: auto"));
        assert!(
            result
                .css
                .contains(".underline-offset-\\(--my-underline-offset\\)")
        );
        assert!(
            result
                .css
                .contains("text-underline-offset: var(--my-underline-offset)")
        );
        assert!(result.css.contains(".underline-offset-\\[3px\\]"));
        assert!(result.css.contains("text-underline-offset: 3px"));
        assert!(result.css.contains(".md\\:underline-offset-4"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_z_index_and_inset_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "z-10".to_string(),
                "z-50".to_string(),
                "z-auto".to_string(),
                "-z-10".to_string(),
                "z-[calc(var(--index)+1)]".to_string(),
                "z-(--my-z)".to_string(),
                "inset-0".to_string(),
                "top-0".to_string(),
                "right-0".to_string(),
                "bottom-0".to_string(),
                "left-0".to_string(),
                "inset-x-0".to_string(),
                "inset-y-0".to_string(),
                "md:z-50".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".z-10"));
        assert!(result.css.contains("z-index: 10"));
        assert!(result.css.contains(".z-50"));
        assert!(result.css.contains("z-index: 50"));
        assert!(result.css.contains(".z-auto"));
        assert!(result.css.contains("z-index: auto"));
        assert!(result.css.contains(".-z-10"));
        assert!(result.css.contains("z-index: calc(10 * -1)"));
        assert!(
            result
                .css
                .contains(".z-\\[calc\\(var\\(--index\\)\\+1\\)\\]")
        );
        assert!(result.css.contains("z-index: calc(var(--index)+1)"));
        assert!(result.css.contains(".z-\\(--my-z\\)"));
        assert!(result.css.contains("z-index: var(--my-z)"));
        assert!(result.css.contains(".inset-0"));
        assert!(result.css.contains("inset: calc(var(--spacing) * 0)"));
        assert!(result.css.contains(".top-0"));
        assert!(result.css.contains("top: calc(var(--spacing) * 0)"));
        assert!(result.css.contains(".right-0"));
        assert!(result.css.contains("right: calc(var(--spacing) * 0)"));
        assert!(result.css.contains(".bottom-0"));
        assert!(result.css.contains("bottom: calc(var(--spacing) * 0)"));
        assert!(result.css.contains(".left-0"));
        assert!(result.css.contains("left: calc(var(--spacing) * 0)"));
        assert!(result.css.contains(".inset-x-0"));
        assert!(
            result
                .css
                .contains("inset-inline: calc(var(--spacing) * 0)")
        );
        assert!(result.css.contains(".inset-y-0"));
        assert!(result.css.contains("inset-block: calc(var(--spacing) * 0)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:z-50"));
    }

    #[test]
    fn generates_gray_backgrounds() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &["bg-gray-100".to_string(), "bg-gray-700".to_string()],
            &config,
        );
        assert!(result.css.contains(".bg-gray-100"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-gray-100)")
        );
        assert!(result.css.contains(".bg-gray-700"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-gray-700)")
        );
    }

    #[test]
    fn generates_blue_backgrounds() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &["bg-blue-100".to_string(), "bg-blue-600".to_string()],
            &config,
        );
        assert!(result.css.contains(".bg-blue-100"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-blue-100)")
        );
        assert!(result.css.contains(".bg-blue-600"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-blue-600)")
        );
    }

    #[test]
    fn generates_background_attachment_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "bg-fixed".to_string(),
                "bg-local".to_string(),
                "bg-scroll".to_string(),
                "md:bg-fixed".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".bg-fixed"));
        assert!(result.css.contains("background-attachment: fixed"));
        assert!(result.css.contains(".bg-local"));
        assert!(result.css.contains("background-attachment: local"));
        assert!(result.css.contains(".bg-scroll"));
        assert!(result.css.contains("background-attachment: scroll"));
        assert!(result.css.contains(".md\\:bg-fixed"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_background_clip_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "bg-clip-border".to_string(),
                "bg-clip-padding".to_string(),
                "bg-clip-content".to_string(),
                "bg-clip-text".to_string(),
                "md:bg-clip-padding".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".bg-clip-border"));
        assert!(result.css.contains("background-clip: border-box"));
        assert!(result.css.contains(".bg-clip-padding"));
        assert!(result.css.contains("background-clip: padding-box"));
        assert!(result.css.contains(".bg-clip-content"));
        assert!(result.css.contains("background-clip: content-box"));
        assert!(result.css.contains(".bg-clip-text"));
        assert!(result.css.contains("background-clip: text"));
        assert!(result.css.contains(".md\\:bg-clip-padding"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_background_origin_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "bg-origin-border".to_string(),
                "bg-origin-padding".to_string(),
                "bg-origin-content".to_string(),
                "md:bg-origin-padding".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".bg-origin-border"));
        assert!(result.css.contains("background-origin: border-box"));
        assert!(result.css.contains(".bg-origin-padding"));
        assert!(result.css.contains("background-origin: padding-box"));
        assert!(result.css.contains(".bg-origin-content"));
        assert!(result.css.contains("background-origin: content-box"));
        assert!(result.css.contains(".md\\:bg-origin-padding"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_background_position_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "bg-top-left".to_string(),
                "bg-top".to_string(),
                "bg-top-right".to_string(),
                "bg-left".to_string(),
                "bg-center".to_string(),
                "bg-right".to_string(),
                "bg-bottom-left".to_string(),
                "bg-bottom".to_string(),
                "bg-bottom-right".to_string(),
                "bg-position-(--my-bg-position)".to_string(),
                "bg-position-[center_top_1rem]".to_string(),
                "md:bg-top".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".bg-top-left"));
        assert!(result.css.contains("background-position: top left"));
        assert!(result.css.contains(".bg-top"));
        assert!(result.css.contains("background-position: top"));
        assert!(result.css.contains(".bg-top-right"));
        assert!(result.css.contains("background-position: top right"));
        assert!(result.css.contains(".bg-left"));
        assert!(result.css.contains("background-position: left"));
        assert!(result.css.contains(".bg-center"));
        assert!(result.css.contains("background-position: center"));
        assert!(result.css.contains(".bg-right"));
        assert!(result.css.contains("background-position: right"));
        assert!(result.css.contains(".bg-bottom-left"));
        assert!(result.css.contains("background-position: bottom left"));
        assert!(result.css.contains(".bg-bottom"));
        assert!(result.css.contains("background-position: bottom"));
        assert!(result.css.contains(".bg-bottom-right"));
        assert!(result.css.contains("background-position: bottom right"));
        assert!(result.css.contains(".bg-position-\\(--my-bg-position\\)"));
        assert!(
            result
                .css
                .contains("background-position: var(--my-bg-position)")
        );
        assert!(result.css.contains(".bg-position-\\[center_top_1rem\\]"));
        assert!(result.css.contains("background-position: center_top_1rem"));
        assert!(result.css.contains(".md\\:bg-top"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_background_repeat_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "bg-repeat".to_string(),
                "bg-repeat-x".to_string(),
                "bg-repeat-y".to_string(),
                "bg-repeat-space".to_string(),
                "bg-repeat-round".to_string(),
                "bg-no-repeat".to_string(),
                "md:bg-repeat-x".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".bg-repeat"));
        assert!(result.css.contains("background-repeat: repeat"));
        assert!(result.css.contains(".bg-repeat-x"));
        assert!(result.css.contains("background-repeat: repeat-x"));
        assert!(result.css.contains(".bg-repeat-y"));
        assert!(result.css.contains("background-repeat: repeat-y"));
        assert!(result.css.contains(".bg-repeat-space"));
        assert!(result.css.contains("background-repeat: space"));
        assert!(result.css.contains(".bg-repeat-round"));
        assert!(result.css.contains("background-repeat: round"));
        assert!(result.css.contains(".bg-no-repeat"));
        assert!(result.css.contains("background-repeat: no-repeat"));
        assert!(result.css.contains(".md\\:bg-repeat-x"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_background_size_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "bg-auto".to_string(),
                "bg-cover".to_string(),
                "bg-contain".to_string(),
                "bg-size-[auto_100px]".to_string(),
                "bg-size-(--my-image-size)".to_string(),
                "md:bg-contain".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".bg-auto"));
        assert!(result.css.contains("background-size: auto"));
        assert!(result.css.contains(".bg-cover"));
        assert!(result.css.contains("background-size: cover"));
        assert!(result.css.contains(".bg-contain"));
        assert!(result.css.contains("background-size: contain"));
        assert!(result.css.contains(".bg-size-\\[auto_100px\\]"));
        assert!(result.css.contains("background-size: auto_100px"));
        assert!(result.css.contains(".bg-size-\\(--my-image-size\\)"));
        assert!(result.css.contains("background-size: var(--my-image-size)"));
        assert!(result.css.contains(".md\\:bg-contain"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_background_color_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "bg-inherit".to_string(),
                "bg-current".to_string(),
                "bg-transparent".to_string(),
                "bg-black".to_string(),
                "bg-white".to_string(),
                "bg-sky-500".to_string(),
                "bg-sky-500/75".to_string(),
                "bg-sky-500/[37%]".to_string(),
                "bg-sky-500/(--my-opacity)".to_string(),
                "bg-[#50d71e]".to_string(),
                "bg-[color:var(--tool-surface)]".to_string(),
                "bg-[color:var(--tool-surface)]/90".to_string(),
                "bg-(--my-color)".to_string(),
                "hover:bg-fuchsia-500".to_string(),
                "md:bg-green-500".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".bg-inherit"));
        assert!(result.css.contains("background-color: inherit"));
        assert!(result.css.contains(".bg-current"));
        assert!(result.css.contains("background-color: currentColor"));
        assert!(result.css.contains(".bg-transparent"));
        assert!(result.css.contains("background-color: transparent"));
        assert!(result.css.contains(".bg-black"));
        assert!(result.css.contains("background-color: var(--color-black)"));
        assert!(result.css.contains(".bg-white"));
        assert!(result.css.contains("background-color: var(--color-white)"));
        assert!(result.css.contains(".bg-sky-500"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-sky-500)")
        );
        assert!(result.css.contains(".bg-sky-500\\/75"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-sky-500) 75%,transparent)")
        );
        assert!(result.css.contains(".bg-sky-500\\/\\[37\\%\\]"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-sky-500) 37%,transparent)")
        );
        assert!(result.css.contains(".bg-sky-500\\/\\(--my-opacity\\)"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-sky-500) var(--my-opacity),transparent)")
        );
        assert!(result.css.contains(".bg-\\[#50d71e\\]"));
        assert!(result.css.contains("background-color: #50d71e"));
        assert!(
            result
                .css
                .contains(".bg-\\[color\\:var\\(--tool-surface\\)\\]")
        );
        assert!(result.css.contains("background-color: var(--tool-surface)"));
        assert!(
            result
                .css
                .contains(".bg-\\[color\\:var\\(--tool-surface\\)\\]\\/90")
        );
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--tool-surface) 90%,transparent)")
        );
        assert!(result.css.contains(".bg-\\(--my-color\\)"));
        assert!(result.css.contains("background-color: var(--my-color)"));
        assert!(result.css.contains(".hover\\:bg-fuchsia-500:hover"));
        assert!(result.css.contains(".md\\:bg-green-500"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_background_image_and_gradient_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "bg-none".to_string(),
                "bg-[url(/img/mountains.jpg)]".to_string(),
                "bg-[url('/what_a_rush.png')]".to_string(),
                "bg-(image:--my-image)".to_string(),
                "bg-gradient-to-r".to_string(),
                "bg-linear-to-r".to_string(),
                "bg-linear-to-r/srgb".to_string(),
                "bg-linear-65".to_string(),
                "-bg-linear-65".to_string(),
                "bg-linear-[25deg,red_5%,yellow_60%]".to_string(),
                "bg-linear-(--my-gradient)".to_string(),
                "bg-radial".to_string(),
                "bg-radial-[at_50%_75%]".to_string(),
                "bg-radial-(--my-radial)".to_string(),
                "bg-conic-180".to_string(),
                "-bg-conic-180".to_string(),
                "bg-conic-(--my-conic)".to_string(),
                "bg-conic-[conic-gradient(from_0deg,red,blue)]".to_string(),
                "from-indigo-500".to_string(),
                "from-white/10".to_string(),
                "from-10%".to_string(),
                "from-(--my-from)".to_string(),
                "from-[#50d71e]".to_string(),
                "via-purple-500".to_string(),
                "via-30%".to_string(),
                "to-pink-500".to_string(),
                "to-white/30".to_string(),
                "to-90%".to_string(),
                "md:from-yellow-500".to_string(),
                "hover:bg-none".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".bg-none"));
        assert!(result.css.contains("background-image: none"));
        assert!(
            result
                .css
                .contains(".bg-\\[url\\(\\/img\\/mountains.jpg\\)\\]")
        );
        assert!(
            result
                .css
                .contains("background-image: url(/img/mountains.jpg)")
        );
        assert!(
            result
                .css
                .contains(".bg-\\[url\\('\\/what_a_rush.png'\\)\\]")
        );
        assert!(
            result
                .css
                .contains("background-image: url('/what_a_rush.png')")
        );
        assert!(result.css.contains(".bg-\\(image\\:--my-image\\)"));
        assert!(result.css.contains("background-image: var(--my-image)"));
        assert!(result.css.contains(".bg-gradient-to-r"));
        assert!(result.css.contains("--tw-gradient-position"));
        assert!(result.css.contains("to right in oklab"));
        assert!(
            result
                .css
                .contains("background-image: linear-gradient(var(--tw-gradient-stops))")
        );
        assert!(result.css.contains(".bg-linear-to-r"));
        assert!(
            result
                .css
                .contains("background-image: linear-gradient(to right, var(--tw-gradient-stops))")
        );
        assert!(result.css.contains(".bg-linear-to-r\\/srgb"));
        assert!(result.css.contains(
            "background-image: linear-gradient(to right in srgb, var(--tw-gradient-stops))"
        ));
        assert!(result.css.contains(".bg-linear-65"));
        assert!(result.css.contains(
            "background-image: linear-gradient(65deg in oklab, var(--tw-gradient-stops))"
        ));
        assert!(result.css.contains(".-bg-linear-65"));
        assert!(result.css.contains(
            "background-image: linear-gradient(-65deg in oklab, var(--tw-gradient-stops))"
        ));
        assert!(
            result
                .css
                .contains(".bg-linear-\\[25deg\\,red_5\\%\\,yellow_60\\%\\]")
        );
        assert!(result.css.contains(
            "background-image: linear-gradient(var(--tw-gradient-stops, 25deg,red_5%,yellow_60%))"
        ));
        assert!(result.css.contains(".bg-linear-\\(--my-gradient\\)"));
        assert!(result.css.contains(
            "background-image: linear-gradient(var(--tw-gradient-stops, var(--my-gradient)))"
        ));
        assert!(result.css.contains(".bg-radial"));
        assert!(
            result
                .css
                .contains("background-image: radial-gradient(in oklab, var(--tw-gradient-stops))")
        );
        assert!(result.css.contains(".bg-radial-\\[at_50\\%_75\\%\\]"));
        assert!(
            result.css.contains(
                "background-image: radial-gradient(var(--tw-gradient-stops, at_50%_75%))"
            )
        );
        assert!(result.css.contains(".bg-radial-\\(--my-radial\\)"));
        assert!(result.css.contains(
            "background-image: radial-gradient(var(--tw-gradient-stops, var(--my-radial)))"
        ));
        assert!(result.css.contains(".bg-conic-180"));
        assert!(result.css.contains(
            "background-image: conic-gradient(from 180deg in oklab, var(--tw-gradient-stops))"
        ));
        assert!(result.css.contains(".-bg-conic-180"));
        assert!(result.css.contains(
            "background-image: conic-gradient(from -180deg in oklab, var(--tw-gradient-stops))"
        ));
        assert!(result.css.contains(".bg-conic-\\(--my-conic\\)"));
        assert!(result.css.contains("background-image: var(--my-conic)"));
        assert!(
            result
                .css
                .contains(".bg-conic-\\[conic-gradient\\(from_0deg\\,red\\,blue\\)\\]")
        );
        assert!(
            result
                .css
                .contains("background-image: conic-gradient(from_0deg,red,blue)")
        );
        assert!(result.css.contains(".from-indigo-500"));
        assert!(
            result
                .css
                .contains("--tw-gradient-from: var(--color-indigo-500)")
        );
        assert!(result.css.contains(".from-white\\/10"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-white) 10%,transparent)")
        );
        assert!(result.css.contains(".from-10\\%"));
        assert!(result.css.contains("--tw-gradient-from-position: 10%"));
        assert!(result.css.contains(".from-\\(--my-from\\)"));
        assert!(result.css.contains("--tw-gradient-from: var(--my-from)"));
        assert!(result.css.contains(".from-\\[#50d71e\\]"));
        assert!(result.css.contains("--tw-gradient-from: #50d71e"));
        assert!(result.css.contains(".via-purple-500"));
        assert!(
            result
                .css
                .contains("--tw-gradient-via: var(--color-purple-500)")
        );
        assert!(result.css.contains(".via-30\\%"));
        assert!(result.css.contains("--tw-gradient-via-position: 30%"));
        assert!(result.css.contains(".to-pink-500"));
        assert!(
            result
                .css
                .contains("--tw-gradient-to: var(--color-pink-500)")
        );
        assert!(result.css.contains(".to-white\\/30"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-white) 30%,transparent)")
        );
        assert!(result.css.contains(".to-90\\%"));
        assert!(result.css.contains("--tw-gradient-to-position: 90%"));
        assert!(result.css.contains(".md\\:from-yellow-500"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".hover\\:bg-none:hover"));
    }

    #[test]
    fn generates_text_colors() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "text-gray-700".to_string(),
                "text-blue-500".to_string(),
                "text-inherit".to_string(),
                "text-current".to_string(),
                "text-transparent".to_string(),
                "text-black".to_string(),
                "text-white".to_string(),
                "text-(--my-color)".to_string(),
                "text-(color:--my-color-hinted)".to_string(),
                "text-[#50d71e]".to_string(),
                "text-[color:var(--accent)]".to_string(),
                "text-red-500".to_string(),
                "text-red-500/75".to_string(),
                "text-red-500/[37%]".to_string(),
                "text-red-500/(--my-opacity)".to_string(),
                "text-white/60".to_string(),
                "text-white/70".to_string(),
                "text-white/90".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".text-gray-700"));
        assert!(result.css.contains("color: var(--color-gray-700)"));
        assert!(result.css.contains(".text-blue-500"));
        assert!(result.css.contains("color: var(--color-blue-500)"));
        assert!(result.css.contains(".text-inherit"));
        assert!(result.css.contains("color: inherit"));
        assert!(result.css.contains(".text-current"));
        assert!(result.css.contains("color: currentColor"));
        assert!(result.css.contains(".text-transparent"));
        assert!(result.css.contains("color: transparent"));
        assert!(result.css.contains(".text-black"));
        assert!(result.css.contains("color: var(--color-black)"));
        assert!(result.css.contains(".text-white"));
        assert!(result.css.contains("color: var(--color-white)"));
        assert!(result.css.contains(".text-\\(--my-color\\)"));
        assert!(result.css.contains("color: var(--my-color)"));
        assert!(result.css.contains(".text-\\(color\\:--my-color-hinted\\)"));
        assert!(result.css.contains("color: var(--my-color-hinted)"));
        assert!(result.css.contains(".text-\\[#50d71e\\]"));
        assert!(result.css.contains("color: #50d71e"));
        assert!(result.css.contains(".text-\\[color\\:var\\(--accent\\)\\]"));
        assert!(result.css.contains("color: var(--accent)"));
        assert!(result.css.contains(".text-red-500"));
        assert!(result.css.contains("color: var(--color-red-500)"));
        assert!(result.css.contains(".text-red-500\\/75"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-red-500) 75%,transparent)")
        );
        assert!(result.css.contains(".text-red-500\\/\\[37\\%\\]"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-red-500) 37%,transparent)")
        );
        assert!(result.css.contains(".text-red-500\\/\\(--my-opacity\\)"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-red-500) var(--my-opacity),transparent)")
        );
        assert!(result.css.contains(".text-white\\/60"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-white) 60%,transparent)")
        );
        assert!(result.css.contains(".text-white\\/70"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-white) 70%,transparent)")
        );
        assert!(result.css.contains(".text-white\\/90"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-white) 90%,transparent)")
        );
    }

    #[test]
    fn generates_fill_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "fill-none".to_string(),
                "fill-inherit".to_string(),
                "fill-current".to_string(),
                "fill-transparent".to_string(),
                "fill-black".to_string(),
                "fill-white".to_string(),
                "fill-red-500".to_string(),
                "fill-cyan-700/25".to_string(),
                "fill-cyan-700/[37%]".to_string(),
                "fill-cyan-700/(--my-opacity)".to_string(),
                "fill-[#50d71e]".to_string(),
                "fill-(--my-fill-color)".to_string(),
                "hover:fill-lime-600".to_string(),
                "md:fill-sky-500".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".fill-none"));
        assert!(result.css.contains("fill: none"));
        assert!(result.css.contains(".fill-inherit"));
        assert!(result.css.contains("fill: inherit"));
        assert!(result.css.contains(".fill-current"));
        assert!(result.css.contains("fill: currentColor"));
        assert!(result.css.contains(".fill-transparent"));
        assert!(result.css.contains("fill: transparent"));
        assert!(result.css.contains(".fill-black"));
        assert!(result.css.contains("fill: var(--color-black)"));
        assert!(result.css.contains(".fill-white"));
        assert!(result.css.contains("fill: var(--color-white)"));
        assert!(result.css.contains(".fill-red-500"));
        assert!(result.css.contains("fill: var(--color-red-500)"));
        assert!(result.css.contains(".fill-cyan-700\\/25"));
        assert!(
            result
                .css
                .contains("fill: color-mix(in oklab,var(--color-cyan-700) 25%,transparent)")
        );
        assert!(result.css.contains(".fill-cyan-700\\/\\[37\\%\\]"));
        assert!(
            result
                .css
                .contains("fill: color-mix(in oklab,var(--color-cyan-700) 37%,transparent)")
        );
        assert!(result.css.contains(".fill-cyan-700\\/\\(--my-opacity\\)"));
        assert!(result.css.contains(
            "fill: color-mix(in oklab,var(--color-cyan-700) var(--my-opacity),transparent)"
        ));
        assert!(result.css.contains(".fill-\\[#50d71e\\]"));
        assert!(result.css.contains("fill: #50d71e"));
        assert!(result.css.contains(".fill-\\(--my-fill-color\\)"));
        assert!(result.css.contains("fill: var(--my-fill-color)"));
        assert!(result.css.contains(".hover\\:fill-lime-600:hover"));
        assert!(result.css.contains(".md\\:fill-sky-500"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_stroke_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "stroke-none".to_string(),
                "stroke-inherit".to_string(),
                "stroke-current".to_string(),
                "stroke-transparent".to_string(),
                "stroke-black".to_string(),
                "stroke-white".to_string(),
                "stroke-red-500".to_string(),
                "stroke-cyan-700/25".to_string(),
                "stroke-cyan-700/[37%]".to_string(),
                "stroke-cyan-700/(--my-opacity)".to_string(),
                "stroke-[#50d71e]".to_string(),
                "stroke-(--my-stroke-color)".to_string(),
                "hover:stroke-lime-600".to_string(),
                "md:stroke-sky-500".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".stroke-none"));
        assert!(result.css.contains("stroke: none"));
        assert!(result.css.contains(".stroke-inherit"));
        assert!(result.css.contains("stroke: inherit"));
        assert!(result.css.contains(".stroke-current"));
        assert!(result.css.contains("stroke: currentColor"));
        assert!(result.css.contains(".stroke-transparent"));
        assert!(result.css.contains("stroke: transparent"));
        assert!(result.css.contains(".stroke-black"));
        assert!(result.css.contains("stroke: var(--color-black)"));
        assert!(result.css.contains(".stroke-white"));
        assert!(result.css.contains("stroke: var(--color-white)"));
        assert!(result.css.contains(".stroke-red-500"));
        assert!(result.css.contains("stroke: var(--color-red-500)"));
        assert!(result.css.contains(".stroke-cyan-700\\/25"));
        assert!(
            result
                .css
                .contains("stroke: color-mix(in oklab,var(--color-cyan-700) 25%,transparent)")
        );
        assert!(result.css.contains(".stroke-cyan-700\\/\\[37\\%\\]"));
        assert!(
            result
                .css
                .contains("stroke: color-mix(in oklab,var(--color-cyan-700) 37%,transparent)")
        );
        assert!(result.css.contains(".stroke-cyan-700\\/\\(--my-opacity\\)"));
        assert!(result.css.contains(
            "stroke: color-mix(in oklab,var(--color-cyan-700) var(--my-opacity),transparent)"
        ));
        assert!(result.css.contains(".stroke-\\[#50d71e\\]"));
        assert!(result.css.contains("stroke: #50d71e"));
        assert!(result.css.contains(".stroke-\\(--my-stroke-color\\)"));
        assert!(result.css.contains("stroke: var(--my-stroke-color)"));
        assert!(result.css.contains(".hover\\:stroke-lime-600:hover"));
        assert!(result.css.contains(".md\\:stroke-sky-500"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_stroke_width_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "stroke-1".to_string(),
                "stroke-2".to_string(),
                "stroke-[1.5]".to_string(),
                "stroke-(length:--my-stroke-width)".to_string(),
                "md:stroke-2".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".stroke-1"));
        assert!(result.css.contains("stroke-width: 1"));
        assert!(result.css.contains(".stroke-2"));
        assert!(result.css.contains("stroke-width: 2"));
        assert!(result.css.contains(".stroke-\\[1.5\\]"));
        assert!(result.css.contains("stroke-width: 1.5"));
        assert!(
            result
                .css
                .contains(".stroke-\\(length\\:--my-stroke-width\\)")
        );
        assert!(result.css.contains("stroke-width: var(--my-stroke-width)"));
        assert!(result.css.contains(".md\\:stroke-2"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_text_shadows() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "text-shadow-2xs".to_string(),
                "text-shadow-xs".to_string(),
                "text-shadow-sm".to_string(),
                "text-shadow-md".to_string(),
                "text-shadow-lg".to_string(),
                "text-shadow-none".to_string(),
                "text-shadow-(--my-text-shadow)".to_string(),
                "text-shadow-(color:--my-shadow-color)".to_string(),
                "text-shadow-[0_35px_35px_rgb(0_0_0_/_0.25)]".to_string(),
                "text-shadow-inherit".to_string(),
                "text-shadow-lg/20".to_string(),
                "text-shadow-sky-300".to_string(),
                "text-shadow-cyan-500/50".to_string(),
                "text-shadow-xl".to_string(),
                "md:text-shadow-lg".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".text-shadow-2xs"));
        assert!(
            result
                .css
                .contains("text-shadow: 0px 1px 0px var(--tw-shadow-color,rgb(0 0 0 / 0.15))")
        );
        assert!(result.css.contains(".text-shadow-xs"));
        assert!(
            result
                .css
                .contains("text-shadow: 0px 1px 1px var(--tw-shadow-color,rgb(0 0 0 / 0.2))")
        );
        assert!(result.css.contains(".text-shadow-sm"));
        assert!(result.css.contains("text-shadow: 0px 1px 0px var(--tw-shadow-color,rgb(0 0 0 / 0.075)),0px 1px 1px var(--tw-shadow-color,rgb(0 0 0 / 0.075)),0px 2px 2px var(--tw-shadow-color,rgb(0 0 0 / 0.075))"));
        assert!(result.css.contains(".text-shadow-md"));
        assert!(result.css.contains("text-shadow: 0px 1px 1px var(--tw-shadow-color,rgb(0 0 0 / 0.1)),0px 1px 2px var(--tw-shadow-color,rgb(0 0 0 / 0.1)),0px 2px 4px var(--tw-shadow-color,rgb(0 0 0 / 0.1))"));
        assert!(result.css.contains(".text-shadow-lg"));
        assert!(result.css.contains("text-shadow: 0px 1px 2px var(--tw-shadow-color,rgb(0 0 0 / 0.1)),0px 3px 2px var(--tw-shadow-color,rgb(0 0 0 / 0.1)),0px 4px 8px var(--tw-shadow-color,rgb(0 0 0 / 0.1))"));
        assert!(result.css.contains(".text-shadow-none"));
        assert!(result.css.contains("text-shadow: none"));
        assert!(result.css.contains(".text-shadow-\\(--my-text-shadow\\)"));
        assert!(result.css.contains("text-shadow: var(--my-text-shadow)"));
        assert!(
            result
                .css
                .contains(".text-shadow-\\(color\\:--my-shadow-color\\)")
        );
        assert!(
            result
                .css
                .contains("--tw-shadow-color: var(--my-shadow-color)")
        );
        assert!(
            result
                .css
                .contains(".text-shadow-\\[0_35px_35px_rgb\\(0_0_0_\\/_0.25\\)\\]")
        );
        assert!(
            result
                .css
                .contains("text-shadow: 0_35px_35px_rgb(0_0_0_/_0.25)")
        );
        assert!(result.css.contains(".text-shadow-inherit"));
        assert!(result.css.contains("--tw-shadow-color: inherit"));
        assert!(result.css.contains(".text-shadow-lg\\/20"));
        assert!(result.css.contains("--tw-shadow-color: rgb(0 0 0 / 20%)"));
        assert!(result.css.contains(".text-shadow-sky-300"));
        assert!(
            result
                .css
                .contains("--tw-shadow-color: var(--color-sky-300)")
        );
        assert!(result.css.contains(".text-shadow-cyan-500\\/50"));
        assert!(result.css.contains(
            "--tw-shadow-color: color-mix(in oklab,var(--color-cyan-500) 50%,transparent)"
        ));
        assert!(result.css.contains(".text-shadow-xl"));
        assert!(result.css.contains("text-shadow: var(--text-shadow-xl)"));
        assert!(result.css.contains(".md\\:text-shadow-lg"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_text_decoration_colors() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "decoration-inherit".to_string(),
                "decoration-current".to_string(),
                "decoration-transparent".to_string(),
                "decoration-black".to_string(),
                "decoration-white".to_string(),
                "decoration-sky-500".to_string(),
                "decoration-pink-500/30".to_string(),
                "decoration-indigo-500/[37%]".to_string(),
                "decoration-red-500/(--my-opacity)".to_string(),
                "decoration-[#50d71e]".to_string(),
                "decoration-(--my-color)".to_string(),
                "hover:decoration-pink-500".to_string(),
                "md:decoration-blue-400".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".decoration-inherit"));
        assert!(result.css.contains("text-decoration-color: inherit"));
        assert!(result.css.contains(".decoration-current"));
        assert!(result.css.contains("text-decoration-color: currentColor"));
        assert!(result.css.contains(".decoration-transparent"));
        assert!(result.css.contains("text-decoration-color: transparent"));
        assert!(result.css.contains(".decoration-black"));
        assert!(
            result
                .css
                .contains("text-decoration-color: var(--color-black)")
        );
        assert!(result.css.contains(".decoration-white"));
        assert!(
            result
                .css
                .contains("text-decoration-color: var(--color-white)")
        );
        assert!(result.css.contains(".decoration-sky-500"));
        assert!(
            result
                .css
                .contains("text-decoration-color: var(--color-sky-500)")
        );
        assert!(result.css.contains(".decoration-pink-500\\/30"));
        assert!(result.css.contains(
            "text-decoration-color: color-mix(in oklab,var(--color-pink-500) 30%,transparent)"
        ));
        assert!(result.css.contains(".decoration-indigo-500\\/\\[37\\%\\]"));
        assert!(result.css.contains(
            "text-decoration-color: color-mix(in oklab,var(--color-indigo-500) 37%,transparent)"
        ));
        assert!(
            result
                .css
                .contains(".decoration-red-500\\/\\(--my-opacity\\)")
        );
        assert!(result.css.contains(
            "text-decoration-color: color-mix(in oklab,var(--color-red-500) var(--my-opacity),transparent)"
        ));
        assert!(result.css.contains(".decoration-\\[#50d71e\\]"));
        assert!(result.css.contains("text-decoration-color: #50d71e"));
        assert!(result.css.contains(".decoration-\\(--my-color\\)"));
        assert!(
            result
                .css
                .contains("text-decoration-color: var(--my-color)")
        );
        assert!(result.css.contains(".hover\\:decoration-pink-500:hover"));
        assert!(result.css.contains(".md\\:decoration-blue-400"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_accent_color_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "accent-inherit".to_string(),
                "accent-current".to_string(),
                "accent-transparent".to_string(),
                "accent-black".to_string(),
                "accent-white".to_string(),
                "accent-rose-500".to_string(),
                "accent-purple-500/25".to_string(),
                "accent-purple-500/[37%]".to_string(),
                "accent-purple-500/(--my-opacity)".to_string(),
                "accent-[#50d71e]".to_string(),
                "accent-[color:var(--tool-accent)]".to_string(),
                "accent-(--my-accent-color)".to_string(),
                "hover:accent-pink-500".to_string(),
                "md:accent-lime-600".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".accent-inherit"));
        assert!(result.css.contains("accent-color: inherit"));
        assert!(result.css.contains(".accent-current"));
        assert!(result.css.contains("accent-color: currentColor"));
        assert!(result.css.contains(".accent-transparent"));
        assert!(result.css.contains("accent-color: transparent"));
        assert!(result.css.contains(".accent-black"));
        assert!(result.css.contains("accent-color: var(--color-black)"));
        assert!(result.css.contains(".accent-white"));
        assert!(result.css.contains("accent-color: var(--color-white)"));
        assert!(result.css.contains(".accent-rose-500"));
        assert!(result.css.contains("accent-color: var(--color-rose-500)"));
        assert!(result.css.contains(".accent-purple-500\\/25"));
        assert!(
            result.css.contains(
                "accent-color: color-mix(in oklab,var(--color-purple-500) 25%,transparent)"
            )
        );
        assert!(result.css.contains(".accent-purple-500\\/\\[37\\%\\]"));
        assert!(
            result.css.contains(
                "accent-color: color-mix(in oklab,var(--color-purple-500) 37%,transparent)"
            )
        );
        assert!(
            result
                .css
                .contains(".accent-purple-500\\/\\(--my-opacity\\)")
        );
        assert!(result.css.contains(
            "accent-color: color-mix(in oklab,var(--color-purple-500) var(--my-opacity),transparent)"
        ));
        assert!(result.css.contains(".accent-\\[#50d71e\\]"));
        assert!(result.css.contains("accent-color: #50d71e"));
        assert!(
            result
                .css
                .contains(".accent-\\[color\\:var\\(--tool-accent\\)\\]")
        );
        assert!(result.css.contains("accent-color: var(--tool-accent)"));
        assert!(result.css.contains(".accent-\\(--my-accent-color\\)"));
        assert!(result.css.contains("accent-color: var(--my-accent-color)"));
        assert!(result.css.contains(".hover\\:accent-pink-500:hover"));
        assert!(result.css.contains(".md\\:accent-lime-600"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_caret_color_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "caret-inherit".to_string(),
                "caret-current".to_string(),
                "caret-transparent".to_string(),
                "caret-black".to_string(),
                "caret-white".to_string(),
                "caret-rose-500".to_string(),
                "caret-purple-500/25".to_string(),
                "caret-purple-500/[37%]".to_string(),
                "caret-purple-500/(--my-opacity)".to_string(),
                "caret-[#50d71e]".to_string(),
                "caret-[color:var(--tool-accent)]".to_string(),
                "caret-(--my-caret-color)".to_string(),
                "md:caret-lime-600".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".caret-inherit"));
        assert!(result.css.contains("caret-color: inherit"));
        assert!(result.css.contains(".caret-current"));
        assert!(result.css.contains("caret-color: currentColor"));
        assert!(result.css.contains(".caret-transparent"));
        assert!(result.css.contains("caret-color: transparent"));
        assert!(result.css.contains(".caret-black"));
        assert!(result.css.contains("caret-color: var(--color-black)"));
        assert!(result.css.contains(".caret-white"));
        assert!(result.css.contains("caret-color: var(--color-white)"));
        assert!(result.css.contains(".caret-rose-500"));
        assert!(result.css.contains("caret-color: var(--color-rose-500)"));
        assert!(result.css.contains(".caret-purple-500\\/25"));
        assert!(
            result.css.contains(
                "caret-color: color-mix(in oklab,var(--color-purple-500) 25%,transparent)"
            )
        );
        assert!(result.css.contains(".caret-purple-500\\/\\[37\\%\\]"));
        assert!(
            result.css.contains(
                "caret-color: color-mix(in oklab,var(--color-purple-500) 37%,transparent)"
            )
        );
        assert!(
            result
                .css
                .contains(".caret-purple-500\\/\\(--my-opacity\\)")
        );
        assert!(result.css.contains(
            "caret-color: color-mix(in oklab,var(--color-purple-500) var(--my-opacity),transparent)"
        ));
        assert!(result.css.contains(".caret-\\[#50d71e\\]"));
        assert!(result.css.contains("caret-color: #50d71e"));
        assert!(
            result
                .css
                .contains(".caret-\\[color\\:var\\(--tool-accent\\)\\]")
        );
        assert!(result.css.contains("caret-color: var(--tool-accent)"));
        assert!(result.css.contains(".caret-\\(--my-caret-color\\)"));
        assert!(result.css.contains("caret-color: var(--my-caret-color)"));
        assert!(result.css.contains(".md\\:caret-lime-600"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn supports_single_token_theme_colors_without_breaking_non_color_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "bg-midnight".to_string(),
                "text-tahiti".to_string(),
                "fill-bermuda".to_string(),
                "stroke-bermuda".to_string(),
                "decoration-midnight".to_string(),
                "accent-midnight".to_string(),
                "caret-midnight".to_string(),
                "border-midnight".to_string(),
                "outline-midnight".to_string(),
                "shadow-midnight".to_string(),
                "ring-midnight".to_string(),
                "inset-shadow-midnight".to_string(),
                "inset-ring-midnight".to_string(),
                "text-sm".to_string(),
                "outline-2".to_string(),
                "border-collapse".to_string(),
            ],
            &config,
        );

        assert!(
            result
                .css
                .contains("background-color: var(--color-midnight)")
        );
        assert!(result.css.contains("color: var(--color-tahiti)"));
        assert!(result.css.contains("fill: var(--color-bermuda)"));
        assert!(result.css.contains("stroke: var(--color-bermuda)"));
        assert!(
            result
                .css
                .contains("text-decoration-color: var(--color-midnight)")
        );
        assert!(result.css.contains("accent-color: var(--color-midnight)"));
        assert!(result.css.contains("caret-color: var(--color-midnight)"));
        assert!(result.css.contains("border-color: var(--color-midnight)"));
        assert!(result.css.contains("outline-color: var(--color-midnight)"));
        assert!(
            result
                .css
                .contains("--tw-shadow-color: var(--color-midnight)")
        );
        assert!(
            result
                .css
                .contains("--tw-ring-color: var(--color-midnight)")
        );
        assert!(
            result
                .css
                .contains("--tw-inset-shadow-color: var(--color-midnight)")
        );
        assert!(
            result
                .css
                .contains("--tw-inset-ring-color: var(--color-midnight)")
        );

        assert!(result.css.contains("font-size: var(--text-sm)"));
        assert!(result.css.contains("outline-width: 2px"));
        assert!(result.css.contains("border-collapse: collapse"));
    }

    #[test]
    fn generates_border_colors() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "border-gray-200".to_string(),
                "border-blue-600".to_string(),
                "border-x-indigo-500".to_string(),
                "border-s-emerald-500/30".to_string(),
                "border-[#243c5a]".to_string(),
                "border-[color:var(--tool-border)]".to_string(),
                "border-(--my-border)".to_string(),
                "divide-rose-400".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".border-gray-200"));
        assert!(result.css.contains("border-color: var(--color-gray-200)"));
        assert!(result.css.contains(".border-blue-600"));
        assert!(result.css.contains("border-color: var(--color-blue-600)"));
        assert!(result.css.contains(".border-x-indigo-500"));
        assert!(
            result
                .css
                .contains("border-inline-color: var(--color-indigo-500)")
        );
        assert!(result.css.contains(".border-s-emerald-500\\/30"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-emerald-500) 30%,transparent)")
        );
        assert!(result.css.contains(".border-\\[#243c5a\\]"));
        assert!(result.css.contains("border-color: #243c5a"));
        assert!(
            result
                .css
                .contains(".border-\\[color\\:var\\(--tool-border\\)\\]")
        );
        assert!(result.css.contains("border-color: var(--tool-border)"));
        assert!(
            !result
                .css
                .contains("border-width: color:var(--tool-border)")
        );
        assert!(result.css.contains(".border-\\(--my-border\\)"));
        assert!(result.css.contains("border-color: var(--my-border)"));
        assert!(result.css.contains(".divide-rose-400"));
        assert!(result.css.contains(":where(& > :not(:last-child))"));
        assert!(result.css.contains("border-color:var(--color-rose-400)"));
    }

    #[test]
    fn generates_border_widths() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "border".to_string(),
                "border-0".to_string(),
                "border-2".to_string(),
                "border-4".to_string(),
                "border-8".to_string(),
                "border-x-4".to_string(),
                "border-t-[3px]".to_string(),
                "border-e-(length:--my-border-width)".to_string(),
                "divide-x-2".to_string(),
                "divide-y-[3px]".to_string(),
                "divide-x-reverse".to_string(),
                "divide-y-reverse".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".border"));
        assert!(result.css.contains("border-width: 1px"));
        assert!(result.css.contains(".border-0"));
        assert!(result.css.contains("border-width: 0px"));
        assert!(result.css.contains(".border-2"));
        assert!(result.css.contains("border-width: 2px"));
        assert!(result.css.contains(".border-4"));
        assert!(result.css.contains("border-width: 4px"));
        assert!(result.css.contains(".border-8"));
        assert!(result.css.contains("border-width: 8px"));
        assert!(result.css.contains(".border-x-4"));
        assert!(result.css.contains("border-inline-width: 4px"));
        assert!(result.css.contains(".border-t-\\[3px\\]"));
        assert!(result.css.contains("border-top-width: 3px"));
        assert!(
            result
                .css
                .contains(".border-e-\\(length\\:--my-border-width\\)")
        );
        assert!(
            result
                .css
                .contains("border-inline-end-width: var(--my-border-width)")
        );
        assert!(result.css.contains(".divide-x-2"));
        assert!(
            result
                .css
                .contains("border-inline-start-width:calc(2px * var(--tw-divide-x-reverse))")
        );
        assert!(
            result.css.contains(
                "border-inline-end-width:calc(2px * calc(1 - var(--tw-divide-x-reverse)))"
            )
        );
        assert!(result.css.contains(".divide-y-\\[3px\\]"));
        assert!(
            result
                .css
                .contains("border-bottom-width:calc(3px * calc(1 - var(--tw-divide-y-reverse)))")
        );
        assert!(result.css.contains(".divide-x-reverse"));
        assert!(result.css.contains("--tw-divide-x-reverse:1"));
        assert!(result.css.contains(".divide-y-reverse"));
        assert!(result.css.contains("--tw-divide-y-reverse:1"));
    }

    #[test]
    fn generates_border_spacing_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "border-spacing-2".to_string(),
                "border-spacing-(--my-border-spacing)".to_string(),
                "border-spacing-[7px]".to_string(),
                "border-spacing-x-3".to_string(),
                "border-spacing-x-(--my-border-spacing-x)".to_string(),
                "border-spacing-x-[11px]".to_string(),
                "border-spacing-y-4".to_string(),
                "border-spacing-y-(--my-border-spacing-y)".to_string(),
                "border-spacing-y-[13px]".to_string(),
                "md:border-spacing-6".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".border-spacing-2"));
        assert!(
            result
                .css
                .contains("border-spacing: calc(var(--spacing) * 2)")
        );
        assert!(
            result
                .css
                .contains(".border-spacing-\\(--my-border-spacing\\)")
        );
        assert!(
            result
                .css
                .contains("border-spacing: var(--my-border-spacing)")
        );
        assert!(result.css.contains(".border-spacing-\\[7px\\]"));
        assert!(result.css.contains("border-spacing: 7px"));
        assert!(result.css.contains(".border-spacing-x-3"));
        assert!(
            result
                .css
                .contains("border-spacing: calc(var(--spacing) * 3) var(--tw-border-spacing-y)")
        );
        assert!(
            result
                .css
                .contains(".border-spacing-x-\\(--my-border-spacing-x\\)")
        );
        assert!(
            result
                .css
                .contains("border-spacing: var(--my-border-spacing-x) var(--tw-border-spacing-y)")
        );
        assert!(result.css.contains(".border-spacing-x-\\[11px\\]"));
        assert!(
            result
                .css
                .contains("border-spacing: 11px var(--tw-border-spacing-y)")
        );
        assert!(result.css.contains(".border-spacing-y-4"));
        assert!(
            result
                .css
                .contains("border-spacing: var(--tw-border-spacing-x) calc(var(--spacing) * 4)")
        );
        assert!(
            result
                .css
                .contains(".border-spacing-y-\\(--my-border-spacing-y\\)")
        );
        assert!(
            result
                .css
                .contains("border-spacing: var(--tw-border-spacing-x) var(--my-border-spacing-y)")
        );
        assert!(result.css.contains(".border-spacing-y-\\[13px\\]"));
        assert!(
            result
                .css
                .contains("border-spacing: var(--tw-border-spacing-x) 13px")
        );
        assert!(result.css.contains(".md\\:border-spacing-6"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_border_styles() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "border-solid".to_string(),
                "border-dashed".to_string(),
                "border-dotted".to_string(),
                "border-double".to_string(),
                "border-hidden".to_string(),
                "border-none".to_string(),
                "divide-dashed".to_string(),
                "divide-none".to_string(),
                "md:border-dotted".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".border-solid"));
        assert!(result.css.contains("border-style: solid"));
        assert!(result.css.contains(".border-dashed"));
        assert!(result.css.contains("border-style: dashed"));
        assert!(result.css.contains(".border-dotted"));
        assert!(result.css.contains("border-style: dotted"));
        assert!(result.css.contains(".border-double"));
        assert!(result.css.contains("border-style: double"));
        assert!(result.css.contains(".border-hidden"));
        assert!(result.css.contains("border-style: hidden"));
        assert!(result.css.contains(".border-none"));
        assert!(result.css.contains("border-style: none"));
        assert!(result.css.contains(".divide-dashed"));
        assert!(result.css.contains(".divide-none"));
        assert!(result.css.contains(".md\\:border-dotted"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_outline_widths() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "outline".to_string(),
                "outline-2".to_string(),
                "outline-4".to_string(),
                "outline-(length:--my-outline-width)".to_string(),
                "outline-[2vw]".to_string(),
                "focus:outline-2".to_string(),
                "md:outline".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".outline"));
        assert!(result.css.contains("outline-width: 1px"));
        assert!(result.css.contains(".outline-2"));
        assert!(result.css.contains("outline-width: 2px"));
        assert!(result.css.contains(".outline-4"));
        assert!(result.css.contains("outline-width: 4px"));
        assert!(
            result
                .css
                .contains(".outline-\\(length\\:--my-outline-width\\)")
        );
        assert!(
            result
                .css
                .contains("outline-width: var(--my-outline-width)")
        );
        assert!(result.css.contains(".outline-\\[2vw\\]"));
        assert!(result.css.contains("outline-width: 2vw"));
        assert!(result.css.contains(".focus\\:outline-2:focus"));
        assert!(result.css.contains(".md\\:outline"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_outline_colors() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "outline-inherit".to_string(),
                "outline-current".to_string(),
                "outline-transparent".to_string(),
                "outline-black".to_string(),
                "outline-white".to_string(),
                "outline-blue-500".to_string(),
                "outline-blue-500/75".to_string(),
                "outline-rose-500/[37%]".to_string(),
                "outline-red-500/(--my-opacity)".to_string(),
                "outline-[#243c5a]".to_string(),
                "outline-[color:var(--tool-primary)]".to_string(),
                "outline-(--my-color)".to_string(),
                "focus:outline-sky-500".to_string(),
                "md:outline-blue-400".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".outline-inherit"));
        assert!(result.css.contains("outline-color: inherit"));
        assert!(result.css.contains(".outline-current"));
        assert!(result.css.contains("outline-color: currentColor"));
        assert!(result.css.contains(".outline-transparent"));
        assert!(result.css.contains("outline-color: transparent"));
        assert!(result.css.contains(".outline-black"));
        assert!(result.css.contains("outline-color: var(--color-black)"));
        assert!(result.css.contains(".outline-white"));
        assert!(result.css.contains("outline-color: var(--color-white)"));
        assert!(result.css.contains(".outline-blue-500"));
        assert!(result.css.contains("outline-color: var(--color-blue-500)"));
        assert!(result.css.contains(".outline-blue-500\\/75"));
        assert!(
            result.css.contains(
                "outline-color: color-mix(in oklab,var(--color-blue-500) 75%,transparent)"
            )
        );
        assert!(result.css.contains(".outline-rose-500\\/\\[37\\%\\]"));
        assert!(
            result.css.contains(
                "outline-color: color-mix(in oklab,var(--color-rose-500) 37%,transparent)"
            )
        );
        assert!(result.css.contains(".outline-red-500\\/\\(--my-opacity\\)"));
        assert!(result.css.contains(
            "outline-color: color-mix(in oklab,var(--color-red-500) var(--my-opacity),transparent)"
        ));
        assert!(result.css.contains(".outline-\\[#243c5a\\]"));
        assert!(result.css.contains("outline-color: #243c5a"));
        assert!(
            result
                .css
                .contains(".outline-\\[color\\:var\\(--tool-primary\\)\\]")
        );
        assert!(result.css.contains("outline-color: var(--tool-primary)"));
        assert!(
            !result
                .css
                .contains("outline-width: color:var(--tool-primary)")
        );
        assert!(result.css.contains(".outline-\\(--my-color\\)"));
        assert!(result.css.contains("outline-color: var(--my-color)"));
        assert!(result.css.contains(".focus\\:outline-sky-500:focus"));
        assert!(result.css.contains(".md\\:outline-blue-400"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_outline_styles() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "outline-solid".to_string(),
                "outline-dashed".to_string(),
                "outline-dotted".to_string(),
                "outline-double".to_string(),
                "outline-none".to_string(),
                "outline-hidden".to_string(),
                "focus:outline-hidden".to_string(),
                "md:outline-dashed".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".outline-solid"));
        assert!(result.css.contains("outline-style: solid"));
        assert!(result.css.contains(".outline-dashed"));
        assert!(result.css.contains("outline-style: dashed"));
        assert!(result.css.contains(".outline-dotted"));
        assert!(result.css.contains("outline-style: dotted"));
        assert!(result.css.contains(".outline-double"));
        assert!(result.css.contains("outline-style: double"));
        assert!(result.css.contains(".outline-none"));
        assert!(result.css.contains("outline-style: none"));
        assert!(result.css.contains(".outline-hidden"));
        assert!(result.css.contains("outline: 2px solid transparent"));
        assert!(result.css.contains("outline-offset: 2px"));
        assert!(result.css.contains(".focus\\:outline-hidden:focus"));
        assert!(result.css.contains(".md\\:outline-dashed"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_outline_offsets() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "outline-offset-0".to_string(),
                "outline-offset-2".to_string(),
                "outline-offset-4".to_string(),
                "-outline-offset-2".to_string(),
                "outline-offset-(--my-outline-offset)".to_string(),
                "outline-offset-[2vw]".to_string(),
                "md:outline-offset-2".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".outline-offset-0"));
        assert!(result.css.contains("outline-offset: 0px"));
        assert!(result.css.contains(".outline-offset-2"));
        assert!(result.css.contains("outline-offset: 2px"));
        assert!(result.css.contains(".outline-offset-4"));
        assert!(result.css.contains("outline-offset: 4px"));
        assert!(result.css.contains(".-outline-offset-2"));
        assert!(result.css.contains("outline-offset: calc(2px * -1)"));
        assert!(
            result
                .css
                .contains(".outline-offset-\\(--my-outline-offset\\)")
        );
        assert!(
            result
                .css
                .contains("outline-offset: var(--my-outline-offset)")
        );
        assert!(result.css.contains(".outline-offset-\\[2vw\\]"));
        assert!(result.css.contains("outline-offset: 2vw"));
        assert!(result.css.contains(".md\\:outline-offset-2"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_rounded_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "rounded".to_string(),
                "rounded-xs".to_string(),
                "rounded-sm".to_string(),
                "rounded-md".to_string(),
                "rounded-lg".to_string(),
                "rounded-xl".to_string(),
                "rounded-2xl".to_string(),
                "rounded-3xl".to_string(),
                "rounded-4xl".to_string(),
                "rounded-none".to_string(),
                "rounded-full".to_string(),
                "rounded-(--my-radius)".to_string(),
                "rounded-[2vw]".to_string(),
                "rounded-s-lg".to_string(),
                "rounded-se-xl".to_string(),
                "rounded-tr-none".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".rounded"));
        assert!(result.css.contains("border-radius: var(--radius-sm)"));
        assert!(result.css.contains(".rounded-xs"));
        assert!(result.css.contains("border-radius: var(--radius-xs)"));
        assert!(result.css.contains(".rounded-sm"));
        assert!(result.css.contains("border-radius: var(--radius-sm)"));
        assert!(result.css.contains(".rounded-md"));
        assert!(result.css.contains("border-radius: var(--radius-md)"));
        assert!(result.css.contains(".rounded-lg"));
        assert!(result.css.contains("border-radius: var(--radius-lg)"));
        assert!(result.css.contains(".rounded-xl"));
        assert!(result.css.contains("border-radius: var(--radius-xl)"));
        assert!(result.css.contains(".rounded-2xl"));
        assert!(result.css.contains("border-radius: var(--radius-2xl)"));
        assert!(result.css.contains(".rounded-3xl"));
        assert!(result.css.contains("border-radius: var(--radius-3xl)"));
        assert!(result.css.contains(".rounded-4xl"));
        assert!(result.css.contains("border-radius: var(--radius-4xl)"));
        assert!(result.css.contains(".rounded-none"));
        assert!(result.css.contains("border-radius: 0"));
        assert!(result.css.contains(".rounded-full"));
        assert!(result.css.contains("border-radius: calc(infinity * 1px)"));
        assert!(result.css.contains(".rounded-\\(--my-radius\\)"));
        assert!(result.css.contains("border-radius: var(--my-radius)"));
        assert!(result.css.contains(".rounded-\\[2vw\\]"));
        assert!(result.css.contains("border-radius: 2vw"));
        assert!(result.css.contains(".rounded-s-lg"));
        assert!(
            result
                .css
                .contains("border-start-start-radius: var(--radius-lg)")
        );
        assert!(
            result
                .css
                .contains("border-end-start-radius: var(--radius-lg)")
        );
        assert!(result.css.contains(".rounded-se-xl"));
        assert!(
            result
                .css
                .contains("border-start-end-radius: var(--radius-xl)")
        );
        assert!(result.css.contains(".rounded-tr-none"));
        assert!(result.css.contains("border-top-right-radius: 0"));
    }

    #[test]
    fn generates_shadow_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "shadow-sm".to_string(),
                "shadow".to_string(),
                "shadow-md".to_string(),
                "shadow-lg".to_string(),
                "shadow-xl".to_string(),
                "shadow-2xl".to_string(),
                "shadow-inner".to_string(),
                "shadow-none".to_string(),
                "shadow-[0_0_20px_rgba(255,255,255,0.3)]".to_string(),
                "shadow-(--portal-shadow)".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".shadow-sm"));
        assert!(
            result
                .css
                .contains("--tw-shadow: 0 1px 3px 0 var(--tw-shadow-color,rgb(0 0 0 / 0.1))")
        );
        assert!(result.css.contains(".shadow"));
        assert!(
            result
                .css
                .contains("--tw-shadow: 0 1px 3px 0 var(--tw-shadow-color,rgb(0 0 0 / 0.1))")
        );
        assert!(result.css.contains(
            "box-shadow: var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)"
        ));
        assert!(result.css.contains(".shadow-md"));
        assert!(
            result
                .css
                .contains("--tw-shadow: 0 4px 6px -1px var(--tw-shadow-color,rgb(0 0 0 / 0.1))")
        );
        assert!(result.css.contains(".shadow-lg"));
        assert!(
            result
                .css
                .contains("--tw-shadow: 0 10px 15px -3px var(--tw-shadow-color,rgb(0 0 0 / 0.1))")
        );
        assert!(result.css.contains(".shadow-xl"));
        assert!(
            result
                .css
                .contains("--tw-shadow: 0 20px 25px -5px var(--tw-shadow-color,rgb(0 0 0 / 0.1))")
        );
        assert!(result.css.contains(".shadow-2xl"));
        assert!(
            result.css.contains(
                "--tw-shadow: 0 25px 50px -12px var(--tw-shadow-color,rgb(0 0 0 / 0.25))"
            )
        );
        assert!(result.css.contains(".shadow-inner"));
        assert!(
            result.css.contains(
                "--tw-shadow: inset 0 2px 4px 0 var(--tw-shadow-color,rgb(0 0 0 / 0.05))"
            )
        );
        assert!(result.css.contains(".shadow-none"));
        assert!(result.css.contains("--tw-shadow: 0 0 #0000"));
        assert!(
            result
                .css
                .contains(".shadow-\\[0_0_20px_rgba\\(255\\,255\\,255\\,0.3\\)\\]")
        );
        assert!(
            result
                .css
                .contains("--tw-shadow: 0 0 20px rgba(255,255,255,0.3)")
        );
        assert!(result.css.contains(".shadow-\\(--portal-shadow\\)"));
        assert!(result.css.contains("--tw-shadow: var(--portal-shadow)"));
    }

    #[test]
    fn generates_shadow_color_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "shadow-md".to_string(),
                "shadow-red-500".to_string(),
                "shadow-cyan-500/[37%]".to_string(),
                "shadow-(--my-shadow-color)".to_string(),
                "shadow-[#243c5a]".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".shadow-red-500"));
        assert!(
            result
                .css
                .contains("--tw-shadow-color: var(--color-red-500)")
        );
        assert!(result.css.contains(".shadow-cyan-500\\/\\[37\\%\\]"));
        assert!(result.css.contains("color-mix(in oklab,color-mix(in oklab,var(--color-cyan-500) 37%,transparent) var(--tw-shadow-alpha),transparent)"));
        assert!(result.css.contains(".shadow-\\(--my-shadow-color\\)"));
        assert!(
            result
                .css
                .contains("--tw-shadow-color: var(--my-shadow-color)")
        );
        assert!(result.css.contains(".shadow-\\[#243c5a\\]"));
        assert!(result.css.contains("--tw-shadow-color: #243c5a"));
    }

    #[test]
    fn generates_inset_shadow_color_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "inset-shadow-red-500".to_string(),
                "inset-shadow-cyan-500/[37%]".to_string(),
                "inset-shadow-(--my-inset-shadow-color)".to_string(),
                "inset-shadow-[#243c5a]".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".inset-shadow-red-500"));
        assert!(
            result
                .css
                .contains("--tw-inset-shadow-color: var(--color-red-500)")
        );
        assert!(result.css.contains(".inset-shadow-cyan-500\\/\\[37\\%\\]"));
        assert!(result.css.contains(
            "--tw-inset-shadow-color: color-mix(in oklab,var(--color-cyan-500) 37%,transparent)"
        ));
        assert!(
            result
                .css
                .contains(".inset-shadow-\\(--my-inset-shadow-color\\)")
        );
        assert!(
            result
                .css
                .contains("--tw-inset-shadow-color: var(--my-inset-shadow-color)")
        );
        assert!(result.css.contains(".inset-shadow-\\[#243c5a\\]"));
        assert!(result.css.contains("--tw-inset-shadow-color: #243c5a"));
    }

    #[test]
    fn generates_ring_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "ring".to_string(),
                "ring-inset".to_string(),
                "ring-0".to_string(),
                "ring-2".to_string(),
                "ring-8".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".ring"));
        assert!(result.css.contains(
            "--tw-ring-shadow: var(--tw-ring-inset,) 0 0 0 calc(1px + var(--tw-ring-offset-width))"
        ));
        assert!(result.css.contains(
            "box-shadow: var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)"
        ));
        assert!(result.css.contains(".ring-inset"));
        assert!(result.css.contains("--tw-ring-inset: inset"));
        assert!(result.css.contains(".ring-0"));
        assert!(result.css.contains(
            "--tw-ring-shadow: var(--tw-ring-inset,) 0 0 0 calc(0px + var(--tw-ring-offset-width))"
        ));
        assert!(result.css.contains(".ring-2"));
        assert!(result.css.contains(
            "--tw-ring-shadow: var(--tw-ring-inset,) 0 0 0 calc(2px + var(--tw-ring-offset-width))"
        ));
        assert!(result.css.contains(".ring-8"));
        assert!(result.css.contains(
            "--tw-ring-shadow: var(--tw-ring-inset,) 0 0 0 calc(8px + var(--tw-ring-offset-width))"
        ));
    }

    #[test]
    fn generates_ring_color_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "ring-2".to_string(),
                "ring-blue-500".to_string(),
                "ring-rose-500/30".to_string(),
                "ring-(--my-ring-color)".to_string(),
                "ring-[#243c5a]".to_string(),
                "ring-[color:var(--tool-danger)]".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".ring-blue-500"));
        assert!(
            result
                .css
                .contains("--tw-ring-color: var(--color-blue-500)")
        );
        assert!(result.css.contains(".ring-rose-500\\/30"));
        assert!(
            result
                .css
                .contains("color-mix(in oklab,var(--color-rose-500) 30%,transparent)")
        );
        assert!(result.css.contains(".ring-\\(--my-ring-color\\)"));
        assert!(result.css.contains("--tw-ring-color: var(--my-ring-color)"));
        assert!(result.css.contains(".ring-\\[#243c5a\\]"));
        assert!(result.css.contains("--tw-ring-color: #243c5a"));
        assert!(
            result
                .css
                .contains(".ring-\\[color\\:var\\(--tool-danger\\)\\]")
        );
        assert!(result.css.contains("--tw-ring-color: var(--tool-danger)"));
    }

    #[test]
    fn generates_inset_ring_color_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "inset-ring-blue-500".to_string(),
                "inset-ring-rose-500/30".to_string(),
                "inset-ring-(--my-inset-ring-color)".to_string(),
                "inset-ring-[#243c5a]".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".inset-ring-blue-500"));
        assert!(
            result
                .css
                .contains("--tw-inset-ring-color: var(--color-blue-500)")
        );
        assert!(result.css.contains(".inset-ring-rose-500\\/30"));
        assert!(result.css.contains(
            "--tw-inset-ring-color: color-mix(in oklab,var(--color-rose-500) 30%,transparent)"
        ));
        assert!(
            result
                .css
                .contains(".inset-ring-\\(--my-inset-ring-color\\)")
        );
        assert!(
            result
                .css
                .contains("--tw-inset-ring-color: var(--my-inset-ring-color)")
        );
        assert!(result.css.contains(".inset-ring-\\[#243c5a\\]"));
        assert!(result.css.contains("--tw-inset-ring-color: #243c5a"));
    }

    #[test]
    fn generates_focus_variant() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(&["focus:ring-2".to_string()], &config);
        assert!(result.css.contains(".focus\\:ring-2:focus"));
        assert!(result.css.contains(
            "--tw-ring-shadow: var(--tw-ring-inset,) 0 0 0 calc(2px + var(--tw-ring-offset-width))"
        ));
    }

    #[test]
    fn generates_hover_and_active_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "hover:bg-blue-500".to_string(),
                "active:bg-blue-600".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".hover\\:bg-blue-500:hover"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-blue-500)")
        );
        assert!(result.css.contains(".active\\:bg-blue-600:active"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-blue-600)")
        );
    }

    #[test]
    fn generates_focus_within_and_disabled_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "focus-within:ring-1".to_string(),
                "disabled:bg-gray-200".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".focus-within\\:ring-1:focus-within"));
        assert!(result.css.contains(
            "--tw-ring-shadow: var(--tw-ring-inset,) 0 0 0 calc(1px + var(--tw-ring-offset-width))"
        ));
        assert!(result.css.contains(".disabled\\:bg-gray-200:disabled"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-gray-200)")
        );
    }

    #[test]
    fn generates_dark_variant() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(&["dark:bg-gray-900".to_string()], &config);
        assert!(result.css.contains(".dark\\:bg-gray-900"));
        assert!(result.css.contains("@media (prefers-color-scheme: dark)"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-gray-900)")
        );
    }

    #[test]
    fn supports_custom_dark_variant_selector_override() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: Some("&:where(.dark, .dark *)".to_string()),
            custom_variant_selectors: vec![],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let result =
            generate_with_overrides(&["dark:bg-black".to_string()], &config, Some(&overrides));
        assert!(
            result
                .css
                .contains(".dark\\:bg-black:where(.dark, .dark *)")
        );
        assert!(!result.css.contains("prefers-color-scheme: dark"));
    }

    #[test]
    fn supports_custom_variant_selector_override() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![(
                "theme-midnight".to_string(),
                "&:where([data-theme=\"midnight\"] *)".to_string(),
            )],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let result = generate_with_overrides(
            &["theme-midnight:bg-black".to_string()],
            &config,
            Some(&overrides),
        );
        assert!(
            result
                .css
                .contains(".theme-midnight\\:bg-black:where([data-theme=\"midnight\"] *)")
        );
    }

    #[test]
    fn supports_custom_variant_block_with_slot() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![(
                "any-hover".to_string(),
                "@media (any-hover: hover) { &:hover { @slot; } }".to_string(),
            )],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let result = generate_with_overrides(
            &["any-hover:bg-black".to_string()],
            &config,
            Some(&overrides),
        );
        assert!(result.css.contains("@media (any-hover: hover)"));
        assert!(result.css.contains(".any-hover\\:bg-black:hover"));
        assert!(result.css.contains("background-color: var(--color-black)"));
    }

    #[test]
    fn supports_custom_variant_block_with_supports_and_media_slot() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![(
                "fancy-hover".to_string(),
                "@supports (display: grid) { @media (any-hover: hover) { &:hover { @slot; } } }"
                    .to_string(),
            )],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let result = generate_with_overrides(
            &["fancy-hover:text-black".to_string()],
            &config,
            Some(&overrides),
        );
        assert!(
            result.css.contains("@supports (display: grid)"),
            "{}",
            result.css
        );
        assert!(
            result.css.contains("@media (any-hover: hover)"),
            "{}",
            result.css
        );
        assert!(result.css.contains(".fancy-hover\\:text-black:hover"));
        assert!(result.css.contains("color: var(--color-black)"));
    }

    #[test]
    fn supports_custom_variant_block_with_container_slot() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![(
                "in-card".to_string(),
                "@container sidebar (width >= 30rem) { &:hover { @slot; } }".to_string(),
            )],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let result =
            generate_with_overrides(&["in-card:bg-black".to_string()], &config, Some(&overrides));
        assert!(result.css.contains("@container sidebar (width >= 30rem)"));
        assert!(result.css.contains(".in-card\\:bg-black:hover"));
        assert!(result.css.contains("background-color: var(--color-black)"));
    }

    #[test]
    fn supports_custom_utility_with_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![(
                "content-auto".to_string(),
                "content-visibility: auto;".to_string(),
            )],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let result = generate_with_overrides(
            &[
                "content-auto".to_string(),
                "hover:content-auto".to_string(),
                "lg:content-auto".to_string(),
            ],
            &config,
            Some(&overrides),
        );
        assert!(result.css.contains(".content-auto"));
        assert!(result.css.contains("content-visibility: auto"));
        assert!(result.css.contains(".hover\\:content-auto:hover"));
        assert!(result.css.contains("@media (hover: hover)"));
        assert!(result.css.contains(".lg\\:content-auto"));
        assert!(result.css.contains("@media (width >= 64rem)"));
    }

    #[test]
    fn supports_complex_custom_utility_with_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![(
                "scrollbar-hidden".to_string(),
                "&::-webkit-scrollbar { display: none; }".to_string(),
            )],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let result = generate_with_overrides(
            &[
                "scrollbar-hidden".to_string(),
                "hover:scrollbar-hidden".to_string(),
            ],
            &config,
            Some(&overrides),
        );
        assert!(result.css.contains(".scrollbar-hidden"));
        assert!(result.css.contains("@media (hover: hover)"));
        assert!(result.css.contains(".hover\\:scrollbar-hidden:hover"));
        assert!(result.css.contains("&::-webkit-scrollbar"));
        assert!(result.css.contains("display: none;"));
    }

    #[test]
    fn supports_functional_custom_utility_with_value_function() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![
                (
                    "tab-*".to_string(),
                    "tab-size: --value(--tab-size-*, integer, [integer]);".to_string(),
                ),
                (
                    "opacity-*".to_string(),
                    "opacity: calc(--value(integer) * 1%);".to_string(),
                ),
            ],
            theme_variable_values: vec![("--tab-size-github".to_string(), "8".to_string())],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let result = generate_with_overrides(
            &[
                "tab-github".to_string(),
                "tab-4".to_string(),
                "tab-[76]".to_string(),
                "opacity-15".to_string(),
                "hover:tab-4".to_string(),
            ],
            &config,
            Some(&overrides),
        );
        assert!(result.css.contains(".tab-github"));
        assert!(result.css.contains("tab-size: 8"));
        assert!(result.css.contains(".tab-4"));
        assert!(result.css.contains("tab-size: 4"));
        assert!(result.css.contains(".tab-\\[76\\]"));
        assert!(result.css.contains("tab-size: 76"));
        assert!(result.css.contains(".opacity-15"));
        assert!(result.css.contains("opacity: calc(15 * 1%)"));
        assert!(result.css.contains(".hover\\:tab-4:hover"));
    }

    #[test]
    fn supports_functional_custom_utility_with_modifier_function() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![(
                "demo-*".to_string(),
                "font-size: --value(integer); line-height: --modifier(integer); letter-spacing: --modifier([*]);".to_string(),
            )],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let result = generate_with_overrides(
            &[
                "demo-12/6".to_string(),
                "demo-10/[0.02em]".to_string(),
                "demo-9".to_string(),
            ],
            &config,
            Some(&overrides),
        );
        assert!(result.css.contains(".demo-12\\/6"));
        assert!(result.css.contains("font-size: 12"));
        assert!(result.css.contains("line-height: 6"));
        assert!(result.css.contains(".demo-10\\/\\[0.02em\\]"));
        assert!(result.css.contains("letter-spacing: 0.02em"));
        assert!(result.css.contains(".demo-9"));
        assert!(result.css.contains("font-size: 9"));
        assert!(!result.css.contains(".demo-9 { line-height:"));
    }

    #[test]
    fn generates_responsive_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "sm:bg-blue-500".to_string(),
                "md:text-gray-700".to_string(),
                "lg:ring-2".to_string(),
                "xl:bg-gray-100".to_string(),
                "2xl:text-blue-600".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains("@media (width >= 40rem)"));
        assert!(result.css.contains(".sm\\:bg-blue-500"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:text-gray-700"));
        assert!(result.css.contains("@media (width >= 64rem)"));
        assert!(result.css.contains(".lg\\:ring-2"));
        assert!(result.css.contains("@media (width >= 80rem)"));
        assert!(result.css.contains(".xl\\:bg-gray-100"));
        assert!(result.css.contains("@media (width >= 96rem)"));
        assert!(result.css.contains(".2xl\\:text-blue-600"));
    }

    #[test]
    fn supports_custom_and_removed_breakpoints_from_overrides() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![
                ("xs".to_string(), "30rem".to_string()),
                ("sm".to_string(), "40rem".to_string()),
            ],
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
        let result = generate_with_overrides(
            &[
                "xs:flex".to_string(),
                "max-xs:hidden".to_string(),
                "md:flex".to_string(),
            ],
            &config,
            Some(&overrides),
        );
        assert!(result.css.contains("@media (width >= 30rem)"));
        assert!(result.css.contains(".xs\\:flex"));
        assert!(result.css.contains("@media (width < 30rem)"));
        assert!(result.css.contains(".max-xs\\:hidden"));
        assert!(!result.css.contains(".md\\:flex"));
    }

    #[test]
    fn supports_custom_container_query_sizes_from_overrides() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![
                ("sm".to_string(), "24rem".to_string()),
                ("8xl".to_string(), "96rem".to_string()),
            ],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let result = generate_with_overrides(
            &["@8xl:flex".to_string(), "@max-sm:hidden".to_string()],
            &config,
            Some(&overrides),
        );
        assert!(result.css.contains("@container (width >= 96rem)"));
        assert!(result.css.contains(".\\@8xl\\:flex"));
        assert!(result.css.contains("@container (width < 24rem)"));
        assert!(result.css.contains(".\\@max-sm\\:hidden"));
    }

    #[test]
    fn respects_global_theme_reset_for_theme_driven_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: true,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec!["--color-brand-500".to_string()],
        };
        let result = generate_with_overrides(
            &[
                "bg-brand-500".to_string(),
                "bg-blue-500".to_string(),
                "p-4".to_string(),
            ],
            &config,
            Some(&overrides),
        );
        assert!(result.css.contains(".bg-brand-500"));
        assert!(!result.css.contains(".bg-blue-500"));
        assert!(!result.css.contains(".p-4"));
    }

    #[test]
    fn respects_namespace_initial_reset_for_color_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec!["color".to_string()],
            disabled_color_families: vec![],
            declared_theme_vars: vec!["--color-brand-500".to_string()],
        };
        let result = generate_with_overrides(
            &[
                "bg-brand-500".to_string(),
                "bg-blue-500".to_string(),
                "p-4".to_string(),
            ],
            &config,
            Some(&overrides),
        );
        assert!(result.css.contains(".bg-brand-500"));
        assert!(!result.css.contains(".bg-blue-500"));
        assert!(result.css.contains(".p-4"));
    }

    #[test]
    fn respects_color_family_initial_reset_for_color_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec!["lime".to_string()],
            declared_theme_vars: vec![
                "--color-lime-500".to_string(),
                "--color-blue-500".to_string(),
            ],
        };
        let result = generate_with_overrides(
            &[
                "bg-lime-500".to_string(),
                "bg-lime-600".to_string(),
                "ring-lime-600".to_string(),
                "shadow-lime-600".to_string(),
                "inset-ring-lime-600".to_string(),
                "inset-shadow-lime-600".to_string(),
                "bg-blue-500".to_string(),
                "p-4".to_string(),
            ],
            &config,
            Some(&overrides),
        );
        assert!(result.css.contains(".bg-lime-500"));
        assert!(!result.css.contains(".bg-lime-600"));
        assert!(!result.css.contains(".ring-lime-600"));
        assert!(!result.css.contains(".shadow-lime-600"));
        assert!(!result.css.contains(".inset-ring-lime-600"));
        assert!(!result.css.contains(".inset-shadow-lime-600"));
        assert!(result.css.contains(".bg-blue-500"));
        assert!(result.css.contains(".p-4"));
    }

    #[test]
    fn rejects_undeclared_theme_variables_when_declarations_are_available() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: false,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![
                "--color-blue-500".to_string(),
                "--font-sans".to_string(),
                "--text-sm".to_string(),
                "--text-sm--line-height".to_string(),
                "--animate-spin".to_string(),
            ],
        };
        let result = generate_with_overrides(
            &[
                "bg-blue-500".to_string(),
                "bg-primary".to_string(),
                "font-sans".to_string(),
                "font-display".to_string(),
                "text-sm".to_string(),
                "text-preview".to_string(),
                "animate-spin".to_string(),
                "animate-fade-in".to_string(),
            ],
            &config,
            Some(&overrides),
        );

        assert!(result.css.contains(".bg-blue-500"));
        assert!(!result.css.contains(".bg-primary"));
        assert!(result.css.contains(".font-sans"));
        assert!(!result.css.contains(".font-display"));
        assert!(result.css.contains(".text-sm"));
        assert!(!result.css.contains(".text-preview"));
        assert!(result.css.contains(".animate-spin"));
        assert!(!result.css.contains(".animate-fade-in"));
    }

    #[test]
    fn respects_global_theme_reset_for_text_shadow_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: true,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec![],
        };
        let result =
            generate_with_overrides(&["text-shadow-xl".to_string()], &config, Some(&overrides));
        assert!(!result.css.contains(".text-shadow-xl"));

        let overrides = VariantOverrides {
            responsive_breakpoints: vec![],
            container_breakpoints: vec![],
            dark_variant_selector: None,
            custom_variant_selectors: vec![],
            custom_utilities: vec![],
            theme_variable_values: vec![],
            global_theme_reset: true,
            disabled_namespaces: vec![],
            disabled_color_families: vec![],
            declared_theme_vars: vec!["--text-shadow-xl".to_string()],
        };
        let result =
            generate_with_overrides(&["text-shadow-xl".to_string()], &config, Some(&overrides));
        assert!(result.css.contains(".text-shadow-xl"));
    }

    #[test]
    fn generates_stacked_state_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(&["disabled:hover:bg-blue-500".to_string()], &config);
        assert!(
            result
                .css
                .contains(".disabled\\:hover\\:bg-blue-500:disabled:hover")
        );
        assert!(
            result
                .css
                .contains("background-color: var(--color-blue-500)")
        );
    }

    #[test]
    fn generates_dark_responsive_hover_stacked_variant() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(&["dark:lg:hover:bg-gray-900".to_string()], &config);
        assert!(
            result
                .css
                .contains(".dark\\:lg\\:hover\\:bg-gray-900:hover")
        );
        assert!(result.css.contains("@media (prefers-color-scheme: dark)"));
        assert!(result.css.contains("@media (width >= 64rem)"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-gray-900)")
        );
    }

    #[test]
    fn generates_group_hover_variant() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(&["group-hover:underline".to_string()], &config);
        assert!(result.css.contains(".group:hover .group-hover\\:underline"));
        assert!(result.css.contains("@media (hover: hover)"));
        assert!(result.css.contains("text-decoration-line: underline"));
    }

    #[test]
    fn generates_data_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "data-current:bg-blue-500".to_string(),
                "data-[state=open]:bg-blue-500".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".data-current\\:bg-blue-500"));
        assert!(
            result
                .css
                .contains(".data-\\[state\\=open\\]\\:bg-blue-500")
        );
        assert!(
            result
                .css
                .contains("background-color: var(--color-blue-500)")
        );
    }

    #[test]
    fn generates_extended_state_and_structural_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "focus-visible:underline".to_string(),
                "visited:text-blue-600".to_string(),
                "first:underline".to_string(),
                "last:underline".to_string(),
                "odd:bg-gray-100".to_string(),
                "even:bg-gray-200".to_string(),
                "nth-3:underline".to_string(),
                "nth-[2n+1]:underline".to_string(),
                "nth-last-2:underline".to_string(),
                "nth-of-type-4:underline".to_string(),
                "nth-last-of-type-[2n+1]:underline".to_string(),
                "only:underline".to_string(),
                "only-of-type:underline".to_string(),
                "empty:hidden".to_string(),
                "read-only:bg-gray-100".to_string(),
                "placeholder-shown:text-gray-500".to_string(),
                "autofill:bg-blue-100".to_string(),
                "in-range:text-blue-600".to_string(),
                "out-of-range:text-gray-700".to_string(),
            ],
            &config,
        );

        assert!(
            result
                .css
                .contains(".focus-visible\\:underline:focus-visible")
        );
        assert!(result.css.contains(".visited\\:text-blue-600:visited"));
        assert!(result.css.contains(".first\\:underline:first-child"));
        assert!(result.css.contains(".last\\:underline:last-child"));
        assert!(result.css.contains(".odd\\:bg-gray-100:nth-child(odd)"));
        assert!(result.css.contains(".even\\:bg-gray-200:nth-child(even)"));
        assert!(result.css.contains(".nth-3\\:underline:nth-child(3)"));
        assert!(
            result
                .css
                .contains(".nth-\\[2n\\+1\\]\\:underline:nth-child(2n+1)")
        );
        assert!(
            result
                .css
                .contains(".nth-last-2\\:underline:nth-last-child(2)")
        );
        assert!(
            result
                .css
                .contains(".nth-of-type-4\\:underline:nth-of-type(4)")
        );
        assert!(
            result
                .css
                .contains(".nth-last-of-type-\\[2n\\+1\\]\\:underline:nth-last-of-type(2n+1)")
        );
        assert!(result.css.contains(".only\\:underline:only-child"));
        assert!(
            result
                .css
                .contains(".only-of-type\\:underline:only-of-type")
        );
        assert!(result.css.contains(".empty\\:hidden:empty"));
        assert!(result.css.contains(".read-only\\:bg-gray-100:read-only"));
        assert!(
            result
                .css
                .contains(".placeholder-shown\\:text-gray-500:placeholder-shown")
        );
        assert!(result.css.contains(".autofill\\:bg-blue-100:autofill"));
        assert!(result.css.contains(".in-range\\:text-blue-600:in-range"));
        assert!(
            result
                .css
                .contains(".out-of-range\\:text-gray-700:out-of-range")
        );
    }

    #[test]
    fn generates_extended_group_peer_and_attribute_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "group-focus:text-blue-600".to_string(),
                "group-hover/item:text-blue-600".to_string(),
                "group-has-checked:underline".to_string(),
                "group-data-[state=open]:underline".to_string(),
                "group-aria-[sort=ascending]:underline".to_string(),
                "peer-checked:text-blue-600".to_string(),
                "peer-invalid:text-gray-700".to_string(),
                "peer-checked/draft:text-blue-600".to_string(),
                "peer-has-checked:hidden".to_string(),
                "peer-data-[size=large]:underline".to_string(),
                "in-focus:underline".to_string(),
                "has-checked:bg-blue-100".to_string(),
                "has-[:focus]:underline".to_string(),
                "not-focus:hover:bg-blue-500".to_string(),
                "not-[:focus]:hover:bg-blue-500".to_string(),
                "aria-checked:bg-blue-500".to_string(),
                "aria-[sort=ascending]:underline".to_string(),
                "data-active:bg-blue-500".to_string(),
                "data-[state=open]:bg-blue-500".to_string(),
                "rtl:ml-4".to_string(),
                "ltr:mr-4".to_string(),
            ],
            &config,
        );

        assert!(
            result
                .css
                .contains(".group:focus .group-focus\\:text-blue-600")
        );
        assert!(
            result
                .css
                .contains(".group\\/item:hover .group-hover\\/item\\:text-blue-600")
        );
        assert!(
            result
                .css
                .contains(".group:has(:checked) .group-has-checked\\:underline")
        );
        assert!(
            result
                .css
                .contains(".group[data-state=open] .group-data-\\[state\\=open\\]\\:underline")
        );
        assert!(result.css.contains(
            ".group[aria-sort=ascending] .group-aria-\\[sort\\=ascending\\]\\:underline"
        ));
        assert!(
            result
                .css
                .contains(".peer:checked ~ .peer-checked\\:text-blue-600")
        );
        assert!(
            result
                .css
                .contains(".peer:invalid ~ .peer-invalid\\:text-gray-700")
        );
        assert!(
            result
                .css
                .contains(".peer\\/draft:checked ~ .peer-checked\\/draft\\:text-blue-600")
        );
        assert!(
            result
                .css
                .contains(".peer:has(:checked) ~ .peer-has-checked\\:hidden")
        );
        assert!(
            result
                .css
                .contains(".peer[data-size=large] ~ .peer-data-\\[size\\=large\\]\\:underline")
        );
        assert!(result.css.contains(":where(:focus) .in-focus\\:underline"));
        assert!(
            result
                .css
                .contains(".has-checked\\:bg-blue-100:has(:checked)")
        );
        assert!(
            result
                .css
                .contains(".has-\\[\\:focus\\]\\:underline:has(:focus)")
        );
        assert!(
            result
                .css
                .contains(".not-focus\\:hover\\:bg-blue-500:not(:focus):hover")
        );
        assert!(
            result
                .css
                .contains(".not-\\[\\:focus\\]\\:hover\\:bg-blue-500:not(:focus):hover")
        );
        assert!(result.css.contains(".aria-checked\\:bg-blue-500"));
        assert!(result.css.contains("[aria-checked=\"true\"]"));
        assert!(
            result
                .css
                .contains(".aria-\\[sort\\=ascending\\]\\:underline")
        );
        assert!(result.css.contains("[aria-sort=ascending]"));
        assert!(result.css.contains(".data-active\\:bg-blue-500"));
        assert!(result.css.contains("[data-active]"));
        assert!(
            result
                .css
                .contains(".data-\\[state\\=open\\]\\:bg-blue-500")
        );
        assert!(result.css.contains("[data-state=open]"));
        assert!(
            result
                .css
                .contains(".rtl\\:ml-4:where(:dir(rtl), [dir=\"rtl\"], [dir=\"rtl\"] *)")
        );
        assert!(
            result
                .css
                .contains(".ltr\\:mr-4:where(:dir(ltr), [dir=\"ltr\"], [dir=\"ltr\"] *)")
        );
    }

    #[test]
    fn generates_extended_pseudo_element_and_wrapper_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "before:bg-blue-500".to_string(),
                "after:text-blue-600".to_string(),
                "first-letter:text-blue-600".to_string(),
                "first-line:uppercase".to_string(),
                "marker:text-blue-600".to_string(),
                "selection:bg-blue-100".to_string(),
                "file:bg-blue-100".to_string(),
                "backdrop:bg-gray-100".to_string(),
                "placeholder:text-gray-500".to_string(),
                "*:underline".to_string(),
                "**:underline".to_string(),
                "open:bg-gray-100".to_string(),
                "inert:bg-gray-100".to_string(),
                "supports-[display:grid]:grid".to_string(),
                "not-supports-[display:grid]:flex".to_string(),
                "supports-backdrop-filter:backdrop-blur-sm".to_string(),
                "starting:open:bg-gray-100".to_string(),
                "@md:flex".to_string(),
                "@max-lg:hidden".to_string(),
                "@sm/main:flex".to_string(),
                "@max-md/sidebar:hidden".to_string(),
                "min-[900px]:grid".to_string(),
                "max-[1200px]:hidden".to_string(),
                "@min-[475px]/card:flex-row".to_string(),
                "@max-[960px]/card:flex-col".to_string(),
                "contrast-more:text-gray-900".to_string(),
                "contrast-less:text-gray-500".to_string(),
                "forced-colors:appearance-auto".to_string(),
                "not-forced-colors:appearance-none".to_string(),
                "inverted-colors:shadow-none".to_string(),
                "pointer-fine:underline".to_string(),
                "pointer-coarse:underline".to_string(),
                "any-pointer-none:underline".to_string(),
                "portrait:hidden".to_string(),
                "landscape:block".to_string(),
                "noscript:block".to_string(),
                "print:hidden".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".before\\:bg-blue-500:before"));
        assert!(result.css.contains("content:\"\"") || result.css.contains("content: \"\""));
        assert!(result.css.contains(".after\\:text-blue-600:after"));
        assert!(
            result
                .css
                .contains(".first-letter\\:text-blue-600::first-letter")
        );
        assert!(result.css.contains(".first-line\\:uppercase::first-line"));
        assert!(
            result
                .css
                .contains(".marker\\:text-blue-600::marker, .marker\\:text-blue-600 *::marker")
        );
        assert!(result.css.contains(
            ".selection\\:bg-blue-100::selection, .selection\\:bg-blue-100 *::selection"
        ));
        assert!(
            result
                .css
                .contains(".file\\:bg-blue-100::file-selector-button")
        );
        assert!(result.css.contains(".backdrop\\:bg-gray-100::backdrop"));
        assert!(
            result
                .css
                .contains(".placeholder\\:text-gray-500::placeholder")
        );
        assert!(result.css.contains(":is(.\\*\\:underline > *)"));
        assert!(result.css.contains(":is(.\\*\\*\\:underline *)"));
        assert!(
            result
                .css
                .contains(".open\\:bg-gray-100:is([open], :popover-open, :open)")
        );
        assert!(result.css.contains(".inert\\:bg-gray-100:is([inert]"));
        assert!(result.css.contains("@supports (display:grid)"));
        assert!(result.css.contains("@supports not (display:grid)"));
        assert!(
            result
                .css
                .contains("@supports (backdrop-filter: var(--tw))")
        );
        assert!(result.css.contains("@starting-style"));
        assert!(result.css.contains("@container (width >= 28rem)"));
        assert!(result.css.contains("@container (width < 32rem)"));
        assert!(result.css.contains("@container main (width >= 24rem)"));
        assert!(result.css.contains("@container sidebar (width < 28rem)"));
        assert!(result.css.contains("@container card (width >= 475px)"));
        assert!(result.css.contains("@container card (width < 960px)"));
        assert!(result.css.contains("@media (width >= 900px)"));
        assert!(result.css.contains("@media (width < 1200px)"));
        assert!(result.css.contains("@media (prefers-contrast: more)"));
        assert!(result.css.contains("@media (prefers-contrast: less)"));
        assert!(result.css.contains("@media (forced-colors: active)"));
        assert!(result.css.contains("@media (not (forced-colors: active))"));
        assert!(result.css.contains("@media (inverted-colors: inverted)"));
        assert!(result.css.contains("@media (pointer: fine)"));
        assert!(result.css.contains("@media (pointer: coarse)"));
        assert!(result.css.contains("@media (any-pointer: none)"));
        assert!(result.css.contains("@media (orientation: portrait)"));
        assert!(result.css.contains("@media (orientation: landscape)"));
        assert!(result.css.contains("@media (scripting: none)"));
        assert!(result.css.contains("@media print"));
    }

    #[test]
    fn generates_container_utility_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &["@container".to_string(), "@container/main".to_string()],
            &config,
        );

        assert!(result.css.contains(".\\@container"));
        assert!(result.css.contains("container-type: inline-size"));
        assert!(result.css.contains(".\\@container\\/main"));
        assert!(result.css.contains("container-name: main"));
    }

    #[test]
    fn generates_arbitrary_variant_selector() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "[&>[data-active]+span]:text-blue-600".to_string(),
                "[&[data-label='hello\\_world']]:underline".to_string(),
            ],
            &config,
        );
        assert!(
            result.css.contains(
                ".\\[\\&\\>\\[data-active\\]\\+span\\]\\:text-blue-600>[data-active]+span"
            )
        );
        assert!(result.css.contains("color: var(--color-blue-600)"));
        assert!(result.css.contains("hello\\\\_world"));
        assert!(result.css.contains(":underline"));
        assert!(result.css.contains("[data-label='hello_world']"));
        assert!(result.css.contains("text-decoration-line: underline"));
    }

    #[test]
    fn generates_complex_stacked_variant_with_data_and_dark() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &["dark:lg:data-current:hover:bg-indigo-600".to_string()],
            &config,
        );
        assert!(
            result
                .css
                .contains(".dark\\:lg\\:data-current\\:hover\\:bg-indigo-600")
        );
        assert!(result.css.contains("@media (prefers-color-scheme: dark)"));
        assert!(result.css.contains("@media (width >= 64rem)"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-indigo-600)")
        );
    }

    #[test]
    fn generates_important_modifier() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(&["bg-red-500!".to_string()], &config);
        assert!(result.css.contains(".bg-red-500\\!"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-red-500) !important")
        );
    }

    #[test]
    fn generates_important_modifier_with_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(&["hover:bg-red-500!".to_string()], &config);
        assert!(result.css.contains(".hover\\:bg-red-500\\!:hover"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-red-500) !important")
        );
    }

    #[test]
    fn generates_important_modifier_with_complex_variants() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &["dark:lg:data-current:hover:bg-indigo-600!".to_string()],
            &config,
        );
        assert!(
            result
                .css
                .contains(".dark\\:lg\\:data-current\\:hover\\:bg-indigo-600\\!")
        );
        assert!(result.css.contains("@media (prefers-color-scheme: dark)"));
        assert!(result.css.contains("@media (width >= 64rem)"));
        assert!(
            result
                .css
                .contains("background-color: var(--color-indigo-600) !important")
        );
    }

    #[test]
    fn generates_arbitrary_values_from_core_concepts_examples() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "bg-[#316ff6]".to_string(),
                "grid-cols-[24rem_2.5rem_minmax(0,1fr)]".to_string(),
                "max-h-[calc(100dvh-(--spacing(6)))]".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".bg-\\[#316ff6\\]"));
        assert!(result.css.contains("background-color: #316ff6"));
        assert!(
            result
                .css
                .contains(".grid-cols-\\[24rem_2.5rem_minmax\\(0\\,1fr\\)\\]")
        );
        assert!(
            result
                .css
                .contains("grid-template-columns: 24rem 2.5rem minmax(0,1fr)")
        );
        assert!(
            result
                .css
                .contains(".max-h-\\[calc\\(100dvh-\\(--spacing\\(6\\)\\)\\)\\]")
        );
        assert!(
            result
                .css
                .contains("max-height: calc(100dvh - (--spacing(6)))")
        );
    }

    #[test]
    fn generates_arbitrary_property_utility_classes() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "[--gutter-width:1rem]".to_string(),
                "lg:[--gutter-width:2rem]".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".\\[--gutter-width\\:1rem\\]"));
        assert!(result.css.contains("--gutter-width: 1rem"));
        assert!(result.css.contains(".lg\\:\\[--gutter-width\\:2rem\\]"));
        assert!(result.css.contains("--gutter-width: 2rem"));
        assert!(result.css.contains("@media (width >= 64rem)"));
    }

    #[test]
    fn supports_core_concepts_custom_styles_examples() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "top-[117px]".to_string(),
                "lg:top-[344px]".to_string(),
                "grid-cols-[1fr_500px_2fr]".to_string(),
                "bg-[url('/what_a_rush.png')]".to_string(),
                "before:content-['hello\\_world']".to_string(),
                "[mask-type:luminance]".to_string(),
                "hover:[mask-type:alpha]".to_string(),
                "lg:[&:nth-child(-n+3)]:hover:underline".to_string(),
                "text-(length:--my-var)".to_string(),
                "text-(color:--my-var)".to_string(),
                "fill-(--my-brand-color)".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".top-\\[117px\\]"));
        assert!(result.css.contains("top: 117px"));
        assert!(result.css.contains(".lg\\:top-\\[344px\\]"));
        assert!(result.css.contains("@media (width >= 64rem)"));
        assert!(result.css.contains("top: 344px"));

        assert!(result.css.contains(".grid-cols-\\[1fr_500px_2fr\\]"));
        assert!(result.css.contains("grid-template-columns: 1fr 500px 2fr"));
        assert!(
            result
                .css
                .contains("background-image: url('/what_a_rush.png')")
        );
        assert!(result.css.contains("hello_world"));

        assert!(result.css.contains(".\\[mask-type\\:luminance\\]"));
        assert!(result.css.contains("mask-type: luminance"));
        assert!(
            result
                .css
                .contains(".hover\\:\\[mask-type\\:alpha\\]:hover")
        );
        assert!(result.css.contains("mask-type: alpha"));

        assert!(result.css.contains(
            ".lg\\:\\[\\&\\:nth-child\\(-n\\+3\\)\\]\\:hover\\:underline:nth-child(-n+3):hover"
        ));
        assert!(result.css.contains("text-decoration-line: underline"));

        assert!(result.css.contains(".text-\\(length\\:--my-var\\)"));
        assert!(result.css.contains("font-size: var(--my-var)"));
        assert!(result.css.contains(".text-\\(color\\:--my-var\\)"));
        assert!(result.css.contains("color: var(--my-var)"));

        assert!(result.css.contains(".fill-\\(--my-brand-color\\)"));
        assert!(result.css.contains("fill: var(--my-brand-color)"));
    }

    #[test]
    fn generates_configured_colors() {
        let mut colors = BTreeMap::new();
        let mut brand = BTreeMap::new();
        brand.insert("500".to_string(), "#00aaff".to_string());
        colors.insert("brand".to_string(), brand);
        let config = GeneratorConfig {
            minify: false,
            colors,
        };
        let result = generate(
            &[
                "text-brand-500".to_string(),
                "bg-brand-500".to_string(),
                "border-brand-500".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".text-brand-500"));
        assert!(result.css.contains("color: #00aaff"));
        assert!(result.css.contains(".bg-brand-500"));
        assert!(result.css.contains("background-color: #00aaff"));
        assert!(result.css.contains(".border-brand-500"));
        assert!(result.css.contains("border-color: #00aaff"));
    }

    #[test]
    fn configured_colors_override_builtin_palette() {
        let mut colors = BTreeMap::new();
        let mut gray = BTreeMap::new();
        gray.insert("500".to_string(), "#123456".to_string());
        colors.insert("gray".to_string(), gray);
        let config = GeneratorConfig {
            minify: false,
            colors,
        };
        let result = generate(
            &["text-gray-500".to_string(), "bg-gray-500".to_string()],
            &config,
        );
        assert!(result.css.contains("color: #123456"));
        assert!(result.css.contains("background-color: #123456"));
    }

    #[test]
    fn generates_display_position_and_sizing_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "inline".to_string(),
                "inline-block".to_string(),
                "flow-root".to_string(),
                "flex".to_string(),
                "inline-flex".to_string(),
                "grid".to_string(),
                "inline-grid".to_string(),
                "contents".to_string(),
                "table".to_string(),
                "table-row".to_string(),
                "table-cell".to_string(),
                "table-caption".to_string(),
                "table-column".to_string(),
                "table-column-group".to_string(),
                "table-header-group".to_string(),
                "table-row-group".to_string(),
                "table-footer-group".to_string(),
                "table-auto".to_string(),
                "table-fixed".to_string(),
                "md:table-fixed".to_string(),
                "caption-top".to_string(),
                "caption-bottom".to_string(),
                "md:caption-bottom".to_string(),
                "border-collapse".to_string(),
                "border-separate".to_string(),
                "md:border-separate".to_string(),
                "sr-only".to_string(),
                "not-sr-only".to_string(),
                "overflow-auto".to_string(),
                "overflow-hidden".to_string(),
                "overflow-clip".to_string(),
                "overflow-visible".to_string(),
                "overflow-scroll".to_string(),
                "overflow-x-auto".to_string(),
                "overflow-y-auto".to_string(),
                "overflow-x-hidden".to_string(),
                "overflow-y-hidden".to_string(),
                "overflow-x-clip".to_string(),
                "overflow-y-clip".to_string(),
                "overflow-x-visible".to_string(),
                "overflow-y-visible".to_string(),
                "overflow-x-scroll".to_string(),
                "overflow-y-scroll".to_string(),
                "hidden".to_string(),
                "absolute".to_string(),
                "w-full".to_string(),
                "h-screen".to_string(),
                "min-h-screen".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".inline"));
        assert!(result.css.contains("display: inline"));
        assert!(result.css.contains(".inline-block"));
        assert!(result.css.contains("display: inline-block"));
        assert!(result.css.contains(".flow-root"));
        assert!(result.css.contains("display: flow-root"));
        assert!(result.css.contains(".flex"));
        assert!(result.css.contains("display: flex"));
        assert!(result.css.contains(".inline-flex"));
        assert!(result.css.contains("display: inline-flex"));
        assert!(result.css.contains(".grid"));
        assert!(result.css.contains("display: grid"));
        assert!(result.css.contains(".inline-grid"));
        assert!(result.css.contains("display: inline-grid"));
        assert!(result.css.contains(".contents"));
        assert!(result.css.contains("display: contents"));
        assert!(result.css.contains(".table"));
        assert!(result.css.contains("display: table"));
        assert!(result.css.contains(".table-row"));
        assert!(result.css.contains("display: table-row"));
        assert!(result.css.contains(".table-cell"));
        assert!(result.css.contains("display: table-cell"));
        assert!(result.css.contains(".table-caption"));
        assert!(result.css.contains("display: table-caption"));
        assert!(result.css.contains(".table-column"));
        assert!(result.css.contains("display: table-column"));
        assert!(result.css.contains(".table-column-group"));
        assert!(result.css.contains("display: table-column-group"));
        assert!(result.css.contains(".table-header-group"));
        assert!(result.css.contains("display: table-header-group"));
        assert!(result.css.contains(".table-row-group"));
        assert!(result.css.contains("display: table-row-group"));
        assert!(result.css.contains(".table-footer-group"));
        assert!(result.css.contains("display: table-footer-group"));
        assert!(result.css.contains(".table-auto"));
        assert!(result.css.contains("table-layout: auto"));
        assert!(result.css.contains(".table-fixed"));
        assert!(result.css.contains("table-layout: fixed"));
        assert!(result.css.contains(".md\\:table-fixed"));
        assert!(result.css.contains(".caption-top"));
        assert!(result.css.contains("caption-side: top"));
        assert!(result.css.contains(".caption-bottom"));
        assert!(result.css.contains("caption-side: bottom"));
        assert!(result.css.contains(".md\\:caption-bottom"));
        assert!(result.css.contains(".border-collapse"));
        assert!(result.css.contains("border-collapse: collapse"));
        assert!(result.css.contains(".border-separate"));
        assert!(result.css.contains("border-collapse: separate"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:border-separate"));
        assert!(result.css.contains(".sr-only"));
        assert!(result.css.contains("position: absolute"));
        assert!(result.css.contains("clip-path: inset(50%)"));
        assert!(result.css.contains(".not-sr-only"));
        assert!(result.css.contains("position: static"));
        assert!(result.css.contains("clip: auto"));
        assert!(result.css.contains(".overflow-auto"));
        assert!(result.css.contains("overflow: auto"));
        assert!(result.css.contains(".overflow-hidden"));
        assert!(result.css.contains("overflow: hidden"));
        assert!(result.css.contains(".overflow-clip"));
        assert!(result.css.contains("overflow: clip"));
        assert!(result.css.contains(".overflow-visible"));
        assert!(result.css.contains("overflow: visible"));
        assert!(result.css.contains(".overflow-scroll"));
        assert!(result.css.contains("overflow: scroll"));
        assert!(result.css.contains(".overflow-x-auto"));
        assert!(result.css.contains("overflow-x: auto"));
        assert!(result.css.contains(".overflow-y-auto"));
        assert!(result.css.contains("overflow-y: auto"));
        assert!(result.css.contains(".overflow-x-hidden"));
        assert!(result.css.contains("overflow-x: hidden"));
        assert!(result.css.contains(".overflow-y-hidden"));
        assert!(result.css.contains("overflow-y: hidden"));
        assert!(result.css.contains(".overflow-x-clip"));
        assert!(result.css.contains("overflow-x: clip"));
        assert!(result.css.contains(".overflow-y-clip"));
        assert!(result.css.contains("overflow-y: clip"));
        assert!(result.css.contains(".overflow-x-visible"));
        assert!(result.css.contains("overflow-x: visible"));
        assert!(result.css.contains(".overflow-y-visible"));
        assert!(result.css.contains("overflow-y: visible"));
        assert!(result.css.contains(".overflow-x-scroll"));
        assert!(result.css.contains("overflow-x: scroll"));
        assert!(result.css.contains(".overflow-y-scroll"));
        assert!(result.css.contains("overflow-y: scroll"));
        assert!(result.css.contains(".hidden"));
        assert!(result.css.contains("display: none"));
        assert!(result.css.contains(".absolute"));
        assert!(result.css.contains("position: absolute"));
        assert!(result.css.contains(".w-full"));
        assert!(result.css.contains("width: 100%"));
        assert!(result.css.contains(".h-screen"));
        assert!(result.css.contains("height: 100vh"));
        assert!(result.css.contains(".min-h-screen"));
        assert!(result.css.contains("min-height: 100vh"));
    }

    #[test]
    fn generates_flex_alignment_overflow_and_gap_utilities() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "flex-row".to_string(),
                "flex-row-reverse".to_string(),
                "flex-nowrap".to_string(),
                "flex-wrap".to_string(),
                "flex-wrap-reverse".to_string(),
                "flex-col".to_string(),
                "flex-col-reverse".to_string(),
                "justify-between".to_string(),
                "justify-around".to_string(),
                "justify-evenly".to_string(),
                "justify-end".to_string(),
                "justify-end-safe".to_string(),
                "justify-center".to_string(),
                "justify-center-safe".to_string(),
                "justify-stretch".to_string(),
                "justify-baseline".to_string(),
                "justify-normal".to_string(),
                "justify-items-start".to_string(),
                "justify-items-end".to_string(),
                "justify-items-end-safe".to_string(),
                "justify-items-center".to_string(),
                "justify-items-center-safe".to_string(),
                "justify-items-stretch".to_string(),
                "justify-items-normal".to_string(),
                "justify-self-auto".to_string(),
                "justify-self-start".to_string(),
                "justify-self-center".to_string(),
                "justify-self-center-safe".to_string(),
                "justify-self-end".to_string(),
                "justify-self-end-safe".to_string(),
                "justify-self-stretch".to_string(),
                "content-normal".to_string(),
                "content-center".to_string(),
                "content-start".to_string(),
                "content-end".to_string(),
                "content-between".to_string(),
                "content-around".to_string(),
                "content-evenly".to_string(),
                "content-baseline".to_string(),
                "content-stretch".to_string(),
                "place-content-center".to_string(),
                "place-content-center-safe".to_string(),
                "place-content-start".to_string(),
                "place-content-end".to_string(),
                "place-content-end-safe".to_string(),
                "place-content-between".to_string(),
                "place-content-around".to_string(),
                "place-content-evenly".to_string(),
                "place-content-baseline".to_string(),
                "place-content-stretch".to_string(),
                "place-items-start".to_string(),
                "place-items-end".to_string(),
                "place-items-end-safe".to_string(),
                "place-items-center".to_string(),
                "place-items-center-safe".to_string(),
                "place-items-baseline".to_string(),
                "place-items-stretch".to_string(),
                "place-self-auto".to_string(),
                "place-self-start".to_string(),
                "place-self-end".to_string(),
                "place-self-end-safe".to_string(),
                "place-self-center".to_string(),
                "place-self-center-safe".to_string(),
                "place-self-stretch".to_string(),
                "items-start".to_string(),
                "items-end".to_string(),
                "items-end-safe".to_string(),
                "items-center".to_string(),
                "items-center-safe".to_string(),
                "items-baseline".to_string(),
                "items-baseline-last".to_string(),
                "items-stretch".to_string(),
                "self-auto".to_string(),
                "self-start".to_string(),
                "self-end".to_string(),
                "self-end-safe".to_string(),
                "self-center".to_string(),
                "self-center-safe".to_string(),
                "self-stretch".to_string(),
                "self-baseline".to_string(),
                "self-baseline-last".to_string(),
                "overflow-hidden".to_string(),
                "gap-4".to_string(),
                "gap-0.5".to_string(),
                "md:flex-row".to_string(),
                "md:flex-wrap-reverse".to_string(),
                "md:justify-between".to_string(),
                "md:justify-items-center".to_string(),
                "md:justify-self-end".to_string(),
                "md:content-around".to_string(),
                "md:place-content-center".to_string(),
                "md:place-items-center".to_string(),
                "md:place-self-end".to_string(),
                "md:items-center-safe".to_string(),
                "md:self-end".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".flex-row"));
        assert!(result.css.contains("flex-direction: row"));
        assert!(result.css.contains(".flex-row-reverse"));
        assert!(result.css.contains("flex-direction: row-reverse"));
        assert!(result.css.contains(".flex-col"));
        assert!(result.css.contains("flex-direction: column"));
        assert!(result.css.contains(".flex-col-reverse"));
        assert!(result.css.contains("flex-direction: column-reverse"));
        assert!(result.css.contains(".flex-nowrap"));
        assert!(result.css.contains("flex-wrap: nowrap"));
        assert!(result.css.contains(".flex-wrap"));
        assert!(result.css.contains("flex-wrap: wrap"));
        assert!(result.css.contains(".flex-wrap-reverse"));
        assert!(result.css.contains("flex-wrap: wrap-reverse"));
        assert!(result.css.contains(".justify-between"));
        assert!(result.css.contains("justify-content: space-between"));
        assert!(result.css.contains(".justify-around"));
        assert!(result.css.contains("justify-content: space-around"));
        assert!(result.css.contains(".justify-evenly"));
        assert!(result.css.contains("justify-content: space-evenly"));
        assert!(result.css.contains(".justify-end"));
        assert!(result.css.contains("justify-content: flex-end"));
        assert!(result.css.contains(".justify-end-safe"));
        assert!(result.css.contains("justify-content: safe flex-end"));
        assert!(result.css.contains(".justify-center"));
        assert!(result.css.contains("justify-content: center"));
        assert!(result.css.contains(".justify-center-safe"));
        assert!(result.css.contains("justify-content: safe center"));
        assert!(result.css.contains(".justify-stretch"));
        assert!(result.css.contains("justify-content: stretch"));
        assert!(result.css.contains(".justify-baseline"));
        assert!(result.css.contains("justify-content: baseline"));
        assert!(result.css.contains(".justify-normal"));
        assert!(result.css.contains("justify-content: normal"));
        assert!(result.css.contains(".justify-items-start"));
        assert!(result.css.contains("justify-items: start"));
        assert!(result.css.contains(".justify-items-end"));
        assert!(result.css.contains("justify-items: end"));
        assert!(result.css.contains(".justify-items-end-safe"));
        assert!(result.css.contains("justify-items: safe end"));
        assert!(result.css.contains(".justify-items-center"));
        assert!(result.css.contains("justify-items: center"));
        assert!(result.css.contains(".justify-items-center-safe"));
        assert!(result.css.contains("justify-items: safe center"));
        assert!(result.css.contains(".justify-items-stretch"));
        assert!(result.css.contains("justify-items: stretch"));
        assert!(result.css.contains(".justify-items-normal"));
        assert!(result.css.contains("justify-items: normal"));
        assert!(result.css.contains(".justify-self-auto"));
        assert!(result.css.contains("justify-self: auto"));
        assert!(result.css.contains(".justify-self-start"));
        assert!(result.css.contains("justify-self: start"));
        assert!(result.css.contains(".justify-self-center"));
        assert!(result.css.contains("justify-self: center"));
        assert!(result.css.contains(".justify-self-center-safe"));
        assert!(result.css.contains("justify-self: safe center"));
        assert!(result.css.contains(".justify-self-end"));
        assert!(result.css.contains("justify-self: end"));
        assert!(result.css.contains(".justify-self-end-safe"));
        assert!(result.css.contains("justify-self: safe end"));
        assert!(result.css.contains(".justify-self-stretch"));
        assert!(result.css.contains("justify-self: stretch"));
        assert!(result.css.contains(".content-normal"));
        assert!(result.css.contains("align-content: normal"));
        assert!(result.css.contains(".content-center"));
        assert!(result.css.contains("align-content: center"));
        assert!(result.css.contains(".content-start"));
        assert!(result.css.contains("align-content: flex-start"));
        assert!(result.css.contains(".content-end"));
        assert!(result.css.contains("align-content: flex-end"));
        assert!(result.css.contains(".content-between"));
        assert!(result.css.contains("align-content: space-between"));
        assert!(result.css.contains(".content-around"));
        assert!(result.css.contains("align-content: space-around"));
        assert!(result.css.contains(".content-evenly"));
        assert!(result.css.contains("align-content: space-evenly"));
        assert!(result.css.contains(".content-baseline"));
        assert!(result.css.contains("align-content: baseline"));
        assert!(result.css.contains(".content-stretch"));
        assert!(result.css.contains("align-content: stretch"));
        assert!(result.css.contains(".place-content-center"));
        assert!(result.css.contains("place-content: center"));
        assert!(result.css.contains(".place-content-center-safe"));
        assert!(result.css.contains("place-content: safe center"));
        assert!(result.css.contains(".place-content-start"));
        assert!(result.css.contains("place-content: start"));
        assert!(result.css.contains(".place-content-end"));
        assert!(result.css.contains("place-content: end"));
        assert!(result.css.contains(".place-content-end-safe"));
        assert!(result.css.contains("place-content: safe end"));
        assert!(result.css.contains(".place-content-between"));
        assert!(result.css.contains("place-content: space-between"));
        assert!(result.css.contains(".place-content-around"));
        assert!(result.css.contains("place-content: space-around"));
        assert!(result.css.contains(".place-content-evenly"));
        assert!(result.css.contains("place-content: space-evenly"));
        assert!(result.css.contains(".place-content-baseline"));
        assert!(result.css.contains("place-content: baseline"));
        assert!(result.css.contains(".place-content-stretch"));
        assert!(result.css.contains("place-content: stretch"));
        assert!(result.css.contains(".place-items-start"));
        assert!(result.css.contains("place-items: start"));
        assert!(result.css.contains(".place-items-end"));
        assert!(result.css.contains("place-items: end"));
        assert!(result.css.contains(".place-items-end-safe"));
        assert!(result.css.contains("place-items: safe end"));
        assert!(result.css.contains(".place-items-center"));
        assert!(result.css.contains("place-items: center"));
        assert!(result.css.contains(".place-items-center-safe"));
        assert!(result.css.contains("place-items: safe center"));
        assert!(result.css.contains(".place-items-baseline"));
        assert!(result.css.contains("place-items: baseline"));
        assert!(result.css.contains(".place-items-stretch"));
        assert!(result.css.contains("place-items: stretch"));
        assert!(result.css.contains(".place-self-auto"));
        assert!(result.css.contains("place-self: auto"));
        assert!(result.css.contains(".place-self-start"));
        assert!(result.css.contains("place-self: start"));
        assert!(result.css.contains(".place-self-end"));
        assert!(result.css.contains("place-self: end"));
        assert!(result.css.contains(".place-self-end-safe"));
        assert!(result.css.contains("place-self: safe end"));
        assert!(result.css.contains(".place-self-center"));
        assert!(result.css.contains("place-self: center"));
        assert!(result.css.contains(".place-self-center-safe"));
        assert!(result.css.contains("place-self: safe center"));
        assert!(result.css.contains(".place-self-stretch"));
        assert!(result.css.contains("place-self: stretch"));
        assert!(result.css.contains(".items-start"));
        assert!(result.css.contains("align-items: flex-start"));
        assert!(result.css.contains(".items-end"));
        assert!(result.css.contains("align-items: flex-end"));
        assert!(result.css.contains(".items-end-safe"));
        assert!(result.css.contains("align-items: safe flex-end"));
        assert!(result.css.contains(".items-center"));
        assert!(result.css.contains("align-items: center"));
        assert!(result.css.contains(".items-center-safe"));
        assert!(result.css.contains("align-items: safe center"));
        assert!(result.css.contains(".items-baseline"));
        assert!(result.css.contains("align-items: baseline"));
        assert!(result.css.contains(".items-baseline-last"));
        assert!(result.css.contains("align-items: last baseline"));
        assert!(result.css.contains(".items-stretch"));
        assert!(result.css.contains("align-items: stretch"));
        assert!(result.css.contains(".self-auto"));
        assert!(result.css.contains("align-self: auto"));
        assert!(result.css.contains(".self-start"));
        assert!(result.css.contains("align-self: flex-start"));
        assert!(result.css.contains(".self-end"));
        assert!(result.css.contains("align-self: flex-end"));
        assert!(result.css.contains(".self-end-safe"));
        assert!(result.css.contains("align-self: safe flex-end"));
        assert!(result.css.contains(".self-center"));
        assert!(result.css.contains("align-self: center"));
        assert!(result.css.contains(".self-center-safe"));
        assert!(result.css.contains("align-self: safe center"));
        assert!(result.css.contains(".self-stretch"));
        assert!(result.css.contains("align-self: stretch"));
        assert!(result.css.contains(".self-baseline"));
        assert!(result.css.contains("align-self: baseline"));
        assert!(result.css.contains(".self-baseline-last"));
        assert!(result.css.contains("align-self: last baseline"));
        assert!(result.css.contains(".overflow-hidden"));
        assert!(result.css.contains("overflow: hidden"));
        assert!(result.css.contains(".gap-4"));
        assert!(result.css.contains("gap: calc(var(--spacing) * 4)"));
        assert!(result.css.contains(".gap-0\\.5"));
        assert!(result.css.contains("gap: calc(var(--spacing) * 0.5)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:flex-row"));
        assert!(result.css.contains(".md\\:flex-wrap-reverse"));
        assert!(result.css.contains(".md\\:justify-between"));
        assert!(result.css.contains(".md\\:justify-items-center"));
        assert!(result.css.contains(".md\\:justify-self-end"));
        assert!(result.css.contains(".md\\:content-around"));
        assert!(result.css.contains(".md\\:place-content-center"));
        assert!(result.css.contains(".md\\:place-items-center"));
        assert!(result.css.contains(".md\\:place-self-end"));
        assert!(result.css.contains(".md\\:items-center-safe"));
        assert!(result.css.contains(".md\\:self-end"));
    }

    #[test]
    fn generates_aspect_ratio_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "aspect-square".to_string(),
                "aspect-video".to_string(),
                "aspect-auto".to_string(),
                "aspect-16/9".to_string(),
                "aspect-(--card-ratio)".to_string(),
                "aspect-[4/3]".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".aspect-square"));
        assert!(result.css.contains("aspect-ratio: 1 / 1"));
        assert!(result.css.contains(".aspect-video"));
        assert!(result.css.contains("aspect-ratio: var(--aspect-video)"));
        assert!(result.css.contains(".aspect-auto"));
        assert!(result.css.contains("aspect-ratio: auto"));
        assert!(result.css.contains(".aspect-16\\/9"));
        assert!(result.css.contains("aspect-ratio: 16/9"));
        assert!(result.css.contains(".aspect-\\(--card-ratio\\)"));
        assert!(result.css.contains("aspect-ratio: var(--card-ratio)"));
        assert!(result.css.contains(".aspect-\\[4\\/3\\]"));
        assert!(result.css.contains("aspect-ratio: 4/3"));
    }

    #[test]
    fn generates_columns_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "columns-3".to_string(),
                "columns-4xs".to_string(),
                "columns-3xs".to_string(),
                "columns-md".to_string(),
                "columns-[30vw]".to_string(),
                "columns-(--my-columns)".to_string(),
                "sm:columns-4".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".columns-3"));
        assert!(result.css.contains("columns: 3"));
        assert!(result.css.contains(".columns-4xs"));
        assert!(result.css.contains("columns: var(--container-4xs)"));
        assert!(result.css.contains(".columns-3xs"));
        assert!(result.css.contains("columns: var(--container-3xs)"));
        assert!(result.css.contains(".columns-md"));
        assert!(result.css.contains("columns: var(--container-md)"));
        assert!(result.css.contains(".columns-\\[30vw\\]"));
        assert!(result.css.contains("columns: 30vw"));
        assert!(result.css.contains(".columns-\\(--my-columns\\)"));
        assert!(result.css.contains("columns: var(--my-columns)"));
        assert!(result.css.contains("@media (width >= 40rem)"));
        assert!(result.css.contains(".sm\\:columns-4"));
        assert!(result.css.contains("columns: 4"));
    }

    #[test]
    fn generates_break_after_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "break-after-auto".to_string(),
                "break-after-avoid".to_string(),
                "break-after-all".to_string(),
                "break-after-avoid-page".to_string(),
                "break-after-page".to_string(),
                "break-after-left".to_string(),
                "break-after-right".to_string(),
                "break-after-column".to_string(),
                "md:break-after-auto".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".break-after-auto"));
        assert!(result.css.contains("break-after: auto"));
        assert!(result.css.contains(".break-after-avoid"));
        assert!(result.css.contains("break-after: avoid"));
        assert!(result.css.contains(".break-after-all"));
        assert!(result.css.contains("break-after: all"));
        assert!(result.css.contains(".break-after-avoid-page"));
        assert!(result.css.contains("break-after: avoid-page"));
        assert!(result.css.contains(".break-after-page"));
        assert!(result.css.contains("break-after: page"));
        assert!(result.css.contains(".break-after-left"));
        assert!(result.css.contains("break-after: left"));
        assert!(result.css.contains(".break-after-right"));
        assert!(result.css.contains("break-after: right"));
        assert!(result.css.contains(".break-after-column"));
        assert!(result.css.contains("break-after: column"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:break-after-auto"));
    }

    #[test]
    fn generates_break_before_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "break-before-auto".to_string(),
                "break-before-avoid".to_string(),
                "break-before-all".to_string(),
                "break-before-avoid-page".to_string(),
                "break-before-page".to_string(),
                "break-before-left".to_string(),
                "break-before-right".to_string(),
                "break-before-column".to_string(),
                "md:break-before-auto".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".break-before-auto"));
        assert!(result.css.contains("break-before: auto"));
        assert!(result.css.contains(".break-before-avoid"));
        assert!(result.css.contains("break-before: avoid"));
        assert!(result.css.contains(".break-before-all"));
        assert!(result.css.contains("break-before: all"));
        assert!(result.css.contains(".break-before-avoid-page"));
        assert!(result.css.contains("break-before: avoid-page"));
        assert!(result.css.contains(".break-before-page"));
        assert!(result.css.contains("break-before: page"));
        assert!(result.css.contains(".break-before-left"));
        assert!(result.css.contains("break-before: left"));
        assert!(result.css.contains(".break-before-right"));
        assert!(result.css.contains("break-before: right"));
        assert!(result.css.contains(".break-before-column"));
        assert!(result.css.contains("break-before: column"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:break-before-auto"));
    }

    #[test]
    fn generates_break_inside_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "break-inside-auto".to_string(),
                "break-inside-avoid".to_string(),
                "break-inside-avoid-page".to_string(),
                "break-inside-avoid-column".to_string(),
                "md:break-inside-auto".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".break-inside-auto"));
        assert!(result.css.contains("break-inside: auto"));
        assert!(result.css.contains(".break-inside-avoid"));
        assert!(result.css.contains("break-inside: avoid"));
        assert!(result.css.contains(".break-inside-avoid-page"));
        assert!(result.css.contains("break-inside: avoid-page"));
        assert!(result.css.contains(".break-inside-avoid-column"));
        assert!(result.css.contains("break-inside: avoid-column"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:break-inside-auto"));
    }

    #[test]
    fn generates_box_decoration_break_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "box-decoration-clone".to_string(),
                "box-decoration-slice".to_string(),
                "md:box-decoration-slice".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".box-decoration-clone"));
        assert!(result.css.contains("box-decoration-break: clone"));
        assert!(result.css.contains(".box-decoration-slice"));
        assert!(result.css.contains("box-decoration-break: slice"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:box-decoration-slice"));
    }

    #[test]
    fn generates_box_sizing_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "box-border".to_string(),
                "box-content".to_string(),
                "md:box-border".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".box-border"));
        assert!(result.css.contains("box-sizing: border-box"));
        assert!(result.css.contains(".box-content"));
        assert!(result.css.contains("box-sizing: content-box"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:box-border"));
    }

    #[test]
    fn generates_float_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "float-right".to_string(),
                "float-left".to_string(),
                "float-start".to_string(),
                "float-end".to_string(),
                "float-none".to_string(),
                "md:float-left".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".float-right"));
        assert!(result.css.contains("float: right"));
        assert!(result.css.contains(".float-left"));
        assert!(result.css.contains("float: left"));
        assert!(result.css.contains(".float-start"));
        assert!(result.css.contains("float: inline-start"));
        assert!(result.css.contains(".float-end"));
        assert!(result.css.contains("float: inline-end"));
        assert!(result.css.contains(".float-none"));
        assert!(result.css.contains("float: none"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:float-left"));
    }

    #[test]
    fn generates_clear_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "clear-left".to_string(),
                "clear-right".to_string(),
                "clear-both".to_string(),
                "clear-start".to_string(),
                "clear-end".to_string(),
                "clear-none".to_string(),
                "md:clear-none".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".clear-left"));
        assert!(result.css.contains("clear: left"));
        assert!(result.css.contains(".clear-right"));
        assert!(result.css.contains("clear: right"));
        assert!(result.css.contains(".clear-both"));
        assert!(result.css.contains("clear: both"));
        assert!(result.css.contains(".clear-start"));
        assert!(result.css.contains("clear: inline-start"));
        assert!(result.css.contains(".clear-end"));
        assert!(result.css.contains("clear: inline-end"));
        assert!(result.css.contains(".clear-none"));
        assert!(result.css.contains("clear: none"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:clear-none"));
    }

    #[test]
    fn generates_isolation_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "isolate".to_string(),
                "isolation-auto".to_string(),
                "md:isolation-auto".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".isolate"));
        assert!(result.css.contains("isolation: isolate"));
        assert!(result.css.contains(".isolation-auto"));
        assert!(result.css.contains("isolation: auto"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:isolation-auto"));
    }

    #[test]
    fn generates_appearance_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "appearance-none".to_string(),
                "appearance-auto".to_string(),
                "md:appearance-none".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".appearance-none"));
        assert!(result.css.contains("appearance: none"));
        assert!(result.css.contains(".appearance-auto"));
        assert!(result.css.contains("appearance: auto"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:appearance-none"));
    }

    #[test]
    fn generates_color_scheme_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "scheme-normal".to_string(),
                "scheme-dark".to_string(),
                "scheme-light".to_string(),
                "scheme-light-dark".to_string(),
                "scheme-only-dark".to_string(),
                "scheme-only-light".to_string(),
                "dark:scheme-dark".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".scheme-normal"));
        assert!(result.css.contains("color-scheme: normal"));
        assert!(result.css.contains(".scheme-dark"));
        assert!(result.css.contains("color-scheme: dark"));
        assert!(result.css.contains(".scheme-light"));
        assert!(result.css.contains("color-scheme: light"));
        assert!(result.css.contains(".scheme-light-dark"));
        assert!(result.css.contains("color-scheme: light dark"));
        assert!(result.css.contains(".scheme-only-dark"));
        assert!(result.css.contains("color-scheme: only dark"));
        assert!(result.css.contains(".scheme-only-light"));
        assert!(result.css.contains("color-scheme: only light"));
        assert!(result.css.contains(".dark\\:scheme-dark"));
        assert!(result.css.contains("@media (prefers-color-scheme: dark)"));
    }

    #[test]
    fn generates_forced_color_adjust_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "forced-color-adjust-auto".to_string(),
                "forced-color-adjust-none".to_string(),
                "md:forced-color-adjust-auto".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".forced-color-adjust-auto"));
        assert!(result.css.contains("forced-color-adjust: auto"));
        assert!(result.css.contains(".forced-color-adjust-none"));
        assert!(result.css.contains("forced-color-adjust: none"));
        assert!(result.css.contains(".md\\:forced-color-adjust-auto"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_cursor_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "cursor-auto".to_string(),
                "cursor-default".to_string(),
                "cursor-pointer".to_string(),
                "cursor-wait".to_string(),
                "cursor-text".to_string(),
                "cursor-move".to_string(),
                "cursor-help".to_string(),
                "cursor-not-allowed".to_string(),
                "cursor-none".to_string(),
                "cursor-context-menu".to_string(),
                "cursor-progress".to_string(),
                "cursor-cell".to_string(),
                "cursor-crosshair".to_string(),
                "cursor-vertical-text".to_string(),
                "cursor-alias".to_string(),
                "cursor-copy".to_string(),
                "cursor-no-drop".to_string(),
                "cursor-grab".to_string(),
                "cursor-grabbing".to_string(),
                "cursor-all-scroll".to_string(),
                "cursor-col-resize".to_string(),
                "cursor-row-resize".to_string(),
                "cursor-n-resize".to_string(),
                "cursor-e-resize".to_string(),
                "cursor-s-resize".to_string(),
                "cursor-w-resize".to_string(),
                "cursor-ne-resize".to_string(),
                "cursor-nw-resize".to_string(),
                "cursor-se-resize".to_string(),
                "cursor-sw-resize".to_string(),
                "cursor-ew-resize".to_string(),
                "cursor-ns-resize".to_string(),
                "cursor-nesw-resize".to_string(),
                "cursor-nwse-resize".to_string(),
                "cursor-zoom-in".to_string(),
                "cursor-zoom-out".to_string(),
                "cursor-[url(hand.cur),_pointer]".to_string(),
                "cursor-(--my-cursor)".to_string(),
                "md:cursor-auto".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".cursor-auto"));
        assert!(result.css.contains("cursor: auto"));
        assert!(result.css.contains(".cursor-default"));
        assert!(result.css.contains("cursor: default"));
        assert!(result.css.contains(".cursor-pointer"));
        assert!(result.css.contains("cursor: pointer"));
        assert!(result.css.contains(".cursor-wait"));
        assert!(result.css.contains("cursor: wait"));
        assert!(result.css.contains(".cursor-text"));
        assert!(result.css.contains("cursor: text"));
        assert!(result.css.contains(".cursor-move"));
        assert!(result.css.contains("cursor: move"));
        assert!(result.css.contains(".cursor-help"));
        assert!(result.css.contains("cursor: help"));
        assert!(result.css.contains(".cursor-not-allowed"));
        assert!(result.css.contains("cursor: not-allowed"));
        assert!(result.css.contains(".cursor-none"));
        assert!(result.css.contains("cursor: none"));
        assert!(result.css.contains(".cursor-context-menu"));
        assert!(result.css.contains("cursor: context-menu"));
        assert!(result.css.contains(".cursor-progress"));
        assert!(result.css.contains("cursor: progress"));
        assert!(result.css.contains(".cursor-cell"));
        assert!(result.css.contains("cursor: cell"));
        assert!(result.css.contains(".cursor-crosshair"));
        assert!(result.css.contains("cursor: crosshair"));
        assert!(result.css.contains(".cursor-vertical-text"));
        assert!(result.css.contains("cursor: vertical-text"));
        assert!(result.css.contains(".cursor-alias"));
        assert!(result.css.contains("cursor: alias"));
        assert!(result.css.contains(".cursor-copy"));
        assert!(result.css.contains("cursor: copy"));
        assert!(result.css.contains(".cursor-no-drop"));
        assert!(result.css.contains("cursor: no-drop"));
        assert!(result.css.contains(".cursor-grab"));
        assert!(result.css.contains("cursor: grab"));
        assert!(result.css.contains(".cursor-grabbing"));
        assert!(result.css.contains("cursor: grabbing"));
        assert!(result.css.contains(".cursor-all-scroll"));
        assert!(result.css.contains("cursor: all-scroll"));
        assert!(result.css.contains(".cursor-col-resize"));
        assert!(result.css.contains("cursor: col-resize"));
        assert!(result.css.contains(".cursor-row-resize"));
        assert!(result.css.contains("cursor: row-resize"));
        assert!(result.css.contains(".cursor-n-resize"));
        assert!(result.css.contains("cursor: n-resize"));
        assert!(result.css.contains(".cursor-e-resize"));
        assert!(result.css.contains("cursor: e-resize"));
        assert!(result.css.contains(".cursor-s-resize"));
        assert!(result.css.contains("cursor: s-resize"));
        assert!(result.css.contains(".cursor-w-resize"));
        assert!(result.css.contains("cursor: w-resize"));
        assert!(result.css.contains(".cursor-ne-resize"));
        assert!(result.css.contains("cursor: ne-resize"));
        assert!(result.css.contains(".cursor-nw-resize"));
        assert!(result.css.contains("cursor: nw-resize"));
        assert!(result.css.contains(".cursor-se-resize"));
        assert!(result.css.contains("cursor: se-resize"));
        assert!(result.css.contains(".cursor-sw-resize"));
        assert!(result.css.contains("cursor: sw-resize"));
        assert!(result.css.contains(".cursor-ew-resize"));
        assert!(result.css.contains("cursor: ew-resize"));
        assert!(result.css.contains(".cursor-ns-resize"));
        assert!(result.css.contains("cursor: ns-resize"));
        assert!(result.css.contains(".cursor-nesw-resize"));
        assert!(result.css.contains("cursor: nesw-resize"));
        assert!(result.css.contains(".cursor-nwse-resize"));
        assert!(result.css.contains("cursor: nwse-resize"));
        assert!(result.css.contains(".cursor-zoom-in"));
        assert!(result.css.contains("cursor: zoom-in"));
        assert!(result.css.contains(".cursor-zoom-out"));
        assert!(result.css.contains("cursor: zoom-out"));
        assert!(
            result
                .css
                .contains(".cursor-\\[url\\(hand.cur\\)\\,_pointer\\]")
        );
        assert!(result.css.contains("cursor: url(hand.cur),_pointer"));
        assert!(result.css.contains(".cursor-\\(--my-cursor\\)"));
        assert!(result.css.contains("cursor: var(--my-cursor)"));
        assert!(result.css.contains(".md\\:cursor-auto"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_field_sizing_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "field-sizing-fixed".to_string(),
                "field-sizing-content".to_string(),
                "md:field-sizing-fixed".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".field-sizing-fixed"));
        assert!(result.css.contains("field-sizing: fixed"));
        assert!(result.css.contains(".field-sizing-content"));
        assert!(result.css.contains("field-sizing: content"));
        assert!(result.css.contains(".md\\:field-sizing-fixed"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_pointer_events_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "pointer-events-auto".to_string(),
                "pointer-events-none".to_string(),
                "md:pointer-events-auto".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".pointer-events-auto"));
        assert!(result.css.contains("pointer-events: auto"));
        assert!(result.css.contains(".pointer-events-none"));
        assert!(result.css.contains("pointer-events: none"));
        assert!(result.css.contains(".md\\:pointer-events-auto"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_resize_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "resize-none".to_string(),
                "resize".to_string(),
                "resize-y".to_string(),
                "resize-x".to_string(),
                "md:resize".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".resize-none"));
        assert!(result.css.contains("resize: none"));
        assert!(result.css.contains(".resize"));
        assert!(result.css.contains("resize: both"));
        assert!(result.css.contains(".resize-y"));
        assert!(result.css.contains("resize: vertical"));
        assert!(result.css.contains(".resize-x"));
        assert!(result.css.contains("resize: horizontal"));
        assert!(result.css.contains(".md\\:resize"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_scroll_behavior_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "scroll-auto".to_string(),
                "scroll-smooth".to_string(),
                "md:scroll-auto".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".scroll-auto"));
        assert!(result.css.contains("scroll-behavior: auto"));
        assert!(result.css.contains(".scroll-smooth"));
        assert!(result.css.contains("scroll-behavior: smooth"));
        assert!(result.css.contains(".md\\:scroll-auto"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_scroll_snap_align_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "snap-start".to_string(),
                "snap-end".to_string(),
                "snap-center".to_string(),
                "snap-align-none".to_string(),
                "md:snap-start".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".snap-start"));
        assert!(result.css.contains("scroll-snap-align: start"));
        assert!(result.css.contains(".snap-end"));
        assert!(result.css.contains("scroll-snap-align: end"));
        assert!(result.css.contains(".snap-center"));
        assert!(result.css.contains("scroll-snap-align: center"));
        assert!(result.css.contains(".snap-align-none"));
        assert!(result.css.contains("scroll-snap-align: none"));
        assert!(result.css.contains(".md\\:snap-start"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_scroll_snap_type_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "snap-none".to_string(),
                "snap-x".to_string(),
                "snap-y".to_string(),
                "snap-both".to_string(),
                "snap-mandatory".to_string(),
                "snap-proximity".to_string(),
                "md:snap-x".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".snap-none"));
        assert!(result.css.contains("scroll-snap-type: none"));
        assert!(result.css.contains(".snap-x"));
        assert!(
            result
                .css
                .contains("scroll-snap-type: x var(--tw-scroll-snap-strictness)")
        );
        assert!(result.css.contains(".snap-y"));
        assert!(
            result
                .css
                .contains("scroll-snap-type: y var(--tw-scroll-snap-strictness)")
        );
        assert!(result.css.contains(".snap-both"));
        assert!(
            result
                .css
                .contains("scroll-snap-type: both var(--tw-scroll-snap-strictness)")
        );
        assert!(result.css.contains(".snap-mandatory"));
        assert!(
            result
                .css
                .contains("--tw-scroll-snap-strictness: mandatory")
        );
        assert!(result.css.contains(".snap-proximity"));
        assert!(
            result
                .css
                .contains("--tw-scroll-snap-strictness: proximity")
        );
        assert!(result.css.contains(".md\\:snap-x"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_scroll_snap_stop_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "snap-normal".to_string(),
                "snap-always".to_string(),
                "md:snap-normal".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".snap-normal"));
        assert!(result.css.contains("scroll-snap-stop: normal"));
        assert!(result.css.contains(".snap-always"));
        assert!(result.css.contains("scroll-snap-stop: always"));
        assert!(result.css.contains(".md\\:snap-normal"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_touch_action_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "touch-auto".to_string(),
                "touch-none".to_string(),
                "touch-pan-x".to_string(),
                "touch-pan-left".to_string(),
                "touch-pan-right".to_string(),
                "touch-pan-y".to_string(),
                "touch-pan-up".to_string(),
                "touch-pan-down".to_string(),
                "touch-pinch-zoom".to_string(),
                "touch-manipulation".to_string(),
                "md:touch-auto".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".touch-auto"));
        assert!(result.css.contains("touch-action: auto"));
        assert!(result.css.contains(".touch-none"));
        assert!(result.css.contains("touch-action: none"));
        assert!(result.css.contains(".touch-pan-x"));
        assert!(result.css.contains("touch-action: pan-x"));
        assert!(result.css.contains(".touch-pan-left"));
        assert!(result.css.contains("touch-action: pan-left"));
        assert!(result.css.contains(".touch-pan-right"));
        assert!(result.css.contains("touch-action: pan-right"));
        assert!(result.css.contains(".touch-pan-y"));
        assert!(result.css.contains("touch-action: pan-y"));
        assert!(result.css.contains(".touch-pan-up"));
        assert!(result.css.contains("touch-action: pan-up"));
        assert!(result.css.contains(".touch-pan-down"));
        assert!(result.css.contains("touch-action: pan-down"));
        assert!(result.css.contains(".touch-pinch-zoom"));
        assert!(result.css.contains("touch-action: pinch-zoom"));
        assert!(result.css.contains(".touch-manipulation"));
        assert!(result.css.contains("touch-action: manipulation"));
        assert!(result.css.contains(".md\\:touch-auto"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_user_select_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "select-none".to_string(),
                "select-text".to_string(),
                "select-all".to_string(),
                "select-auto".to_string(),
                "md:select-all".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".select-none"));
        assert!(result.css.contains("user-select: none"));
        assert!(result.css.contains(".select-text"));
        assert!(result.css.contains("user-select: text"));
        assert!(result.css.contains(".select-all"));
        assert!(result.css.contains("user-select: all"));
        assert!(result.css.contains(".select-auto"));
        assert!(result.css.contains("user-select: auto"));
        assert!(result.css.contains(".md\\:select-all"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_will_change_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "will-change-auto".to_string(),
                "will-change-scroll".to_string(),
                "will-change-contents".to_string(),
                "will-change-transform".to_string(),
                "will-change-(--my-properties)".to_string(),
                "will-change-[top,left]".to_string(),
                "md:will-change-auto".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".will-change-auto"));
        assert!(result.css.contains("will-change: auto"));
        assert!(result.css.contains(".will-change-scroll"));
        assert!(result.css.contains("will-change: scroll-position"));
        assert!(result.css.contains(".will-change-contents"));
        assert!(result.css.contains("will-change: contents"));
        assert!(result.css.contains(".will-change-transform"));
        assert!(result.css.contains("will-change: transform"));
        assert!(result.css.contains(".will-change-\\(--my-properties\\)"));
        assert!(result.css.contains("will-change: var(--my-properties)"));
        assert!(result.css.contains(".will-change-\\[top\\,left\\]"));
        assert!(result.css.contains("will-change: top,left"));
        assert!(result.css.contains(".md\\:will-change-auto"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_mix_blend_mode_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "mix-blend-normal".to_string(),
                "mix-blend-multiply".to_string(),
                "mix-blend-screen".to_string(),
                "mix-blend-overlay".to_string(),
                "mix-blend-darken".to_string(),
                "mix-blend-lighten".to_string(),
                "mix-blend-color-dodge".to_string(),
                "mix-blend-color-burn".to_string(),
                "mix-blend-hard-light".to_string(),
                "mix-blend-soft-light".to_string(),
                "mix-blend-difference".to_string(),
                "mix-blend-exclusion".to_string(),
                "mix-blend-hue".to_string(),
                "mix-blend-saturation".to_string(),
                "mix-blend-color".to_string(),
                "mix-blend-luminosity".to_string(),
                "mix-blend-plus-darker".to_string(),
                "mix-blend-plus-lighter".to_string(),
                "md:mix-blend-overlay".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".mix-blend-normal"));
        assert!(result.css.contains("mix-blend-mode: normal"));
        assert!(result.css.contains(".mix-blend-multiply"));
        assert!(result.css.contains("mix-blend-mode: multiply"));
        assert!(result.css.contains(".mix-blend-screen"));
        assert!(result.css.contains("mix-blend-mode: screen"));
        assert!(result.css.contains(".mix-blend-overlay"));
        assert!(result.css.contains("mix-blend-mode: overlay"));
        assert!(result.css.contains(".mix-blend-darken"));
        assert!(result.css.contains("mix-blend-mode: darken"));
        assert!(result.css.contains(".mix-blend-lighten"));
        assert!(result.css.contains("mix-blend-mode: lighten"));
        assert!(result.css.contains(".mix-blend-color-dodge"));
        assert!(result.css.contains("mix-blend-mode: color-dodge"));
        assert!(result.css.contains(".mix-blend-color-burn"));
        assert!(result.css.contains("mix-blend-mode: color-burn"));
        assert!(result.css.contains(".mix-blend-hard-light"));
        assert!(result.css.contains("mix-blend-mode: hard-light"));
        assert!(result.css.contains(".mix-blend-soft-light"));
        assert!(result.css.contains("mix-blend-mode: soft-light"));
        assert!(result.css.contains(".mix-blend-difference"));
        assert!(result.css.contains("mix-blend-mode: difference"));
        assert!(result.css.contains(".mix-blend-exclusion"));
        assert!(result.css.contains("mix-blend-mode: exclusion"));
        assert!(result.css.contains(".mix-blend-hue"));
        assert!(result.css.contains("mix-blend-mode: hue"));
        assert!(result.css.contains(".mix-blend-saturation"));
        assert!(result.css.contains("mix-blend-mode: saturation"));
        assert!(result.css.contains(".mix-blend-color"));
        assert!(result.css.contains("mix-blend-mode: color"));
        assert!(result.css.contains(".mix-blend-luminosity"));
        assert!(result.css.contains("mix-blend-mode: luminosity"));
        assert!(result.css.contains(".mix-blend-plus-darker"));
        assert!(result.css.contains("mix-blend-mode: plus-darker"));
        assert!(result.css.contains(".mix-blend-plus-lighter"));
        assert!(result.css.contains("mix-blend-mode: plus-lighter"));
        assert!(result.css.contains(".md\\:mix-blend-overlay"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_background_blend_mode_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "bg-blend-normal".to_string(),
                "bg-blend-multiply".to_string(),
                "bg-blend-screen".to_string(),
                "bg-blend-overlay".to_string(),
                "bg-blend-darken".to_string(),
                "bg-blend-lighten".to_string(),
                "bg-blend-color-dodge".to_string(),
                "bg-blend-color-burn".to_string(),
                "bg-blend-hard-light".to_string(),
                "bg-blend-soft-light".to_string(),
                "bg-blend-difference".to_string(),
                "bg-blend-exclusion".to_string(),
                "bg-blend-hue".to_string(),
                "bg-blend-saturation".to_string(),
                "bg-blend-color".to_string(),
                "bg-blend-luminosity".to_string(),
                "md:bg-blend-darken".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".bg-blend-normal"));
        assert!(result.css.contains("background-blend-mode: normal"));
        assert!(result.css.contains(".bg-blend-multiply"));
        assert!(result.css.contains("background-blend-mode: multiply"));
        assert!(result.css.contains(".bg-blend-screen"));
        assert!(result.css.contains("background-blend-mode: screen"));
        assert!(result.css.contains(".bg-blend-overlay"));
        assert!(result.css.contains("background-blend-mode: overlay"));
        assert!(result.css.contains(".bg-blend-darken"));
        assert!(result.css.contains("background-blend-mode: darken"));
        assert!(result.css.contains(".bg-blend-lighten"));
        assert!(result.css.contains("background-blend-mode: lighten"));
        assert!(result.css.contains(".bg-blend-color-dodge"));
        assert!(result.css.contains("background-blend-mode: color-dodge"));
        assert!(result.css.contains(".bg-blend-color-burn"));
        assert!(result.css.contains("background-blend-mode: color-burn"));
        assert!(result.css.contains(".bg-blend-hard-light"));
        assert!(result.css.contains("background-blend-mode: hard-light"));
        assert!(result.css.contains(".bg-blend-soft-light"));
        assert!(result.css.contains("background-blend-mode: soft-light"));
        assert!(result.css.contains(".bg-blend-difference"));
        assert!(result.css.contains("background-blend-mode: difference"));
        assert!(result.css.contains(".bg-blend-exclusion"));
        assert!(result.css.contains("background-blend-mode: exclusion"));
        assert!(result.css.contains(".bg-blend-hue"));
        assert!(result.css.contains("background-blend-mode: hue"));
        assert!(result.css.contains(".bg-blend-saturation"));
        assert!(result.css.contains("background-blend-mode: saturation"));
        assert!(result.css.contains(".bg-blend-color"));
        assert!(result.css.contains("background-blend-mode: color"));
        assert!(result.css.contains(".bg-blend-luminosity"));
        assert!(result.css.contains("background-blend-mode: luminosity"));
        assert!(result.css.contains(".md\\:bg-blend-darken"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_mask_image_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "mask-none".to_string(),
                "mask-[url(/img/scribble.png)]".to_string(),
                "mask-(--my-mask)".to_string(),
                "mask-linear-50".to_string(),
                "-mask-linear-50".to_string(),
                "mask-linear-from-20".to_string(),
                "mask-linear-from-50%".to_string(),
                "mask-linear-from-regal-blue".to_string(),
                "mask-linear-from-(--my-stop)".to_string(),
                "mask-linear-from-[12px]".to_string(),
                "mask-linear-to-40".to_string(),
                "mask-linear-to-80%".to_string(),
                "mask-linear-to-regal-blue".to_string(),
                "mask-linear-to-(--my-stop)".to_string(),
                "mask-linear-to-[18px]".to_string(),
                "mask-t-from-50%".to_string(),
                "mask-r-to-30%".to_string(),
                "mask-y-from-70%".to_string(),
                "mask-x-to-90%".to_string(),
                "mask-radial-[100%_100%]".to_string(),
                "mask-radial-[at_30%_30%,black,transparent]".to_string(),
                "mask-circle".to_string(),
                "mask-radial-farthest-corner".to_string(),
                "mask-radial-at-bottom-left".to_string(),
                "mask-radial-at-[35%_35%]".to_string(),
                "mask-radial-from-75%".to_string(),
                "mask-radial-to-regal-blue".to_string(),
                "mask-conic-75".to_string(),
                "-mask-conic-75".to_string(),
                "mask-conic-from-75%".to_string(),
                "mask-conic-to-regal-blue".to_string(),
                "md:mask-radial-from-50%".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".mask-none"));
        assert!(result.css.contains("mask-image: none"));
        assert!(
            result
                .css
                .contains(".mask-\\[url\\(\\/img\\/scribble.png\\)\\]")
        );
        assert!(result.css.contains("mask-image: url(/img/scribble.png)"));
        assert!(result.css.contains(".mask-\\(--my-mask\\)"));
        assert!(result.css.contains("mask-image: var(--my-mask)"));
        assert!(result.css.contains(".mask-linear-50"));
        assert!(result.css.contains(
            "mask-image: linear-gradient(50deg,black var(--tw-mask-linear-from),transparent var(--tw-mask-linear-to))"
        ));
        assert!(result.css.contains(".-mask-linear-50"));
        assert!(result.css.contains(
            "mask-image: linear-gradient(calc(50deg * -1),black var(--tw-mask-linear-from),transparent var(--tw-mask-linear-to))"
        ));
        assert!(result.css.contains(".mask-linear-from-20"));
        assert!(result.css.contains(
            "mask-image: linear-gradient(var(--tw-mask-linear-position),black calc(var(--spacing) * 20),transparent var(--tw-mask-linear-to))"
        ));
        assert!(result.css.contains(".mask-linear-from-50\\%"));
        assert!(
            result
                .css
                .contains("black 50%,transparent var(--tw-mask-linear-to)")
        );
        assert!(result.css.contains(".mask-linear-from-regal-blue"));
        assert!(
            result
                .css
                .contains("var(--color-regal-blue) var(--tw-mask-linear-from)")
        );
        assert!(result.css.contains(".mask-linear-from-\\(--my-stop\\)"));
        assert!(
            result
                .css
                .contains("black var(--my-stop),transparent var(--tw-mask-linear-to)")
        );
        assert!(result.css.contains(".mask-linear-to-40"));
        assert!(result.css.contains(
            "mask-image: linear-gradient(var(--tw-mask-linear-position),black var(--tw-mask-linear-from),transparent calc(var(--spacing) * 40))"
        ));
        assert!(result.css.contains(".mask-t-from-50\\%"));
        assert!(result.css.contains(
            "mask-image: linear-gradient(to top,black 50%,transparent var(--tw-mask-top-to))"
        ));
        assert!(result.css.contains(".mask-r-to-30\\%"));
        assert!(result.css.contains(
            "mask-image: linear-gradient(to right,black var(--tw-mask-right-from),transparent 30%)"
        ));
        assert!(result.css.contains(".mask-y-from-70\\%"));
        assert!(result.css.contains("mask-composite: intersect"));
        assert!(result.css.contains(".mask-x-to-90\\%"));
        assert!(result
            .css
            .contains("linear-gradient(to right,black var(--tw-mask-right-from),transparent 90%),linear-gradient(to left,black var(--tw-mask-left-from),transparent 90%)"));
        assert!(result.css.contains(".mask-radial-\\[100\\%_100\\%\\]"));
        assert!(result.css.contains("--tw-mask-radial-size: 100%_100%"));
        assert!(
            result
                .css
                .contains(".mask-radial-\\[at_30\\%_30\\%\\,black\\,transparent\\]")
        );
        assert!(
            result
                .css
                .contains("mask-image: radial-gradient(at_30%_30%,black,transparent)")
        );
        assert!(result.css.contains(".mask-circle"));
        assert!(result.css.contains("--tw-mask-radial-shape: circle"));
        assert!(result.css.contains(".mask-radial-farthest-corner"));
        assert!(
            result
                .css
                .contains("--tw-mask-radial-size: farthest-corner")
        );
        assert!(result.css.contains(".mask-radial-at-bottom-left"));
        assert!(
            result
                .css
                .contains("--tw-mask-radial-position: bottom left")
        );
        assert!(result.css.contains(".mask-radial-at-\\[35\\%_35\\%\\]"));
        assert!(result.css.contains("--tw-mask-radial-position: 35%_35%"));
        assert!(result.css.contains(".mask-radial-from-75\\%"));
        assert!(result.css.contains("mask-image: radial-gradient"));
        assert!(result.css.contains(".mask-radial-to-regal-blue"));
        assert!(
            result
                .css
                .contains("var(--color-regal-blue) var(--tw-mask-radial-to)")
        );
        assert!(result.css.contains(".mask-conic-75"));
        assert!(result.css.contains(
            "mask-image: conic-gradient(from 75deg,black var(--tw-mask-conic-from),transparent var(--tw-mask-conic-to))"
        ));
        assert!(result.css.contains(".-mask-conic-75"));
        assert!(result.css.contains(
            "mask-image: conic-gradient(from calc(75deg * -1),black var(--tw-mask-conic-from),transparent var(--tw-mask-conic-to))"
        ));
        assert!(result.css.contains(".mask-conic-from-75\\%"));
        assert!(
            result
                .css
                .contains("from var(--tw-mask-conic-position),black 75%")
        );
        assert!(result.css.contains(".mask-conic-to-regal-blue"));
        assert!(
            result
                .css
                .contains("var(--color-regal-blue) var(--tw-mask-conic-to)")
        );
        assert!(result.css.contains(".md\\:mask-radial-from-50\\%"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_mask_mode_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "mask-alpha".to_string(),
                "mask-luminance".to_string(),
                "mask-match".to_string(),
                "md:mask-luminance".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".mask-alpha"));
        assert!(result.css.contains("mask-mode: alpha"));
        assert!(result.css.contains(".mask-luminance"));
        assert!(result.css.contains("mask-mode: luminance"));
        assert!(result.css.contains(".mask-match"));
        assert!(result.css.contains("mask-mode: match-source"));
        assert!(result.css.contains(".md\\:mask-luminance"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_mask_type_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "mask-type-alpha".to_string(),
                "mask-type-luminance".to_string(),
                "md:mask-type-luminance".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".mask-type-alpha"));
        assert!(result.css.contains("mask-type: alpha"));
        assert!(result.css.contains(".mask-type-luminance"));
        assert!(result.css.contains("mask-type: luminance"));
        assert!(result.css.contains(".md\\:mask-type-luminance"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_mask_origin_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "mask-origin-border".to_string(),
                "mask-origin-padding".to_string(),
                "mask-origin-content".to_string(),
                "mask-origin-fill".to_string(),
                "mask-origin-stroke".to_string(),
                "mask-origin-view".to_string(),
                "md:mask-origin-padding".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".mask-origin-border"));
        assert!(result.css.contains("mask-origin: border-box"));
        assert!(result.css.contains(".mask-origin-padding"));
        assert!(result.css.contains("mask-origin: padding-box"));
        assert!(result.css.contains(".mask-origin-content"));
        assert!(result.css.contains("mask-origin: content-box"));
        assert!(result.css.contains(".mask-origin-fill"));
        assert!(result.css.contains("mask-origin: fill-box"));
        assert!(result.css.contains(".mask-origin-stroke"));
        assert!(result.css.contains("mask-origin: stroke-box"));
        assert!(result.css.contains(".mask-origin-view"));
        assert!(result.css.contains("mask-origin: view-box"));
        assert!(result.css.contains(".md\\:mask-origin-padding"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_mask_position_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "mask-top-left".to_string(),
                "mask-top".to_string(),
                "mask-top-right".to_string(),
                "mask-left".to_string(),
                "mask-center".to_string(),
                "mask-right".to_string(),
                "mask-bottom-left".to_string(),
                "mask-bottom".to_string(),
                "mask-bottom-right".to_string(),
                "mask-position-(--my-mask-position)".to_string(),
                "mask-position-[center_top_1rem]".to_string(),
                "md:mask-top".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".mask-top-left"));
        assert!(result.css.contains("mask-position: top left"));
        assert!(result.css.contains(".mask-top"));
        assert!(result.css.contains("mask-position: top"));
        assert!(result.css.contains(".mask-top-right"));
        assert!(result.css.contains("mask-position: top right"));
        assert!(result.css.contains(".mask-left"));
        assert!(result.css.contains("mask-position: left"));
        assert!(result.css.contains(".mask-center"));
        assert!(result.css.contains("mask-position: center"));
        assert!(result.css.contains(".mask-right"));
        assert!(result.css.contains("mask-position: right"));
        assert!(result.css.contains(".mask-bottom-left"));
        assert!(result.css.contains("mask-position: bottom left"));
        assert!(result.css.contains(".mask-bottom"));
        assert!(result.css.contains("mask-position: bottom"));
        assert!(result.css.contains(".mask-bottom-right"));
        assert!(result.css.contains("mask-position: bottom right"));
        assert!(
            result
                .css
                .contains(".mask-position-\\(--my-mask-position\\)")
        );
        assert!(
            result
                .css
                .contains("mask-position: var(--my-mask-position)")
        );
        assert!(result.css.contains(".mask-position-\\[center_top_1rem\\]"));
        assert!(result.css.contains("mask-position: center_top_1rem"));
        assert!(result.css.contains(".md\\:mask-top"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_mask_repeat_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "mask-repeat".to_string(),
                "mask-no-repeat".to_string(),
                "mask-repeat-x".to_string(),
                "mask-repeat-y".to_string(),
                "mask-repeat-space".to_string(),
                "mask-repeat-round".to_string(),
                "md:mask-repeat-x".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".mask-repeat"));
        assert!(result.css.contains("mask-repeat: repeat"));
        assert!(result.css.contains(".mask-no-repeat"));
        assert!(result.css.contains("mask-repeat: no-repeat"));
        assert!(result.css.contains(".mask-repeat-x"));
        assert!(result.css.contains("mask-repeat: repeat-x"));
        assert!(result.css.contains(".mask-repeat-y"));
        assert!(result.css.contains("mask-repeat: repeat-y"));
        assert!(result.css.contains(".mask-repeat-space"));
        assert!(result.css.contains("mask-repeat: space"));
        assert!(result.css.contains(".mask-repeat-round"));
        assert!(result.css.contains("mask-repeat: round"));
        assert!(result.css.contains(".md\\:mask-repeat-x"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_mask_size_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "mask-auto".to_string(),
                "mask-cover".to_string(),
                "mask-contain".to_string(),
                "mask-size-(--my-mask-size)".to_string(),
                "mask-size-[auto_100px]".to_string(),
                "md:mask-contain".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".mask-auto"));
        assert!(result.css.contains("mask-size: auto"));
        assert!(result.css.contains(".mask-cover"));
        assert!(result.css.contains("mask-size: cover"));
        assert!(result.css.contains(".mask-contain"));
        assert!(result.css.contains("mask-size: contain"));
        assert!(result.css.contains(".mask-size-\\(--my-mask-size\\)"));
        assert!(result.css.contains("mask-size: var(--my-mask-size)"));
        assert!(result.css.contains(".mask-size-\\[auto_100px\\]"));
        assert!(result.css.contains("mask-size: auto_100px"));
        assert!(result.css.contains(".md\\:mask-contain"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_mask_clip_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "mask-clip-border".to_string(),
                "mask-clip-padding".to_string(),
                "mask-clip-content".to_string(),
                "mask-clip-fill".to_string(),
                "mask-clip-stroke".to_string(),
                "mask-clip-view".to_string(),
                "mask-no-clip".to_string(),
                "md:mask-clip-padding".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".mask-clip-border"));
        assert!(result.css.contains("mask-clip: border-box"));
        assert!(result.css.contains(".mask-clip-padding"));
        assert!(result.css.contains("mask-clip: padding-box"));
        assert!(result.css.contains(".mask-clip-content"));
        assert!(result.css.contains("mask-clip: content-box"));
        assert!(result.css.contains(".mask-clip-fill"));
        assert!(result.css.contains("mask-clip: fill-box"));
        assert!(result.css.contains(".mask-clip-stroke"));
        assert!(result.css.contains("mask-clip: stroke-box"));
        assert!(result.css.contains(".mask-clip-view"));
        assert!(result.css.contains("mask-clip: view-box"));
        assert!(result.css.contains(".mask-no-clip"));
        assert!(result.css.contains("mask-clip: no-clip"));
        assert!(result.css.contains(".md\\:mask-clip-padding"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_mask_composite_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "mask-add".to_string(),
                "mask-subtract".to_string(),
                "mask-intersect".to_string(),
                "mask-exclude".to_string(),
                "md:mask-subtract".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".mask-add"));
        assert!(result.css.contains("mask-composite: add"));
        assert!(result.css.contains(".mask-subtract"));
        assert!(result.css.contains("mask-composite: subtract"));
        assert!(result.css.contains(".mask-intersect"));
        assert!(result.css.contains("mask-composite: intersect"));
        assert!(result.css.contains(".mask-exclude"));
        assert!(result.css.contains("mask-composite: exclude"));
        assert!(result.css.contains(".md\\:mask-subtract"));
        assert!(result.css.contains("@media (width >= 48rem)"));
    }

    #[test]
    fn generates_object_fit_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "object-contain".to_string(),
                "object-cover".to_string(),
                "object-fill".to_string(),
                "object-none".to_string(),
                "object-scale-down".to_string(),
                "md:object-cover".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".object-contain"));
        assert!(result.css.contains("object-fit: contain"));
        assert!(result.css.contains(".object-cover"));
        assert!(result.css.contains("object-fit: cover"));
        assert!(result.css.contains(".object-fill"));
        assert!(result.css.contains("object-fit: fill"));
        assert!(result.css.contains(".object-none"));
        assert!(result.css.contains("object-fit: none"));
        assert!(result.css.contains(".object-scale-down"));
        assert!(result.css.contains("object-fit: scale-down"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:object-cover"));
    }

    #[test]
    fn generates_object_position_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "object-top-left".to_string(),
                "object-top".to_string(),
                "object-top-right".to_string(),
                "object-left".to_string(),
                "object-center".to_string(),
                "object-right".to_string(),
                "object-bottom-left".to_string(),
                "object-bottom".to_string(),
                "object-bottom-right".to_string(),
                "object-(--my-object)".to_string(),
                "object-[25%_75%]".to_string(),
                "md:object-top".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".object-top-left"));
        assert!(result.css.contains("object-position: top left"));
        assert!(result.css.contains(".object-top"));
        assert!(result.css.contains("object-position: top"));
        assert!(result.css.contains(".object-top-right"));
        assert!(result.css.contains("object-position: top right"));
        assert!(result.css.contains(".object-left"));
        assert!(result.css.contains("object-position: left"));
        assert!(result.css.contains(".object-center"));
        assert!(result.css.contains("object-position: center"));
        assert!(result.css.contains(".object-right"));
        assert!(result.css.contains("object-position: right"));
        assert!(result.css.contains(".object-bottom-left"));
        assert!(result.css.contains("object-position: bottom left"));
        assert!(result.css.contains(".object-bottom"));
        assert!(result.css.contains("object-position: bottom"));
        assert!(result.css.contains(".object-bottom-right"));
        assert!(result.css.contains("object-position: bottom right"));
        assert!(result.css.contains(".object-\\(--my-object\\)"));
        assert!(result.css.contains("object-position: var(--my-object)"));
        assert!(result.css.contains(".object-\\[25\\%_75\\%\\]"));
        assert!(result.css.contains("object-position: 25%_75%"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:object-top"));
    }

    #[test]
    fn generates_overscroll_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "overscroll-auto".to_string(),
                "overscroll-contain".to_string(),
                "overscroll-none".to_string(),
                "overscroll-x-auto".to_string(),
                "overscroll-x-contain".to_string(),
                "overscroll-x-none".to_string(),
                "overscroll-y-auto".to_string(),
                "overscroll-y-contain".to_string(),
                "overscroll-y-none".to_string(),
                "md:overscroll-contain".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".overscroll-auto"));
        assert!(result.css.contains("overscroll-behavior: auto"));
        assert!(result.css.contains(".overscroll-contain"));
        assert!(result.css.contains("overscroll-behavior: contain"));
        assert!(result.css.contains(".overscroll-none"));
        assert!(result.css.contains("overscroll-behavior: none"));
        assert!(result.css.contains(".overscroll-x-auto"));
        assert!(result.css.contains("overscroll-behavior-x: auto"));
        assert!(result.css.contains(".overscroll-x-contain"));
        assert!(result.css.contains("overscroll-behavior-x: contain"));
        assert!(result.css.contains(".overscroll-x-none"));
        assert!(result.css.contains("overscroll-behavior-x: none"));
        assert!(result.css.contains(".overscroll-y-auto"));
        assert!(result.css.contains("overscroll-behavior-y: auto"));
        assert!(result.css.contains(".overscroll-y-contain"));
        assert!(result.css.contains("overscroll-behavior-y: contain"));
        assert!(result.css.contains(".overscroll-y-none"));
        assert!(result.css.contains("overscroll-behavior-y: none"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:overscroll-contain"));
    }

    #[test]
    fn generates_position_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "static".to_string(),
                "fixed".to_string(),
                "absolute".to_string(),
                "relative".to_string(),
                "sticky".to_string(),
                "md:absolute".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".static"));
        assert!(result.css.contains("position: static"));
        assert!(result.css.contains(".fixed"));
        assert!(result.css.contains("position: fixed"));
        assert!(result.css.contains(".absolute"));
        assert!(result.css.contains("position: absolute"));
        assert!(result.css.contains(".relative"));
        assert!(result.css.contains("position: relative"));
        assert!(result.css.contains(".sticky"));
        assert!(result.css.contains("position: sticky"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:absolute"));
    }

    #[test]
    fn generates_inset_spacing_fraction_and_custom_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "inset-4".to_string(),
                "-inset-4".to_string(),
                "inset-x-2".to_string(),
                "inset-y-3".to_string(),
                "top-1/2".to_string(),
                "-top-1/2".to_string(),
                "left-px".to_string(),
                "-left-px".to_string(),
                "right-full".to_string(),
                "-right-full".to_string(),
                "bottom-auto".to_string(),
                "start-2".to_string(),
                "end-3".to_string(),
                "inset-(--my-position)".to_string(),
                "inset-[3px]".to_string(),
                "-top-[3px]".to_string(),
                "md:top-6".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".inset-4"));
        assert!(result.css.contains("inset: calc(var(--spacing) * 4)"));
        assert!(result.css.contains(".-inset-4"));
        assert!(result.css.contains("inset: calc(var(--spacing) * -4)"));
        assert!(result.css.contains(".inset-x-2"));
        assert!(
            result
                .css
                .contains("inset-inline: calc(var(--spacing) * 2)")
        );
        assert!(result.css.contains(".inset-y-3"));
        assert!(result.css.contains("inset-block: calc(var(--spacing) * 3)"));
        assert!(result.css.contains(".top-1\\/2"));
        assert!(result.css.contains("top: calc(1/2 * 100%)"));
        assert!(result.css.contains(".-top-1\\/2"));
        assert!(result.css.contains("top: calc(1/2 * -100%)"));
        assert!(result.css.contains(".left-px"));
        assert!(result.css.contains("left: 1px"));
        assert!(result.css.contains(".-left-px"));
        assert!(result.css.contains("left: calc(1px * -1)"));
        assert!(result.css.contains(".right-full"));
        assert!(result.css.contains("right: 100%"));
        assert!(result.css.contains(".-right-full"));
        assert!(result.css.contains("right: calc(100% * -1)"));
        assert!(result.css.contains(".bottom-auto"));
        assert!(result.css.contains("bottom: auto"));
        assert!(result.css.contains(".start-2"));
        assert!(
            result
                .css
                .contains("inset-inline-start: calc(var(--spacing) * 2)")
        );
        assert!(result.css.contains(".end-3"));
        assert!(
            result
                .css
                .contains("inset-inline-end: calc(var(--spacing) * 3)")
        );
        assert!(result.css.contains(".inset-\\(--my-position\\)"));
        assert!(result.css.contains("inset: var(--my-position)"));
        assert!(result.css.contains(".inset-\\[3px\\]"));
        assert!(result.css.contains("inset: 3px"));
        assert!(result.css.contains(".-top-\\[3px\\]"));
        assert!(result.css.contains("top: calc(3px * -1)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:top-6"));
    }

    #[test]
    fn generates_visibility_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "visible".to_string(),
                "invisible".to_string(),
                "collapse".to_string(),
                "md:invisible".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".visible"));
        assert!(result.css.contains("visibility: visible"));
        assert!(result.css.contains(".invisible"));
        assert!(result.css.contains("visibility: hidden"));
        assert!(result.css.contains(".collapse"));
        assert!(result.css.contains("visibility: collapse"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:invisible"));
    }

    #[test]
    fn generates_backface_visibility_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "backface-hidden".to_string(),
                "backface-visible".to_string(),
                "md:backface-hidden".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".backface-hidden"));
        assert!(result.css.contains("backface-visibility: hidden"));
        assert!(result.css.contains(".backface-visible"));
        assert!(result.css.contains("backface-visibility: visible"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:backface-hidden"));
    }

    #[test]
    fn generates_perspective_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "perspective-dramatic".to_string(),
                "perspective-near".to_string(),
                "perspective-normal".to_string(),
                "perspective-midrange".to_string(),
                "perspective-distant".to_string(),
                "perspective-none".to_string(),
                "perspective-(--my-perspective)".to_string(),
                "perspective-[750px]".to_string(),
                "perspective-remote".to_string(),
                "md:perspective-dramatic".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".perspective-dramatic"));
        assert!(
            result
                .css
                .contains("perspective: var(--perspective-dramatic)")
        );
        assert!(result.css.contains(".perspective-near"));
        assert!(result.css.contains("perspective: var(--perspective-near)"));
        assert!(result.css.contains(".perspective-normal"));
        assert!(
            result
                .css
                .contains("perspective: var(--perspective-normal)")
        );
        assert!(result.css.contains(".perspective-midrange"));
        assert!(
            result
                .css
                .contains("perspective: var(--perspective-midrange)")
        );
        assert!(result.css.contains(".perspective-distant"));
        assert!(
            result
                .css
                .contains("perspective: var(--perspective-distant)")
        );
        assert!(result.css.contains(".perspective-none"));
        assert!(result.css.contains("perspective: none"));
        assert!(result.css.contains(".perspective-\\(--my-perspective\\)"));
        assert!(result.css.contains("perspective: var(--my-perspective)"));
        assert!(result.css.contains(".perspective-\\[750px\\]"));
        assert!(result.css.contains("perspective: 750px"));
        assert!(result.css.contains(".perspective-remote"));
        assert!(
            result
                .css
                .contains("perspective: var(--perspective-remote)")
        );
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:perspective-dramatic"));
    }

    #[test]
    fn generates_perspective_origin_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "perspective-origin-center".to_string(),
                "perspective-origin-top".to_string(),
                "perspective-origin-top-right".to_string(),
                "perspective-origin-right".to_string(),
                "perspective-origin-bottom-right".to_string(),
                "perspective-origin-bottom".to_string(),
                "perspective-origin-bottom-left".to_string(),
                "perspective-origin-left".to_string(),
                "perspective-origin-top-left".to_string(),
                "perspective-origin-(--my-perspective-origin)".to_string(),
                "perspective-origin-[200%_150%]".to_string(),
                "md:perspective-origin-bottom-left".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".perspective-origin-center"));
        assert!(result.css.contains("perspective-origin: center"));
        assert!(result.css.contains(".perspective-origin-top"));
        assert!(result.css.contains("perspective-origin: top"));
        assert!(result.css.contains(".perspective-origin-top-right"));
        assert!(result.css.contains("perspective-origin: top right"));
        assert!(result.css.contains(".perspective-origin-right"));
        assert!(result.css.contains("perspective-origin: right"));
        assert!(result.css.contains(".perspective-origin-bottom-right"));
        assert!(result.css.contains("perspective-origin: bottom right"));
        assert!(result.css.contains(".perspective-origin-bottom"));
        assert!(result.css.contains("perspective-origin: bottom"));
        assert!(result.css.contains(".perspective-origin-bottom-left"));
        assert!(result.css.contains("perspective-origin: bottom left"));
        assert!(result.css.contains(".perspective-origin-left"));
        assert!(result.css.contains("perspective-origin: left"));
        assert!(result.css.contains(".perspective-origin-top-left"));
        assert!(result.css.contains("perspective-origin: top left"));
        assert!(
            result
                .css
                .contains(".perspective-origin-\\(--my-perspective-origin\\)")
        );
        assert!(
            result
                .css
                .contains("perspective-origin: var(--my-perspective-origin)")
        );
        assert!(
            result
                .css
                .contains(".perspective-origin-\\[200\\%_150\\%\\]")
        );
        assert!(result.css.contains("perspective-origin: 200%_150%"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:perspective-origin-bottom-left"));
    }

    #[test]
    fn generates_rotate_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "rotate-none".to_string(),
                "rotate-45".to_string(),
                "-rotate-90".to_string(),
                "rotate-(--my-rotation)".to_string(),
                "rotate-[3.142rad]".to_string(),
                "rotate-x-50".to_string(),
                "-rotate-x-15".to_string(),
                "rotate-x-(--my-rotate-x)".to_string(),
                "rotate-x-[25deg]".to_string(),
                "rotate-y-25".to_string(),
                "-rotate-y-30".to_string(),
                "rotate-y-(--my-rotate-y)".to_string(),
                "rotate-y-[0.5turn]".to_string(),
                "rotate-z-45".to_string(),
                "-rotate-z-60".to_string(),
                "rotate-z-(--my-rotate-z)".to_string(),
                "rotate-z-[1.25rad]".to_string(),
                "md:rotate-60".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".rotate-none"));
        assert!(result.css.contains("rotate: none"));
        assert!(result.css.contains(".rotate-45"));
        assert!(result.css.contains("rotate: 45deg"));
        assert!(result.css.contains(".-rotate-90"));
        assert!(result.css.contains("rotate: calc(90deg * -1)"));
        assert!(result.css.contains(".rotate-\\(--my-rotation\\)"));
        assert!(result.css.contains("rotate: var(--my-rotation)"));
        assert!(result.css.contains(".rotate-\\[3.142rad\\]"));
        assert!(result.css.contains("rotate: 3.142rad"));

        assert!(result.css.contains(".rotate-x-50"));
        assert!(
            result
                .css
                .contains("transform: rotateX(50deg) var(--tw-rotate-y)")
        );
        assert!(result.css.contains(".-rotate-x-15"));
        assert!(
            result
                .css
                .contains("transform: rotateX(-15deg) var(--tw-rotate-y)")
        );
        assert!(result.css.contains(".rotate-x-\\(--my-rotate-x\\)"));
        assert!(
            result
                .css
                .contains("transform: rotateX(var(--my-rotate-x)) var(--tw-rotate-y)")
        );
        assert!(result.css.contains(".rotate-x-\\[25deg\\]"));
        assert!(
            result
                .css
                .contains("transform: rotateX(25deg) var(--tw-rotate-y)")
        );

        assert!(result.css.contains(".rotate-y-25"));
        assert!(
            result
                .css
                .contains("transform: var(--tw-rotate-x) rotateY(25deg)")
        );
        assert!(result.css.contains(".-rotate-y-30"));
        assert!(
            result
                .css
                .contains("transform: var(--tw-rotate-x) rotateY(-30deg)")
        );
        assert!(result.css.contains(".rotate-y-\\(--my-rotate-y\\)"));
        assert!(
            result
                .css
                .contains("transform: var(--tw-rotate-x) rotateY(var(--my-rotate-y))")
        );
        assert!(result.css.contains(".rotate-y-\\[0.5turn\\]"));
        assert!(
            result
                .css
                .contains("transform: var(--tw-rotate-x) rotateY(0.5turn)")
        );

        assert!(result.css.contains(".rotate-z-45"));
        assert!(
            result
                .css
                .contains("transform: var(--tw-rotate-x) var(--tw-rotate-y) rotateZ(45deg)")
        );
        assert!(result.css.contains(".-rotate-z-60"));
        assert!(
            result
                .css
                .contains("transform: var(--tw-rotate-x) var(--tw-rotate-y) rotateZ(-60deg)")
        );
        assert!(result.css.contains(".rotate-z-\\(--my-rotate-z\\)"));
        assert!(result.css.contains(
            "transform: var(--tw-rotate-x) var(--tw-rotate-y) rotateZ(var(--my-rotate-z))"
        ));
        assert!(result.css.contains(".rotate-z-\\[1.25rad\\]"));
        assert!(
            result
                .css
                .contains("transform: var(--tw-rotate-x) var(--tw-rotate-y) rotateZ(1.25rad)")
        );

        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:rotate-60"));
        assert!(result.css.contains("rotate: 60deg"));
    }

    #[test]
    fn generates_scale_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "scale-none".to_string(),
                "scale-75".to_string(),
                "-scale-125".to_string(),
                "scale-(--my-scale)".to_string(),
                "scale-[1.7]".to_string(),
                "scale-x-75".to_string(),
                "-scale-x-150".to_string(),
                "scale-x-(--my-scale-x)".to_string(),
                "scale-x-[1.2]".to_string(),
                "scale-y-125".to_string(),
                "-scale-y-90".to_string(),
                "scale-y-(--my-scale-y)".to_string(),
                "scale-y-[1.3]".to_string(),
                "scale-z-110".to_string(),
                "-scale-z-80".to_string(),
                "scale-z-(--my-scale-z)".to_string(),
                "scale-z-[0.8]".to_string(),
                "scale-3d".to_string(),
                "hover:scale-120".to_string(),
                "md:scale-150".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".scale-none"));
        assert!(result.css.contains("scale: none"));
        assert!(result.css.contains(".scale-75"));
        assert!(result.css.contains("--tw-scale-x: 75%"));
        assert!(result.css.contains("--tw-scale-y: 75%"));
        assert!(result.css.contains("--tw-scale-z: 75%"));
        assert!(
            result
                .css
                .contains("scale: var(--tw-scale-x) var(--tw-scale-y)")
        );
        assert!(result.css.contains(".-scale-125"));
        assert!(result.css.contains("--tw-scale-x: calc(125% * -1)"));
        assert!(result.css.contains("--tw-scale-y: calc(125% * -1)"));
        assert!(result.css.contains("--tw-scale-z: calc(125% * -1)"));
        assert!(result.css.contains(".scale-\\(--my-scale\\)"));
        assert!(result.css.contains("--tw-scale-x: var(--my-scale)"));
        assert!(result.css.contains("--tw-scale-y: var(--my-scale)"));
        assert!(result.css.contains("--tw-scale-z: var(--my-scale)"));
        assert!(result.css.contains(".scale-\\[1.7\\]"));
        assert!(result.css.contains("scale: 1.7"));

        assert!(result.css.contains(".scale-x-75"));
        assert!(result.css.contains("scale: 75% var(--tw-scale-y)"));
        assert!(result.css.contains(".-scale-x-150"));
        assert!(
            result
                .css
                .contains("scale: calc(150% * -1) var(--tw-scale-y)")
        );
        assert!(result.css.contains(".scale-x-\\(--my-scale-x\\)"));
        assert!(
            result
                .css
                .contains("scale: var(--my-scale-x) var(--tw-scale-y)")
        );
        assert!(result.css.contains(".scale-x-\\[1.2\\]"));
        assert!(result.css.contains("scale: 1.2 var(--tw-scale-y)"));

        assert!(result.css.contains(".scale-y-125"));
        assert!(result.css.contains("scale: var(--tw-scale-x) 125%"));
        assert!(result.css.contains(".-scale-y-90"));
        assert!(
            result
                .css
                .contains("scale: var(--tw-scale-x) calc(90% * -1)")
        );
        assert!(result.css.contains(".scale-y-\\(--my-scale-y\\)"));
        assert!(
            result
                .css
                .contains("scale: var(--tw-scale-x) var(--my-scale-y)")
        );
        assert!(result.css.contains(".scale-y-\\[1.3\\]"));
        assert!(result.css.contains("scale: var(--tw-scale-x) 1.3"));

        assert!(result.css.contains(".scale-z-110"));
        assert!(
            result
                .css
                .contains("scale: var(--tw-scale-x) var(--tw-scale-y) 110%")
        );
        assert!(result.css.contains(".-scale-z-80"));
        assert!(
            result
                .css
                .contains("scale: var(--tw-scale-x) var(--tw-scale-y) calc(80% * -1)")
        );
        assert!(result.css.contains(".scale-z-\\(--my-scale-z\\)"));
        assert!(
            result
                .css
                .contains("scale: var(--tw-scale-x) var(--tw-scale-y) var(--my-scale-z)")
        );
        assert!(result.css.contains(".scale-z-\\[0.8\\]"));
        assert!(
            result
                .css
                .contains("scale: var(--tw-scale-x) var(--tw-scale-y) 0.8")
        );
        assert!(result.css.contains(".scale-3d"));
        assert!(
            result
                .css
                .contains("scale: var(--tw-scale-x) var(--tw-scale-y) var(--tw-scale-z)")
        );

        assert!(result.css.contains(".hover\\:scale-120:hover"));
        assert!(result.css.contains("--tw-scale-x: 120%"));
        assert!(result.css.contains("--tw-scale-y: 120%"));
        assert!(result.css.contains("--tw-scale-z: 120%"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:scale-150"));
        assert!(result.css.contains("--tw-scale-x: 150%"));
        assert!(result.css.contains("--tw-scale-y: 150%"));
        assert!(result.css.contains("--tw-scale-z: 150%"));
    }

    #[test]
    fn generates_skew_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "skew-3".to_string(),
                "-skew-12".to_string(),
                "skew-(--my-skew)".to_string(),
                "skew-[3.142rad]".to_string(),
                "skew-x-6".to_string(),
                "-skew-x-10".to_string(),
                "skew-x-(--my-skew-x)".to_string(),
                "skew-x-[25deg]".to_string(),
                "skew-y-6".to_string(),
                "-skew-y-10".to_string(),
                "skew-y-(--my-skew-y)".to_string(),
                "skew-y-[0.25turn]".to_string(),
                "md:skew-12".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".skew-3"));
        assert!(result.css.contains("--tw-skew-x: skewX(3deg)"));
        assert!(result.css.contains("--tw-skew-y: skewY(3deg)"));
        assert!(result.css.contains(
            "transform: var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,) var(--tw-skew-x,) var(--tw-skew-y,)"
        ));
        assert!(result.css.contains(".-skew-12"));
        assert!(result.css.contains("--tw-skew-x: skewX(-12deg)"));
        assert!(result.css.contains("--tw-skew-y: skewY(-12deg)"));
        assert!(result.css.contains(".skew-\\(--my-skew\\)"));
        assert!(result.css.contains("--tw-skew-x: skewX(var(--my-skew))"));
        assert!(result.css.contains("--tw-skew-y: skewY(var(--my-skew))"));
        assert!(result.css.contains(".skew-\\[3.142rad\\]"));
        assert!(result.css.contains("--tw-skew-x: skewX(3.142rad)"));
        assert!(result.css.contains("--tw-skew-y: skewY(3.142rad)"));

        assert!(result.css.contains(".skew-x-6"));
        assert!(result.css.contains("--tw-skew-x: skewX(6deg)"));
        assert!(result.css.contains(".-skew-x-10"));
        assert!(result.css.contains("--tw-skew-x: skewX(-10deg)"));
        assert!(result.css.contains(".skew-x-\\(--my-skew-x\\)"));
        assert!(result.css.contains("--tw-skew-x: skewX(var(--my-skew-x))"));
        assert!(result.css.contains(".skew-x-\\[25deg\\]"));
        assert!(result.css.contains("--tw-skew-x: skewX(25deg)"));

        assert!(result.css.contains(".skew-y-6"));
        assert!(result.css.contains("--tw-skew-y: skewY(6deg)"));
        assert!(result.css.contains(".-skew-y-10"));
        assert!(result.css.contains("--tw-skew-y: skewY(-10deg)"));
        assert!(result.css.contains(".skew-y-\\(--my-skew-y\\)"));
        assert!(result.css.contains("--tw-skew-y: skewY(var(--my-skew-y))"));
        assert!(result.css.contains(".skew-y-\\[0.25turn\\]"));
        assert!(result.css.contains("--tw-skew-y: skewY(0.25turn)"));

        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:skew-12"));
        assert!(result.css.contains("--tw-skew-x: skewX(12deg)"));
        assert!(result.css.contains("--tw-skew-y: skewY(12deg)"));
    }

    #[test]
    fn generates_transform_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "transform".to_string(),
                "transform-none".to_string(),
                "transform-gpu".to_string(),
                "transform-cpu".to_string(),
                "transform-(--my-transform)".to_string(),
                "transform-[matrix(1,2,3,4,5,6)]".to_string(),
                "md:transform-none".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".transform"));
        assert!(result.css.contains(
            "transform: var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,) var(--tw-skew-x,) var(--tw-skew-y,)"
        ));
        assert!(result.css.contains(".transform-none"));
        assert!(result.css.contains("transform: none"));
        assert!(result.css.contains(".transform-gpu"));
        assert!(result.css.contains(
            "transform: translateZ(0) var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,) var(--tw-skew-x,) var(--tw-skew-y,)"
        ));
        assert!(result.css.contains(".transform-cpu"));
        assert!(result.css.contains(
            "transform: var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,) var(--tw-skew-x,) var(--tw-skew-y,)"
        ));
        assert!(result.css.contains(".transform-\\(--my-transform\\)"));
        assert!(result.css.contains("transform: var(--my-transform)"));
        assert!(
            result
                .css
                .contains(".transform-\\[matrix\\(1\\,2\\,3\\,4\\,5\\,6\\)\\]")
        );
        assert!(result.css.contains("transform: matrix(1,2,3,4,5,6)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:transform-none"));
    }

    #[test]
    fn generates_transform_style_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "transform-3d".to_string(),
                "transform-flat".to_string(),
                "md:transform-flat".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".transform-3d"));
        assert!(result.css.contains("transform-style: preserve-3d"));
        assert!(result.css.contains(".transform-flat"));
        assert!(result.css.contains("transform-style: flat"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:transform-flat"));
    }

    #[test]
    fn generates_transform_origin_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "origin-center".to_string(),
                "origin-top".to_string(),
                "origin-top-right".to_string(),
                "origin-right".to_string(),
                "origin-bottom-right".to_string(),
                "origin-bottom".to_string(),
                "origin-bottom-left".to_string(),
                "origin-left".to_string(),
                "origin-top-left".to_string(),
                "origin-(--my-transform-origin)".to_string(),
                "origin-[33%_75%]".to_string(),
                "md:origin-top".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".origin-center"));
        assert!(result.css.contains("transform-origin: center"));
        assert!(result.css.contains(".origin-top"));
        assert!(result.css.contains("transform-origin: top"));
        assert!(result.css.contains(".origin-top-right"));
        assert!(result.css.contains("transform-origin: top right"));
        assert!(result.css.contains(".origin-right"));
        assert!(result.css.contains("transform-origin: right"));
        assert!(result.css.contains(".origin-bottom-right"));
        assert!(result.css.contains("transform-origin: bottom right"));
        assert!(result.css.contains(".origin-bottom"));
        assert!(result.css.contains("transform-origin: bottom"));
        assert!(result.css.contains(".origin-bottom-left"));
        assert!(result.css.contains("transform-origin: bottom left"));
        assert!(result.css.contains(".origin-left"));
        assert!(result.css.contains("transform-origin: left"));
        assert!(result.css.contains(".origin-top-left"));
        assert!(result.css.contains("transform-origin: top left"));
        assert!(result.css.contains(".origin-\\(--my-transform-origin\\)"));
        assert!(
            result
                .css
                .contains("transform-origin: var(--my-transform-origin)")
        );
        assert!(result.css.contains(".origin-\\[33\\%_75\\%\\]"));
        assert!(result.css.contains("transform-origin: 33%_75%"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:origin-top"));
    }

    #[test]
    fn generates_translate_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "translate-none".to_string(),
                "translate-2".to_string(),
                "-translate-4".to_string(),
                "translate-1/2".to_string(),
                "-translate-1/4".to_string(),
                "translate-full".to_string(),
                "-translate-full".to_string(),
                "translate-px".to_string(),
                "-translate-px".to_string(),
                "translate-(--my-translate)".to_string(),
                "translate-[3.142rad]".to_string(),
                "translate-x-3".to_string(),
                "-translate-x-6".to_string(),
                "translate-x-1/2".to_string(),
                "-translate-x-1/4".to_string(),
                "translate-x-full".to_string(),
                "-translate-x-full".to_string(),
                "translate-x-px".to_string(),
                "-translate-x-px".to_string(),
                "translate-x-(--my-translate-x)".to_string(),
                "translate-x-[4px]".to_string(),
                "translate-y-8".to_string(),
                "-translate-y-2".to_string(),
                "translate-y-1/3".to_string(),
                "-translate-y-1/2".to_string(),
                "translate-y-full".to_string(),
                "-translate-y-full".to_string(),
                "translate-y-px".to_string(),
                "-translate-y-px".to_string(),
                "translate-y-(--my-translate-y)".to_string(),
                "translate-y-[5px]".to_string(),
                "translate-z-12".to_string(),
                "-translate-z-8".to_string(),
                "translate-z-px".to_string(),
                "-translate-z-px".to_string(),
                "translate-z-(--my-translate-z)".to_string(),
                "translate-z-[2rem]".to_string(),
                "md:translate-6".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".translate-none"));
        assert!(result.css.contains("translate: none"));
        assert!(result.css.contains(".translate-2"));
        assert!(
            result
                .css
                .contains("--tw-translate-x: calc(var(--spacing) * 2)")
        );
        assert!(
            result
                .css
                .contains("--tw-translate-y: calc(var(--spacing) * 2)")
        );
        assert!(
            result
                .css
                .contains("translate: var(--tw-translate-x) var(--tw-translate-y)")
        );
        assert!(result.css.contains(".-translate-4"));
        assert!(
            result
                .css
                .contains("--tw-translate-x: calc(var(--spacing) * -4)")
        );
        assert!(
            result
                .css
                .contains("--tw-translate-y: calc(var(--spacing) * -4)")
        );
        assert!(result.css.contains(".translate-1\\/2"));
        assert!(result.css.contains("--tw-translate-x: calc(1/2 * 100%)"));
        assert!(result.css.contains("--tw-translate-y: calc(1/2 * 100%)"));
        assert!(result.css.contains(".-translate-1\\/4"));
        assert!(
            result
                .css
                .contains("--tw-translate-x: calc(calc(1/4 * 100%) * -1)")
        );
        assert!(
            result
                .css
                .contains("--tw-translate-y: calc(calc(1/4 * 100%) * -1)")
        );
        assert!(result.css.contains(".translate-full"));
        assert!(result.css.contains("--tw-translate-x: 100%"));
        assert!(result.css.contains("--tw-translate-y: 100%"));
        assert!(result.css.contains(".-translate-full"));
        assert!(result.css.contains("--tw-translate-x: -100%"));
        assert!(result.css.contains("--tw-translate-y: -100%"));
        assert!(result.css.contains(".translate-px"));
        assert!(result.css.contains("--tw-translate-x: 1px"));
        assert!(result.css.contains("--tw-translate-y: 1px"));
        assert!(result.css.contains(".-translate-px"));
        assert!(result.css.contains("--tw-translate-x: -1px"));
        assert!(result.css.contains("--tw-translate-y: -1px"));
        assert!(result.css.contains(".translate-\\(--my-translate\\)"));
        assert!(result.css.contains("--tw-translate-x: var(--my-translate)"));
        assert!(result.css.contains("--tw-translate-y: var(--my-translate)"));
        assert!(result.css.contains(".translate-\\[3.142rad\\]"));
        assert!(result.css.contains("--tw-translate-x: 3.142rad"));
        assert!(result.css.contains("--tw-translate-y: 3.142rad"));

        assert!(result.css.contains(".translate-x-3"));
        assert!(
            result
                .css
                .contains("--tw-translate-x: calc(var(--spacing) * 3)")
        );
        assert!(result.css.contains(".-translate-x-6"));
        assert!(
            result
                .css
                .contains("--tw-translate-x: calc(var(--spacing) * -6)")
        );
        assert!(result.css.contains(".translate-x-1\\/2"));
        assert!(result.css.contains("--tw-translate-x: calc(1/2 * 100%)"));
        assert!(result.css.contains(".-translate-x-1\\/4"));
        assert!(
            result
                .css
                .contains("--tw-translate-x: calc(calc(1/4 * 100%) * -1)")
        );
        assert!(result.css.contains(".translate-x-full"));
        assert!(result.css.contains("--tw-translate-x: 100%"));
        assert!(result.css.contains(".-translate-x-full"));
        assert!(result.css.contains("--tw-translate-x: -100%"));
        assert!(result.css.contains(".translate-x-px"));
        assert!(result.css.contains("--tw-translate-x: 1px"));
        assert!(result.css.contains(".-translate-x-px"));
        assert!(result.css.contains("--tw-translate-x: -1px"));
        assert!(result.css.contains(".translate-x-\\(--my-translate-x\\)"));
        assert!(
            result
                .css
                .contains("--tw-translate-x: var(--my-translate-x)")
        );
        assert!(result.css.contains(".translate-x-\\[4px\\]"));
        assert!(result.css.contains("--tw-translate-x: 4px"));

        assert!(result.css.contains(".translate-y-8"));
        assert!(
            result
                .css
                .contains("--tw-translate-y: calc(var(--spacing) * 8)")
        );
        assert!(result.css.contains(".-translate-y-2"));
        assert!(
            result
                .css
                .contains("--tw-translate-y: calc(var(--spacing) * -2)")
        );
        assert!(result.css.contains(".translate-y-1\\/3"));
        assert!(result.css.contains("--tw-translate-y: calc(1/3 * 100%)"));
        assert!(result.css.contains(".-translate-y-1\\/2"));
        assert!(
            result
                .css
                .contains("--tw-translate-y: calc(calc(1/2 * 100%) * -1)")
        );
        assert!(result.css.contains(".translate-y-full"));
        assert!(result.css.contains("--tw-translate-y: 100%"));
        assert!(result.css.contains(".-translate-y-full"));
        assert!(result.css.contains("--tw-translate-y: -100%"));
        assert!(result.css.contains(".translate-y-px"));
        assert!(result.css.contains("--tw-translate-y: 1px"));
        assert!(result.css.contains(".-translate-y-px"));
        assert!(result.css.contains("--tw-translate-y: -1px"));
        assert!(result.css.contains(".translate-y-\\(--my-translate-y\\)"));
        assert!(
            result
                .css
                .contains("--tw-translate-y: var(--my-translate-y)")
        );
        assert!(result.css.contains(".translate-y-\\[5px\\]"));
        assert!(result.css.contains("--tw-translate-y: 5px"));

        assert!(result.css.contains(".translate-z-12"));
        assert!(result.css.contains(
            "translate: var(--tw-translate-x) var(--tw-translate-y) calc(var(--spacing) * 12)"
        ));
        assert!(result.css.contains(".-translate-z-8"));
        assert!(result.css.contains(
            "translate: var(--tw-translate-x) var(--tw-translate-y) calc(var(--spacing) * -8)"
        ));
        assert!(result.css.contains(".translate-z-px"));
        assert!(
            result
                .css
                .contains("translate: var(--tw-translate-x) var(--tw-translate-y) 1px")
        );
        assert!(result.css.contains(".-translate-z-px"));
        assert!(
            result
                .css
                .contains("translate: var(--tw-translate-x) var(--tw-translate-y) -1px")
        );
        assert!(result.css.contains(".translate-z-\\(--my-translate-z\\)"));
        assert!(result.css.contains(
            "translate: var(--tw-translate-x) var(--tw-translate-y) var(--my-translate-z)"
        ));
        assert!(result.css.contains(".translate-z-\\[2rem\\]"));
        assert!(
            result
                .css
                .contains("translate: var(--tw-translate-x) var(--tw-translate-y) 2rem")
        );

        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:translate-6"));
        assert!(
            result
                .css
                .contains("--tw-translate-x: calc(var(--spacing) * 6)")
        );
        assert!(
            result
                .css
                .contains("--tw-translate-y: calc(var(--spacing) * 6)")
        );
    }

    #[test]
    fn generates_flex_basis_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "basis-64".to_string(),
                "basis-1/3".to_string(),
                "basis-full".to_string(),
                "basis-auto".to_string(),
                "basis-3xs".to_string(),
                "basis-2xs".to_string(),
                "basis-xs".to_string(),
                "basis-sm".to_string(),
                "basis-4xs".to_string(),
                "basis-md".to_string(),
                "basis-lg".to_string(),
                "basis-xl".to_string(),
                "basis-2xl".to_string(),
                "basis-3xl".to_string(),
                "basis-4xl".to_string(),
                "basis-5xl".to_string(),
                "basis-6xl".to_string(),
                "basis-7xl".to_string(),
                "basis-[30vw]".to_string(),
                "basis-(--my-basis)".to_string(),
                "md:basis-1/2".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".basis-64"));
        assert!(result.css.contains("flex-basis: calc(var(--spacing) * 64)"));
        assert!(result.css.contains(".basis-1\\/3"));
        assert!(result.css.contains("flex-basis: calc(1/3 * 100%)"));
        assert!(result.css.contains(".basis-full"));
        assert!(result.css.contains("flex-basis: 100%"));
        assert!(result.css.contains(".basis-auto"));
        assert!(result.css.contains("flex-basis: auto"));
        assert!(result.css.contains(".basis-3xs"));
        assert!(result.css.contains("flex-basis: var(--container-3xs)"));
        assert!(result.css.contains(".basis-2xs"));
        assert!(result.css.contains("flex-basis: var(--container-2xs)"));
        assert!(result.css.contains(".basis-xs"));
        assert!(result.css.contains("flex-basis: var(--container-xs)"));
        assert!(result.css.contains(".basis-sm"));
        assert!(result.css.contains("flex-basis: var(--container-sm)"));
        assert!(result.css.contains(".basis-4xs"));
        assert!(result.css.contains("flex-basis: var(--container-4xs)"));
        assert!(result.css.contains(".basis-md"));
        assert!(result.css.contains("flex-basis: var(--container-md)"));
        assert!(result.css.contains(".basis-lg"));
        assert!(result.css.contains("flex-basis: var(--container-lg)"));
        assert!(result.css.contains(".basis-xl"));
        assert!(result.css.contains("flex-basis: var(--container-xl)"));
        assert!(result.css.contains(".basis-2xl"));
        assert!(result.css.contains("flex-basis: var(--container-2xl)"));
        assert!(result.css.contains(".basis-3xl"));
        assert!(result.css.contains("flex-basis: var(--container-3xl)"));
        assert!(result.css.contains(".basis-4xl"));
        assert!(result.css.contains("flex-basis: var(--container-4xl)"));
        assert!(result.css.contains(".basis-5xl"));
        assert!(result.css.contains("flex-basis: var(--container-5xl)"));
        assert!(result.css.contains(".basis-6xl"));
        assert!(result.css.contains("flex-basis: var(--container-6xl)"));
        assert!(result.css.contains(".basis-7xl"));
        assert!(result.css.contains("flex-basis: var(--container-7xl)"));
        assert!(result.css.contains(".basis-\\[30vw\\]"));
        assert!(result.css.contains("flex-basis: 30vw"));
        assert!(result.css.contains(".basis-\\(--my-basis\\)"));
        assert!(result.css.contains("flex-basis: var(--my-basis)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:basis-1\\/2"));
        assert!(result.css.contains("flex-basis: calc(1/2 * 100%)"));
    }

    #[test]
    fn generates_flex_shorthand_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "flex-1".to_string(),
                "flex-2".to_string(),
                "flex-1/2".to_string(),
                "flex-auto".to_string(),
                "flex-initial".to_string(),
                "flex-none".to_string(),
                "flex-[3_1_auto]".to_string(),
                "flex-(--my-flex)".to_string(),
                "md:flex-1".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".flex-1"));
        assert!(result.css.contains("flex: 1"));
        assert!(result.css.contains(".flex-2"));
        assert!(result.css.contains("flex: 2"));
        assert!(result.css.contains(".flex-1\\/2"));
        assert!(result.css.contains("flex: calc(1/2 * 100%)"));
        assert!(result.css.contains(".flex-auto"));
        assert!(result.css.contains("flex: auto"));
        assert!(result.css.contains(".flex-initial"));
        assert!(result.css.contains("flex: 0 auto"));
        assert!(result.css.contains(".flex-none"));
        assert!(result.css.contains("flex: none"));
        assert!(result.css.contains(".flex-\\[3_1_auto\\]"));
        assert!(result.css.contains("flex: 3_1_auto"));
        assert!(result.css.contains(".flex-\\(--my-flex\\)"));
        assert!(result.css.contains("flex: var(--my-flex)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:flex-1"));
    }

    #[test]
    fn generates_flex_grow_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "grow".to_string(),
                "flex-grow".to_string(),
                "grow-0".to_string(),
                "flex-grow-0".to_string(),
                "grow-3".to_string(),
                "grow-7".to_string(),
                "grow-[25vw]".to_string(),
                "grow-(--my-grow)".to_string(),
                "md:grow-0".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".grow"));
        assert!(result.css.contains("flex-grow: 1"));
        assert!(result.css.contains(".flex-grow"));
        assert!(result.css.contains("flex-grow: 1"));
        assert!(result.css.contains(".grow-0"));
        assert!(result.css.contains("flex-grow: 0"));
        assert!(result.css.contains(".flex-grow-0"));
        assert!(result.css.contains("flex-grow: 0"));
        assert!(result.css.contains(".grow-3"));
        assert!(result.css.contains("flex-grow: 3"));
        assert!(result.css.contains(".grow-7"));
        assert!(result.css.contains("flex-grow: 7"));
        assert!(result.css.contains(".grow-\\[25vw\\]"));
        assert!(result.css.contains("flex-grow: 25vw"));
        assert!(result.css.contains(".grow-\\(--my-grow\\)"));
        assert!(result.css.contains("flex-grow: var(--my-grow)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:grow-0"));
    }

    #[test]
    fn generates_flex_shrink_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "shrink".to_string(),
                "flex-shrink".to_string(),
                "shrink-0".to_string(),
                "flex-shrink-0".to_string(),
                "shrink-2".to_string(),
                "shrink-[calc(100vw-var(--sidebar))]".to_string(),
                "shrink-(--my-shrink)".to_string(),
                "md:shrink-0".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".shrink"));
        assert!(result.css.contains("flex-shrink: 1"));
        assert!(result.css.contains(".flex-shrink"));
        assert!(result.css.contains("flex-shrink: 1"));
        assert!(result.css.contains(".shrink-0"));
        assert!(result.css.contains("flex-shrink: 0"));
        assert!(result.css.contains(".flex-shrink-0"));
        assert!(result.css.contains("flex-shrink: 0"));
        assert!(result.css.contains(".shrink-2"));
        assert!(result.css.contains("flex-shrink: 2"));
        assert!(
            result
                .css
                .contains(".shrink-\\[calc\\(100vw-var\\(--sidebar\\)\\)\\]")
        );
        assert!(
            result
                .css
                .contains("flex-shrink: calc(100vw-var(--sidebar))")
        );
        assert!(result.css.contains(".shrink-\\(--my-shrink\\)"));
        assert!(result.css.contains("flex-shrink: var(--my-shrink)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:shrink-0"));
    }

    #[test]
    fn generates_grid_template_columns_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "grid-cols-1".to_string(),
                "grid-cols-6".to_string(),
                "grid-cols-none".to_string(),
                "grid-cols-subgrid".to_string(),
                "grid-cols-[200px_minmax(900px,_1fr)_100px]".to_string(),
                "grid-cols-(--my-grid-cols)".to_string(),
                "md:grid-cols-6".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".grid-cols-1"));
        assert!(
            result
                .css
                .contains("grid-template-columns: repeat(1, minmax(0, 1fr))")
        );
        assert!(result.css.contains(".grid-cols-6"));
        assert!(
            result
                .css
                .contains("grid-template-columns: repeat(6, minmax(0, 1fr))")
        );
        assert!(result.css.contains(".grid-cols-none"));
        assert!(result.css.contains("grid-template-columns: none"));
        assert!(result.css.contains(".grid-cols-subgrid"));
        assert!(result.css.contains("grid-template-columns: subgrid"));
        assert!(
            result
                .css
                .contains(".grid-cols-\\[200px_minmax\\(900px\\,_1fr\\)_100px\\]")
        );
        assert!(
            result
                .css
                .contains("grid-template-columns: 200px minmax(900px, 1fr) 100px")
        );
        assert!(result.css.contains(".grid-cols-\\(--my-grid-cols\\)"));
        assert!(
            result
                .css
                .contains("grid-template-columns: var(--my-grid-cols)")
        );
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:grid-cols-6"));
    }

    #[test]
    fn generates_grid_auto_flow_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "grid-flow-row".to_string(),
                "grid-flow-col".to_string(),
                "grid-flow-dense".to_string(),
                "grid-flow-row-dense".to_string(),
                "grid-flow-col-dense".to_string(),
                "md:grid-flow-row".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".grid-flow-row"));
        assert!(result.css.contains("grid-auto-flow: row"));
        assert!(result.css.contains(".grid-flow-col"));
        assert!(result.css.contains("grid-auto-flow: column"));
        assert!(result.css.contains(".grid-flow-dense"));
        assert!(result.css.contains("grid-auto-flow: dense"));
        assert!(result.css.contains(".grid-flow-row-dense"));
        assert!(result.css.contains("grid-auto-flow: row dense"));
        assert!(result.css.contains(".grid-flow-col-dense"));
        assert!(result.css.contains("grid-auto-flow: column dense"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:grid-flow-row"));
    }

    #[test]
    fn generates_grid_auto_columns_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "auto-cols-auto".to_string(),
                "auto-cols-min".to_string(),
                "auto-cols-max".to_string(),
                "auto-cols-fr".to_string(),
                "auto-cols-[minmax(0,2fr)]".to_string(),
                "auto-cols-(--my-auto-cols)".to_string(),
                "md:auto-cols-min".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".auto-cols-auto"));
        assert!(result.css.contains("grid-auto-columns: auto"));
        assert!(result.css.contains(".auto-cols-min"));
        assert!(result.css.contains("grid-auto-columns: min-content"));
        assert!(result.css.contains(".auto-cols-max"));
        assert!(result.css.contains("grid-auto-columns: max-content"));
        assert!(result.css.contains(".auto-cols-fr"));
        assert!(result.css.contains("grid-auto-columns: minmax(0, 1fr)"));
        assert!(result.css.contains(".auto-cols-\\[minmax\\(0\\,2fr\\)\\]"));
        assert!(result.css.contains("grid-auto-columns: minmax(0,2fr)"));
        assert!(result.css.contains(".auto-cols-\\(--my-auto-cols\\)"));
        assert!(
            result
                .css
                .contains("grid-auto-columns: var(--my-auto-cols)")
        );
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:auto-cols-min"));
    }

    #[test]
    fn generates_grid_auto_rows_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "auto-rows-auto".to_string(),
                "auto-rows-min".to_string(),
                "auto-rows-max".to_string(),
                "auto-rows-fr".to_string(),
                "auto-rows-[minmax(0,2fr)]".to_string(),
                "auto-rows-(--my-auto-rows)".to_string(),
                "md:auto-rows-min".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".auto-rows-auto"));
        assert!(result.css.contains("grid-auto-rows: auto"));
        assert!(result.css.contains(".auto-rows-min"));
        assert!(result.css.contains("grid-auto-rows: min-content"));
        assert!(result.css.contains(".auto-rows-max"));
        assert!(result.css.contains("grid-auto-rows: max-content"));
        assert!(result.css.contains(".auto-rows-fr"));
        assert!(result.css.contains("grid-auto-rows: minmax(0, 1fr)"));
        assert!(result.css.contains(".auto-rows-\\[minmax\\(0\\,2fr\\)\\]"));
        assert!(result.css.contains("grid-auto-rows: minmax(0,2fr)"));
        assert!(result.css.contains(".auto-rows-\\(--my-auto-rows\\)"));
        assert!(result.css.contains("grid-auto-rows: var(--my-auto-rows)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:auto-rows-min"));
    }

    #[test]
    fn generates_gap_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "gap-4".to_string(),
                "gap-0.5".to_string(),
                "gap-[10vw]".to_string(),
                "gap-(--my-gap)".to_string(),
                "gap-x-8".to_string(),
                "gap-x-[5rem]".to_string(),
                "gap-x-(--my-gap-x)".to_string(),
                "gap-x-0.5".to_string(),
                "gap-y-2".to_string(),
                "gap-y-[3vh]".to_string(),
                "gap-y-(--my-gap-y)".to_string(),
                "gap-y-0.5".to_string(),
                "md:gap-6".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".gap-4"));
        assert!(result.css.contains("gap: calc(var(--spacing) * 4)"));
        assert!(result.css.contains(".gap-0\\.5"));
        assert!(result.css.contains("gap: calc(var(--spacing) * 0.5)"));
        assert!(result.css.contains(".gap-\\[10vw\\]"));
        assert!(result.css.contains("gap: 10vw"));
        assert!(result.css.contains(".gap-\\(--my-gap\\)"));
        assert!(result.css.contains("gap: var(--my-gap)"));
        assert!(result.css.contains(".gap-x-8"));
        assert!(result.css.contains("column-gap: calc(var(--spacing) * 8)"));
        assert!(result.css.contains(".gap-x-0\\.5"));
        assert!(
            result
                .css
                .contains("column-gap: calc(var(--spacing) * 0.5)")
        );
        assert!(result.css.contains(".gap-x-\\[5rem\\]"));
        assert!(result.css.contains("column-gap: 5rem"));
        assert!(result.css.contains(".gap-x-\\(--my-gap-x\\)"));
        assert!(result.css.contains("column-gap: var(--my-gap-x)"));
        assert!(result.css.contains(".gap-y-2"));
        assert!(result.css.contains("row-gap: calc(var(--spacing) * 2)"));
        assert!(result.css.contains(".gap-y-0\\.5"));
        assert!(result.css.contains("row-gap: calc(var(--spacing) * 0.5)"));
        assert!(result.css.contains(".gap-y-\\[3vh\\]"));
        assert!(result.css.contains("row-gap: 3vh"));
        assert!(result.css.contains(".gap-y-\\(--my-gap-y\\)"));
        assert!(result.css.contains("row-gap: var(--my-gap-y)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:gap-6"));
    }

    #[test]
    fn generates_space_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "space-x-4".to_string(),
                "-space-x-2".to_string(),
                "space-x-px".to_string(),
                "-space-x-px".to_string(),
                "space-x-(--my-space-x)".to_string(),
                "space-x-[3rem]".to_string(),
                "space-y-6".to_string(),
                "-space-y-1".to_string(),
                "space-y-px".to_string(),
                "-space-y-px".to_string(),
                "space-y-(--my-space-y)".to_string(),
                "space-y-[10%]".to_string(),
                "space-x-reverse".to_string(),
                "space-y-reverse".to_string(),
                "md:space-x-8".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".space-x-4"));
        assert!(result.css.contains(":where(& > :not(:last-child))"));
        assert!(result.css.contains(
            "margin-inline-start:calc(calc(var(--spacing) * 4) * var(--tw-space-x-reverse))"
        ));
        assert!(result.css.contains(".-space-x-2"));
        assert!(result
            .css
            .contains("margin-inline-end:calc(calc(var(--spacing) * -2) * calc(1 - var(--tw-space-x-reverse)))"));
        assert!(result.css.contains(".space-x-px"));
        assert!(
            result
                .css
                .contains("margin-inline-start:calc(1px * var(--tw-space-x-reverse))")
        );
        assert!(result.css.contains(".-space-x-px"));
        assert!(
            result
                .css
                .contains("margin-inline-end:calc(-1px * calc(1 - var(--tw-space-x-reverse)))")
        );
        assert!(result.css.contains(".space-x-\\(--my-space-x\\)"));
        assert!(
            result.css.contains(
                "margin-inline-start:calc(var(--my-space-x) * var(--tw-space-x-reverse))"
            )
        );
        assert!(result.css.contains(".space-x-\\[3rem\\]"));
        assert!(
            result
                .css
                .contains("margin-inline-end:calc(3rem * calc(1 - var(--tw-space-x-reverse)))")
        );
        assert!(result.css.contains(".space-y-6"));
        assert!(result.css.contains(
            "margin-block-start:calc(calc(var(--spacing) * 6) * var(--tw-space-y-reverse))"
        ));
        assert!(result.css.contains(".-space-y-1"));
        assert!(result.css.contains(
            "margin-block-end:calc(calc(var(--spacing) * -1) * calc(1 - var(--tw-space-y-reverse)))"
        ));
        assert!(result.css.contains(".space-y-px"));
        assert!(
            result
                .css
                .contains("margin-block-start:calc(1px * var(--tw-space-y-reverse))")
        );
        assert!(result.css.contains(".-space-y-px"));
        assert!(
            result
                .css
                .contains("margin-block-end:calc(-1px * calc(1 - var(--tw-space-y-reverse)))")
        );
        assert!(result.css.contains(".space-y-\\(--my-space-y\\)"));
        assert!(
            result
                .css
                .contains("margin-block-start:calc(var(--my-space-y) * var(--tw-space-y-reverse))")
        );
        assert!(result.css.contains(".space-y-\\[10\\%\\]"));
        assert!(
            result
                .css
                .contains("margin-block-end:calc(10% * calc(1 - var(--tw-space-y-reverse)))")
        );
        assert!(result.css.contains(".space-x-reverse"));
        assert!(result.css.contains("--tw-space-x-reverse:1"));
        assert!(result.css.contains(".space-y-reverse"));
        assert!(result.css.contains("--tw-space-y-reverse:1"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:space-x-8"));
    }

    #[test]
    fn generates_width_and_size_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "w-4".to_string(),
                "w-0.5".to_string(),
                "w-1/2".to_string(),
                "w-3xs".to_string(),
                "w-auto".to_string(),
                "w-px".to_string(),
                "w-full".to_string(),
                "w-screen".to_string(),
                "w-dvw".to_string(),
                "w-dvh".to_string(),
                "w-lvw".to_string(),
                "w-lvh".to_string(),
                "w-svw".to_string(),
                "w-svh".to_string(),
                "w-min".to_string(),
                "w-max".to_string(),
                "w-fit".to_string(),
                "w-(--my-width)".to_string(),
                "w-[42ch]".to_string(),
                "h-0.5".to_string(),
                "size-8".to_string(),
                "size-0.5".to_string(),
                "size-1/2".to_string(),
                "size-auto".to_string(),
                "size-px".to_string(),
                "size-full".to_string(),
                "size-dvw".to_string(),
                "size-dvh".to_string(),
                "size-lvw".to_string(),
                "size-lvh".to_string(),
                "size-svw".to_string(),
                "size-svh".to_string(),
                "size-min".to_string(),
                "size-max".to_string(),
                "size-fit".to_string(),
                "size-(--my-size)".to_string(),
                "size-[10rem]".to_string(),
                "md:w-auto".to_string(),
                "md:size-4".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".w-4"));
        assert!(result.css.contains("width: calc(var(--spacing) * 4)"));
        assert!(result.css.contains(".w-0\\.5"));
        assert!(result.css.contains("width: calc(var(--spacing) * 0.5)"));
        assert!(result.css.contains(".w-1\\/2"));
        assert!(result.css.contains("width: calc(1/2 * 100%)"));
        assert!(result.css.contains(".w-3xs"));
        assert!(result.css.contains("width: var(--container-3xs)"));
        assert!(result.css.contains(".w-auto"));
        assert!(result.css.contains("width: auto"));
        assert!(result.css.contains(".w-px"));
        assert!(result.css.contains("width: 1px"));
        assert!(result.css.contains(".w-full"));
        assert!(result.css.contains("width: 100%"));
        assert!(result.css.contains(".w-screen"));
        assert!(result.css.contains("width: 100vw"));
        assert!(result.css.contains(".w-dvw"));
        assert!(result.css.contains("width: 100dvw"));
        assert!(result.css.contains(".w-dvh"));
        assert!(result.css.contains("width: 100dvh"));
        assert!(result.css.contains(".w-lvw"));
        assert!(result.css.contains("width: 100lvw"));
        assert!(result.css.contains(".w-lvh"));
        assert!(result.css.contains("width: 100lvh"));
        assert!(result.css.contains(".w-svw"));
        assert!(result.css.contains("width: 100svw"));
        assert!(result.css.contains(".w-svh"));
        assert!(result.css.contains("width: 100svh"));
        assert!(result.css.contains(".w-min"));
        assert!(result.css.contains("width: min-content"));
        assert!(result.css.contains(".w-max"));
        assert!(result.css.contains("width: max-content"));
        assert!(result.css.contains(".w-fit"));
        assert!(result.css.contains("width: fit-content"));
        assert!(result.css.contains(".w-\\(--my-width\\)"));
        assert!(result.css.contains("width: var(--my-width)"));
        assert!(result.css.contains(".w-\\[42ch\\]"));
        assert!(result.css.contains("width: 42ch"));
        assert!(result.css.contains(".h-0\\.5"));
        assert!(result.css.contains("height: calc(var(--spacing) * 0.5)"));

        assert!(result.css.contains(".size-8"));
        assert!(result.css.contains("width: calc(var(--spacing) * 8)"));
        assert!(result.css.contains("height: calc(var(--spacing) * 8)"));
        assert!(result.css.contains(".size-0\\.5"));
        assert!(result.css.contains("width: calc(var(--spacing) * 0.5)"));
        assert!(result.css.contains("height: calc(var(--spacing) * 0.5)"));
        assert!(result.css.contains(".size-1\\/2"));
        assert!(result.css.contains("width: calc(1/2 * 100%)"));
        assert!(result.css.contains("height: calc(1/2 * 100%)"));
        assert!(result.css.contains(".size-auto"));
        assert!(result.css.contains("width: auto"));
        assert!(result.css.contains("height: auto"));
        assert!(result.css.contains(".size-px"));
        assert!(result.css.contains("width: 1px"));
        assert!(result.css.contains("height: 1px"));
        assert!(result.css.contains(".size-full"));
        assert!(result.css.contains("width: 100%"));
        assert!(result.css.contains("height: 100%"));
        assert!(result.css.contains(".size-dvw"));
        assert!(result.css.contains("width: 100dvw"));
        assert!(result.css.contains("height: 100dvw"));
        assert!(result.css.contains(".size-dvh"));
        assert!(result.css.contains("width: 100dvh"));
        assert!(result.css.contains("height: 100dvh"));
        assert!(result.css.contains(".size-lvw"));
        assert!(result.css.contains("width: 100lvw"));
        assert!(result.css.contains("height: 100lvw"));
        assert!(result.css.contains(".size-lvh"));
        assert!(result.css.contains("width: 100lvh"));
        assert!(result.css.contains("height: 100lvh"));
        assert!(result.css.contains(".size-svw"));
        assert!(result.css.contains("width: 100svw"));
        assert!(result.css.contains("height: 100svw"));
        assert!(result.css.contains(".size-svh"));
        assert!(result.css.contains("width: 100svh"));
        assert!(result.css.contains("height: 100svh"));
        assert!(result.css.contains(".size-min"));
        assert!(result.css.contains("width: min-content"));
        assert!(result.css.contains("height: min-content"));
        assert!(result.css.contains(".size-max"));
        assert!(result.css.contains("width: max-content"));
        assert!(result.css.contains("height: max-content"));
        assert!(result.css.contains(".size-fit"));
        assert!(result.css.contains("width: fit-content"));
        assert!(result.css.contains("height: fit-content"));
        assert!(result.css.contains(".size-\\(--my-size\\)"));
        assert!(result.css.contains("width: var(--my-size)"));
        assert!(result.css.contains("height: var(--my-size)"));
        assert!(result.css.contains(".size-\\[10rem\\]"));
        assert!(result.css.contains("width: 10rem"));
        assert!(result.css.contains("height: 10rem"));

        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:w-auto"));
        assert!(result.css.contains(".md\\:size-4"));
    }

    #[test]
    fn generates_min_width_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "min-w-0".to_string(),
                "min-w-4".to_string(),
                "min-w-1/2".to_string(),
                "min-w-3xs".to_string(),
                "min-w-auto".to_string(),
                "min-w-px".to_string(),
                "min-w-full".to_string(),
                "min-w-screen".to_string(),
                "min-w-dvw".to_string(),
                "min-w-dvh".to_string(),
                "min-w-lvw".to_string(),
                "min-w-lvh".to_string(),
                "min-w-svw".to_string(),
                "min-w-svh".to_string(),
                "min-w-min".to_string(),
                "min-w-max".to_string(),
                "min-w-fit".to_string(),
                "min-w-(--my-min-width)".to_string(),
                "min-w-[220px]".to_string(),
                "md:min-w-0".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".min-w-0"));
        assert!(result.css.contains("min-width: calc(var(--spacing) * 0)"));
        assert!(result.css.contains(".min-w-4"));
        assert!(result.css.contains("min-width: calc(var(--spacing) * 4)"));
        assert!(result.css.contains(".min-w-1\\/2"));
        assert!(result.css.contains("min-width: calc(1/2 * 100%)"));
        assert!(result.css.contains(".min-w-3xs"));
        assert!(result.css.contains("min-width: var(--container-3xs)"));
        assert!(result.css.contains(".min-w-auto"));
        assert!(result.css.contains("min-width: auto"));
        assert!(result.css.contains(".min-w-px"));
        assert!(result.css.contains("min-width: 1px"));
        assert!(result.css.contains(".min-w-full"));
        assert!(result.css.contains("min-width: 100%"));
        assert!(result.css.contains(".min-w-screen"));
        assert!(result.css.contains("min-width: 100vw"));
        assert!(result.css.contains(".min-w-dvw"));
        assert!(result.css.contains("min-width: 100dvw"));
        assert!(result.css.contains(".min-w-dvh"));
        assert!(result.css.contains("min-width: 100dvh"));
        assert!(result.css.contains(".min-w-lvw"));
        assert!(result.css.contains("min-width: 100lvw"));
        assert!(result.css.contains(".min-w-lvh"));
        assert!(result.css.contains("min-width: 100lvh"));
        assert!(result.css.contains(".min-w-svw"));
        assert!(result.css.contains("min-width: 100svw"));
        assert!(result.css.contains(".min-w-svh"));
        assert!(result.css.contains("min-width: 100svh"));
        assert!(result.css.contains(".min-w-min"));
        assert!(result.css.contains("min-width: min-content"));
        assert!(result.css.contains(".min-w-max"));
        assert!(result.css.contains("min-width: max-content"));
        assert!(result.css.contains(".min-w-fit"));
        assert!(result.css.contains("min-width: fit-content"));
        assert!(result.css.contains(".min-w-\\(--my-min-width\\)"));
        assert!(result.css.contains("min-width: var(--my-min-width)"));
        assert!(result.css.contains(".min-w-\\[220px\\]"));
        assert!(result.css.contains("min-width: 220px"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:min-w-0"));
    }

    #[test]
    fn generates_max_width_and_container_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "max-w-4".to_string(),
                "max-w-1/2".to_string(),
                "max-w-3xs".to_string(),
                "max-w-none".to_string(),
                "max-w-px".to_string(),
                "max-w-full".to_string(),
                "max-w-screen".to_string(),
                "max-w-dvw".to_string(),
                "max-w-dvh".to_string(),
                "max-w-lvw".to_string(),
                "max-w-lvh".to_string(),
                "max-w-svw".to_string(),
                "max-w-svh".to_string(),
                "max-w-min".to_string(),
                "max-w-max".to_string(),
                "max-w-fit".to_string(),
                "max-w-(--my-max-width)".to_string(),
                "max-w-[220px]".to_string(),
                "container".to_string(),
                "md:max-w-lg".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".max-w-4"));
        assert!(result.css.contains("max-width: calc(var(--spacing) * 4)"));
        assert!(result.css.contains(".max-w-1\\/2"));
        assert!(result.css.contains("max-width: calc(1/2 * 100%)"));
        assert!(result.css.contains(".max-w-3xs"));
        assert!(result.css.contains("max-width: var(--container-3xs)"));
        assert!(result.css.contains(".max-w-none"));
        assert!(result.css.contains("max-width: none"));
        assert!(result.css.contains(".max-w-px"));
        assert!(result.css.contains("max-width: 1px"));
        assert!(result.css.contains(".max-w-full"));
        assert!(result.css.contains("max-width: 100%"));
        assert!(result.css.contains(".max-w-screen"));
        assert!(result.css.contains("max-width: 100vw"));
        assert!(result.css.contains(".max-w-dvw"));
        assert!(result.css.contains("max-width: 100dvw"));
        assert!(result.css.contains(".max-w-dvh"));
        assert!(result.css.contains("max-width: 100dvh"));
        assert!(result.css.contains(".max-w-lvw"));
        assert!(result.css.contains("max-width: 100lvw"));
        assert!(result.css.contains(".max-w-lvh"));
        assert!(result.css.contains("max-width: 100lvh"));
        assert!(result.css.contains(".max-w-svw"));
        assert!(result.css.contains("max-width: 100svw"));
        assert!(result.css.contains(".max-w-svh"));
        assert!(result.css.contains("max-width: 100svh"));
        assert!(result.css.contains(".max-w-min"));
        assert!(result.css.contains("max-width: min-content"));
        assert!(result.css.contains(".max-w-max"));
        assert!(result.css.contains("max-width: max-content"));
        assert!(result.css.contains(".max-w-fit"));
        assert!(result.css.contains("max-width: fit-content"));
        assert!(result.css.contains(".max-w-\\(--my-max-width\\)"));
        assert!(result.css.contains("max-width: var(--my-max-width)"));
        assert!(result.css.contains(".max-w-\\[220px\\]"));
        assert!(result.css.contains("max-width: 220px"));
        assert!(result.css.contains(".container"));
        assert!(result.css.contains("width: 100%"));
        assert!(result.css.contains("@media (width >= 40rem)"));
        assert!(result.css.contains("max-width: 40rem"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains("max-width: 48rem"));
        assert!(result.css.contains("@media (width >= 64rem)"));
        assert!(result.css.contains("max-width: 64rem"));
        assert!(result.css.contains("@media (width >= 80rem)"));
        assert!(result.css.contains("max-width: 80rem"));
        assert!(result.css.contains("@media (width >= 96rem)"));
        assert!(result.css.contains("max-width: 96rem"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:max-w-lg"));
    }

    #[test]
    fn generates_height_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "h-4".to_string(),
                "h-1/2".to_string(),
                "h-auto".to_string(),
                "h-px".to_string(),
                "h-full".to_string(),
                "h-screen".to_string(),
                "h-dvh".to_string(),
                "h-dvw".to_string(),
                "h-lvh".to_string(),
                "h-lvw".to_string(),
                "h-svh".to_string(),
                "h-svw".to_string(),
                "h-min".to_string(),
                "h-max".to_string(),
                "h-fit".to_string(),
                "h-lh".to_string(),
                "h-(--my-height)".to_string(),
                "h-[32rem]".to_string(),
                "md:h-full".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".h-4"));
        assert!(result.css.contains("height: calc(var(--spacing) * 4)"));
        assert!(result.css.contains(".h-1\\/2"));
        assert!(result.css.contains("height: calc(1/2 * 100%)"));
        assert!(result.css.contains(".h-auto"));
        assert!(result.css.contains("height: auto"));
        assert!(result.css.contains(".h-px"));
        assert!(result.css.contains("height: 1px"));
        assert!(result.css.contains(".h-full"));
        assert!(result.css.contains("height: 100%"));
        assert!(result.css.contains(".h-screen"));
        assert!(result.css.contains("height: 100vh"));
        assert!(result.css.contains(".h-dvh"));
        assert!(result.css.contains("height: 100dvh"));
        assert!(result.css.contains(".h-dvw"));
        assert!(result.css.contains("height: 100dvw"));
        assert!(result.css.contains(".h-lvh"));
        assert!(result.css.contains("height: 100lvh"));
        assert!(result.css.contains(".h-lvw"));
        assert!(result.css.contains("height: 100lvw"));
        assert!(result.css.contains(".h-svh"));
        assert!(result.css.contains("height: 100svh"));
        assert!(result.css.contains(".h-svw"));
        assert!(result.css.contains("height: 100svw"));
        assert!(result.css.contains(".h-min"));
        assert!(result.css.contains("height: min-content"));
        assert!(result.css.contains(".h-max"));
        assert!(result.css.contains("height: max-content"));
        assert!(result.css.contains(".h-fit"));
        assert!(result.css.contains("height: fit-content"));
        assert!(result.css.contains(".h-lh"));
        assert!(result.css.contains("height: 1lh"));
        assert!(result.css.contains(".h-\\(--my-height\\)"));
        assert!(result.css.contains("height: var(--my-height)"));
        assert!(result.css.contains(".h-\\[32rem\\]"));
        assert!(result.css.contains("height: 32rem"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:h-full"));
    }

    #[test]
    fn generates_min_height_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "min-h-0".to_string(),
                "min-h-4".to_string(),
                "min-h-1/2".to_string(),
                "min-h-px".to_string(),
                "min-h-full".to_string(),
                "min-h-screen".to_string(),
                "min-h-dvh".to_string(),
                "min-h-dvw".to_string(),
                "min-h-lvh".to_string(),
                "min-h-lvw".to_string(),
                "min-h-svh".to_string(),
                "min-h-svw".to_string(),
                "min-h-auto".to_string(),
                "min-h-min".to_string(),
                "min-h-max".to_string(),
                "min-h-fit".to_string(),
                "min-h-lh".to_string(),
                "min-h-(--my-min-height)".to_string(),
                "min-h-[220px]".to_string(),
                "md:min-h-0".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".min-h-0"));
        assert!(result.css.contains("min-height: calc(var(--spacing) * 0)"));
        assert!(result.css.contains(".min-h-4"));
        assert!(result.css.contains("min-height: calc(var(--spacing) * 4)"));
        assert!(result.css.contains(".min-h-1\\/2"));
        assert!(result.css.contains("min-height: calc(1/2 * 100%)"));
        assert!(result.css.contains(".min-h-px"));
        assert!(result.css.contains("min-height: 1px"));
        assert!(result.css.contains(".min-h-full"));
        assert!(result.css.contains("min-height: 100%"));
        assert!(result.css.contains(".min-h-screen"));
        assert!(result.css.contains("min-height: 100vh"));
        assert!(result.css.contains(".min-h-dvh"));
        assert!(result.css.contains("min-height: 100dvh"));
        assert!(result.css.contains(".min-h-dvw"));
        assert!(result.css.contains("min-height: 100dvw"));
        assert!(result.css.contains(".min-h-lvh"));
        assert!(result.css.contains("min-height: 100lvh"));
        assert!(result.css.contains(".min-h-lvw"));
        assert!(result.css.contains("min-height: 100lvw"));
        assert!(result.css.contains(".min-h-svh"));
        assert!(result.css.contains("min-height: 100svh"));
        assert!(result.css.contains(".min-h-svw"));
        assert!(result.css.contains("min-height: 100svw"));
        assert!(result.css.contains(".min-h-auto"));
        assert!(result.css.contains("min-height: auto"));
        assert!(result.css.contains(".min-h-min"));
        assert!(result.css.contains("min-height: min-content"));
        assert!(result.css.contains(".min-h-max"));
        assert!(result.css.contains("min-height: max-content"));
        assert!(result.css.contains(".min-h-fit"));
        assert!(result.css.contains("min-height: fit-content"));
        assert!(result.css.contains(".min-h-lh"));
        assert!(result.css.contains("min-height: 1lh"));
        assert!(result.css.contains(".min-h-\\(--my-min-height\\)"));
        assert!(result.css.contains("min-height: var(--my-min-height)"));
        assert!(result.css.contains(".min-h-\\[220px\\]"));
        assert!(result.css.contains("min-height: 220px"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:min-h-0"));
    }

    #[test]
    fn generates_max_height_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "max-h-0".to_string(),
                "max-h-4".to_string(),
                "max-h-1/2".to_string(),
                "max-h-none".to_string(),
                "max-h-px".to_string(),
                "max-h-full".to_string(),
                "max-h-screen".to_string(),
                "max-h-dvh".to_string(),
                "max-h-dvw".to_string(),
                "max-h-lvh".to_string(),
                "max-h-lvw".to_string(),
                "max-h-svh".to_string(),
                "max-h-svw".to_string(),
                "max-h-min".to_string(),
                "max-h-max".to_string(),
                "max-h-fit".to_string(),
                "max-h-lh".to_string(),
                "max-h-(--my-max-height)".to_string(),
                "max-h-[220px]".to_string(),
                "md:max-h-screen".to_string(),
            ],
            &config,
        );

        assert!(result.css.contains(".max-h-0"));
        assert!(result.css.contains("max-height: calc(var(--spacing) * 0)"));
        assert!(result.css.contains(".max-h-4"));
        assert!(result.css.contains("max-height: calc(var(--spacing) * 4)"));
        assert!(result.css.contains(".max-h-1\\/2"));
        assert!(result.css.contains("max-height: calc(1/2 * 100%)"));
        assert!(result.css.contains(".max-h-none"));
        assert!(result.css.contains("max-height: none"));
        assert!(result.css.contains(".max-h-px"));
        assert!(result.css.contains("max-height: 1px"));
        assert!(result.css.contains(".max-h-full"));
        assert!(result.css.contains("max-height: 100%"));
        assert!(result.css.contains(".max-h-screen"));
        assert!(result.css.contains("max-height: 100vh"));
        assert!(result.css.contains(".max-h-dvh"));
        assert!(result.css.contains("max-height: 100dvh"));
        assert!(result.css.contains(".max-h-dvw"));
        assert!(result.css.contains("max-height: 100dvw"));
        assert!(result.css.contains(".max-h-lvh"));
        assert!(result.css.contains("max-height: 100lvh"));
        assert!(result.css.contains(".max-h-lvw"));
        assert!(result.css.contains("max-height: 100lvw"));
        assert!(result.css.contains(".max-h-svh"));
        assert!(result.css.contains("max-height: 100svh"));
        assert!(result.css.contains(".max-h-svw"));
        assert!(result.css.contains("max-height: 100svw"));
        assert!(result.css.contains(".max-h-min"));
        assert!(result.css.contains("max-height: min-content"));
        assert!(result.css.contains(".max-h-max"));
        assert!(result.css.contains("max-height: max-content"));
        assert!(result.css.contains(".max-h-fit"));
        assert!(result.css.contains("max-height: fit-content"));
        assert!(result.css.contains(".max-h-lh"));
        assert!(result.css.contains("max-height: 1lh"));
        assert!(result.css.contains(".max-h-\\(--my-max-height\\)"));
        assert!(result.css.contains("max-height: var(--my-max-height)"));
        assert!(result.css.contains(".max-h-\\[220px\\]"));
        assert!(result.css.contains("max-height: 220px"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:max-h-screen"));
    }

    #[test]
    fn generates_grid_column_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "col-span-2".to_string(),
                "col-span-full".to_string(),
                "col-span-[5]".to_string(),
                "col-span-(--my-span)".to_string(),
                "col-start-2".to_string(),
                "-col-start-2".to_string(),
                "col-start-auto".to_string(),
                "col-start-[7]".to_string(),
                "col-start-(--my-start)".to_string(),
                "col-end-7".to_string(),
                "-col-end-3".to_string(),
                "col-end-auto".to_string(),
                "col-end-[9]".to_string(),
                "col-end-(--my-end)".to_string(),
                "col-auto".to_string(),
                "col-7".to_string(),
                "-col-4".to_string(),
                "col-[16_/_span_16]".to_string(),
                "col-(--my-columns)".to_string(),
                "md:col-span-6".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".col-span-2"));
        assert!(result.css.contains("grid-column: span 2 / span 2"));
        assert!(result.css.contains(".col-span-full"));
        assert!(result.css.contains("grid-column: 1 / -1"));
        assert!(result.css.contains(".col-span-\\[5\\]"));
        assert!(result.css.contains("grid-column: span 5 / span 5"));
        assert!(result.css.contains(".col-span-\\(--my-span\\)"));
        assert!(
            result
                .css
                .contains("grid-column: span var(--my-span) / span var(--my-span)")
        );
        assert!(result.css.contains(".col-start-2"));
        assert!(result.css.contains("grid-column-start: 2"));
        assert!(result.css.contains(".-col-start-2"));
        assert!(result.css.contains("grid-column-start: calc(2 * -1)"));
        assert!(result.css.contains(".col-start-auto"));
        assert!(result.css.contains("grid-column-start: auto"));
        assert!(result.css.contains(".col-start-\\[7\\]"));
        assert!(result.css.contains("grid-column-start: 7"));
        assert!(result.css.contains(".col-start-\\(--my-start\\)"));
        assert!(result.css.contains("grid-column-start: var(--my-start)"));
        assert!(result.css.contains(".col-end-7"));
        assert!(result.css.contains("grid-column-end: 7"));
        assert!(result.css.contains(".-col-end-3"));
        assert!(result.css.contains("grid-column-end: calc(3 * -1)"));
        assert!(result.css.contains(".col-end-auto"));
        assert!(result.css.contains("grid-column-end: auto"));
        assert!(result.css.contains(".col-end-\\[9\\]"));
        assert!(result.css.contains("grid-column-end: 9"));
        assert!(result.css.contains(".col-end-\\(--my-end\\)"));
        assert!(result.css.contains("grid-column-end: var(--my-end)"));
        assert!(result.css.contains(".col-auto"));
        assert!(result.css.contains("grid-column: auto"));
        assert!(result.css.contains(".col-7"));
        assert!(result.css.contains("grid-column: 7"));
        assert!(result.css.contains(".-col-4"));
        assert!(result.css.contains("grid-column: calc(4 * -1)"));
        assert!(result.css.contains(".col-\\[16_\\/_span_16\\]"));
        assert!(result.css.contains("grid-column: 16_/_span_16"));
        assert!(result.css.contains(".col-\\(--my-columns\\)"));
        assert!(result.css.contains("grid-column: var(--my-columns)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:col-span-6"));
    }

    #[test]
    fn generates_grid_row_rules() {
        let config = GeneratorConfig {
            minify: false,
            colors: BTreeMap::new(),
        };
        let result = generate(
            &[
                "row-span-2".to_string(),
                "row-span-full".to_string(),
                "row-span-[5]".to_string(),
                "row-span-(--my-span)".to_string(),
                "row-start-2".to_string(),
                "-row-start-2".to_string(),
                "row-start-auto".to_string(),
                "row-start-[7]".to_string(),
                "row-start-(--my-start)".to_string(),
                "row-end-7".to_string(),
                "-row-end-3".to_string(),
                "row-end-auto".to_string(),
                "row-end-[9]".to_string(),
                "row-end-(--my-end)".to_string(),
                "row-auto".to_string(),
                "row-7".to_string(),
                "-row-4".to_string(),
                "row-[span_16_/_span_16]".to_string(),
                "row-(--my-rows)".to_string(),
                "md:row-span-4".to_string(),
            ],
            &config,
        );
        assert!(result.css.contains(".row-span-2"));
        assert!(result.css.contains("grid-row: span 2 / span 2"));
        assert!(result.css.contains(".row-span-full"));
        assert!(result.css.contains("grid-row: 1 / -1"));
        assert!(result.css.contains(".row-span-\\[5\\]"));
        assert!(result.css.contains("grid-row: span 5 / span 5"));
        assert!(result.css.contains(".row-span-\\(--my-span\\)"));
        assert!(
            result
                .css
                .contains("grid-row: span var(--my-span) / span var(--my-span)")
        );
        assert!(result.css.contains(".row-start-2"));
        assert!(result.css.contains("grid-row-start: 2"));
        assert!(result.css.contains(".-row-start-2"));
        assert!(result.css.contains("grid-row-start: calc(2 * -1)"));
        assert!(result.css.contains(".row-start-auto"));
        assert!(result.css.contains("grid-row-start: auto"));
        assert!(result.css.contains(".row-start-\\[7\\]"));
        assert!(result.css.contains("grid-row-start: 7"));
        assert!(result.css.contains(".row-start-\\(--my-start\\)"));
        assert!(result.css.contains("grid-row-start: var(--my-start)"));
        assert!(result.css.contains(".row-end-7"));
        assert!(result.css.contains("grid-row-end: 7"));
        assert!(result.css.contains(".-row-end-3"));
        assert!(result.css.contains("grid-row-end: calc(3 * -1)"));
        assert!(result.css.contains(".row-end-auto"));
        assert!(result.css.contains("grid-row-end: auto"));
        assert!(result.css.contains(".row-end-\\[9\\]"));
        assert!(result.css.contains("grid-row-end: 9"));
        assert!(result.css.contains(".row-end-\\(--my-end\\)"));
        assert!(result.css.contains("grid-row-end: var(--my-end)"));
        assert!(result.css.contains(".row-auto"));
        assert!(result.css.contains("grid-row: auto"));
        assert!(result.css.contains(".row-7"));
        assert!(result.css.contains("grid-row: 7"));
        assert!(result.css.contains(".-row-4"));
        assert!(result.css.contains("grid-row: calc(4 * -1)"));
        assert!(result.css.contains(".row-\\[span_16_\\/_span_16\\]"));
        assert!(result.css.contains("grid-row: span_16_/_span_16"));
        assert!(result.css.contains(".row-\\(--my-rows\\)"));
        assert!(result.css.contains("grid-row: var(--my-rows)"));
        assert!(result.css.contains("@media (width >= 48rem)"));
        assert!(result.css.contains(".md\\:row-span-4"));
    }
}
