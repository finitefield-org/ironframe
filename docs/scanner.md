# Scanner Design (Draft)

This document describes the initial design for the `tailwind_scanner` crate. The scanner extracts Tailwind-style class candidates from project files for JIT generation.

## Scope

- Input: file paths from CLI or config
- Output: de-duplicated class candidates and scan metadata
- Non-goals: executing code, evaluating templates, or parsing full ASTs in v1

## High-Level Pipeline

1. Resolve file list from config/CLI globs
2. Read file contents as UTF-8 text, skip binary or invalid encodings
3. Select extractor by extension
4. Extract class lists or string literals
5. Tokenize class lists into class candidates
6. Validate candidates and de-duplicate

## Extractors

The scanner uses a small set of heuristic extractors, keyed by file extension. Each extractor produces a list of candidate strings and class lists.

### Markup Extractor

Targets HTML-like syntaxes and template files.

- Supported attributes: `class`, `className`, `:class`, `v-bind:class`, `class:list`
- Supports quoted values (`"..."`, `'...'`) and unquoted values (`class=foo`)
- For `:class` or `v-bind:class`, extract string literals inside the expression
- For `class:list` (Svelte), extract from quoted lists or object keys

### Script Extractor

Targets JS/TS/JSX/TSX.

- Extract from string literals: `"..."`, `'...'`, and template string static segments
- Ignore interpolations inside `${...}` for template strings
- Optionally detect common class helper calls (`clsx`, `classnames`, `tw`, `cva`) by simple pattern match

### Markdown Extractor

Targets Markdown and MDX.

- Extract from inline HTML blocks and JSX blocks
- Extract from fenced code blocks if language is HTML/JSX/TSX

### Fallback Extractor

For unknown file types, extract from string literals only.

## Class List Tokenization

Given a class list string, split it into tokens while respecting Tailwind syntax.

Rules:

- Split on ASCII whitespace when not inside brackets or parentheses
- Track nesting for `[]` and `()`
- Allow escaped characters with `\`
- Do not split inside balanced brackets (arbitrary values and arbitrary variants)

Pseudo-logic:

```
stack = []
current = ""
for each char c:
  if c is escape and next exists:
    append c and next to current
    continue
  if c is '[' or '(':
    push c
  if c is ']' or ')':
    if stack top matches, pop
  if is_whitespace(c) and stack is empty:
    emit current
    clear current
  else:
    append c
emit current
```

## Candidate Validation

After tokenization, each candidate is validated to reduce false positives.

- Allow leading `!` (important)
- Require at least one ASCII letter or `[`
- Allow characters: `A-Z a-z 0-9 - _ / : . % # [ ] ( ) !`
- Reject candidates that are only separators (e.g., `:` or `-`)

In the future, validation should call `tailwind_core::parse` and keep only tokens that parse successfully.

## Data Structures

- `ScanTarget { path: PathBuf }`
- `ScanResult { classes: Vec<String>, files_scanned: usize }`
- Internally use `HashSet<String>` for de-duplication and a `Vec<String>` for stable output order

## Error Handling

- Skip unreadable files and collect a warning
- Skip non-UTF-8 content and collect a warning
- `ScanError` only for fatal issues (e.g., config errors)

## Performance Notes

- Stream file reading to avoid large allocations
- Avoid regex-heavy scanning in hot paths
- Cache extractor selection by extension
- Respect `.gitignore`/`.ignore` rules via the filesystem walker
- Optional extra ignore patterns can be provided via CLI (`--ignore`)

## Test Fixtures (Initial)

- HTML: `class="text-sm bg-red-500"`
- JSX: `<div className={"p-4"} />`
- Vue: `<div :class="['p-4', isDark && 'bg-black']" />`
- Svelte: `<div class:list="p-4 bg-red-500" />`
- Arbitrary values: `bg-[color:var(--brand)]`
- Arbitrary variants: `[&:hover]:bg-red-500`
- Important: `!text-sm`

## Future Extensions

- Pluggable extractors per framework
- Configurable variant separator
- Incremental scanning and change detection
