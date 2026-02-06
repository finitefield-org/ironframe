# ironframe

This repository is for learning and prototyping a Rust implementation that matches the feature set of Tailwind CSS v4.1.

## Goals

- Understand the design principles and feature composition of Tailwind CSS v4.1
- Design a Rust architecture with equivalent capabilities and build a minimal working prototype
- Evaluate performance and developer experience tradeoffs

## Target Feature Scope (Tailwind CSS v4.1 Equivalent)

- Utility class generation
- JIT compilation
- Content scanning (HTML/TSX/Vue/etc.)
- Configuration files (theme, extensions, plugins)
- Arbitrary values (custom colors/sizes)
- Layering (base/components/utilities)
- Variants (hover, focus, dark, etc.)
- Plugin API
- Preflight-equivalent reset
- CLI and build pipeline integration

## Approach

1. Break down features and define requirements
2. Implement core pieces in Rust (parser, scanner, generator)
3. Design the CLI and ship a minimal MVP
4. Benchmark and optimize

## Expected Outputs

- Design documents
- Rust implementations of core components
- Minimal CLI
- Sample projects

## Workspace Layout

- `crates/core`: tokenization, AST, and core analysis utilities
- `crates/scanner`: content scanning and class extraction
- `crates/generator`: utility class generation and CSS output
- `crates/config`: configuration, theme, and plugin loading
- `crates/cli`: CLI entrypoint and build pipeline integration

## Minimal API (Draft)

- `ironframe_core`
  - `Token` and `TokenKind` for lexer output
  - `AstNode` for parsed structures
  - `ParseError` and `Diagnostic`
  - `parse(input: &str) -> Result<AstNode, ParseError>`
- `ironframe_scanner`
  - `ScanTarget` and `ScanResult`
  - `ScanGlobOptions`
  - `scan(paths: &[PathBuf]) -> Result<ScanResult, ScanError>`
  - `scan_globs(patterns: &[String]) -> Result<ScanResult, ScanError>`
  - `scan_globs_with_ignore(patterns: &[String], ignore: &[String]) -> Result<ScanResult, ScanError>`
  - `scan_globs_with_options(patterns: &[String], ignore: &[String], options: &ScanGlobOptions) -> Result<ScanResult, ScanError>`
  - `extract_classes(text: &str) -> Vec<String>`
- `ironframe_generator`
  - `GeneratorConfig` and `GenerationResult`
  - `generate(classes: &[String], config: &GeneratorConfig) -> GenerationResult`
  - `emit_css(result: &GenerationResult) -> String`
- `ironframe_config`
  - `Config` and `Theme`
  - `load(path: &Path) -> Result<Config, ConfigError>`
  - `resolve_theme(config: &Config) -> Theme`
- `ironframe_cli`
  - `main` with subcommands: `scan`, `build`, `watch`
  - `build` wires: `config -> scanner -> generator -> css`

## Notes

This repository references Tailwind CSS v4.1 for behavior and ideas but is not affiliated with the official implementation.

`ironframe_cli build --input-css` supports Tailwind-style `@source` directives for:
- explicit source registration (`@source "../path"`)
- source exclusion (`@source not "../path"`)
- disabling auto detection (`@import "tailwindcss" source(none)`)
- base path override (`@import "tailwindcss" source("../src")`)
- inline safelist / blocklist via brace expansion (`@source inline(...)`, `@source not inline(...)`)

## Example Config

`tailwind.toml`:

```toml
[theme]
name = "default"

[theme.colors.gray]
100 = "#f3f4f6"
500 = "#6b7280"
```

## License

TBD
