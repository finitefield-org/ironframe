# ironframe

ironframe is a production-ready Rust implementation targeting high compatibility with Tailwind CSS v4.1 behavior and output.
It is designed for teams that want a Tailwind-compatible workflow without installing Tailwind CSS through Node.js or npm.
It is especially suited to developers building web apps in ecosystems like htmx, where avoiding Node.js-based tooling is a key requirement.
Developed by [Finite Field, K.K.](https://finitefield.org/en/).

## Goals

- Provide a production-operable Rust CSS engine with Tailwind CSS v4.1-equivalent features
- Maintain high compatibility with Tailwind CSS utility generation, variants, and directives
- Deliver reproducible builds, strong performance, and practical developer ergonomics

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
3. Design and operate a production-grade CLI
4. Benchmark and optimize

## Expected Outputs

- Design documents
- Rust implementations of core components
- Minimal CLI
- Sample projects

## Crate Layout

- `src/core.rs`: tokenization, AST, and core analysis utilities
- `src/scanner.rs`: content scanning and class extraction
- `src/generator.rs`: utility class generation and CSS output
- `src/config.rs`: configuration, theme, and plugin loading
- `src/lib.rs`: CLI orchestration and build pipeline integration
- `src/main.rs`: binary entrypoint (`ironframe`)

## CLI Usage

### Run

- Run without installing:
  - `cargo run -- <command> [options]`
- Install to your Cargo bin directory:
  - `cargo install --path .`
  - `ironframe --help`
- Build release binary:
  - `cargo build --release`
  - `./target/release/ironframe --help`

### Commands

```text
ironframe scan [--ignore <glob>] <glob...>
ironframe build [--output <path>] [--minify] [--input-css <path>] [--compare-css <path>] [--config <path>] [--ignore <glob>] <glob...>
ironframe watch [--output <path>] [--minify] [--input-css <path>] [--config <path>] [--ignore <glob>] [--poll] [--poll-interval <ms>] <glob...>
```

- `scan`
  - scans files and prints unique class candidates to stdout
- `build`
  - generates CSS once
  - writes to stdout by default, or to `--output <path>` (alias: `--out`, `-o`) when specified
  - `--input-css <path>` (aliases: `-i`, `-s`) enables processing for `@import`, `@source`, `@theme`, and `@apply`
  - `--compare-css <path>` compares generated CSS with a reference CSS and fails if line 2 onward differs (line 1 header is ignored)
  - `--config <path>` loads TOML config (default: built-in config)
  - `--ignore <glob>` can be passed multiple times
  - `--minify` emits minified output
- `watch`
  - runs an initial build, then rebuilds on file changes
  - supports the same options as `build`
  - `--poll` enables polling mode (useful where filesystem notifications are unreliable)
  - `--poll-interval <ms>` sets polling interval (default: `500`)

### Examples

```bash
# Scan class candidates
ironframe scan "src/**/*.{html,tsx}"

# Build once to a file
ironframe build --output dist/tailwind.css "src/**/*.{html,tsx}"

# Build with template CSS and config
ironframe build -i src/app.css -c ironframe.toml -o dist/app.css "src/**/*.{html,tsx}"

# Build and compare with Tailwind output (line 1 is ignored)
ironframe build -i src/app.css -o dist/app.css --compare-css dist/app.tailwind.css "src/**/*.{html,tsx}"

# Watch mode (native watcher)
ironframe watch --output dist/tailwind.css "src/**/*.{html,tsx}"

# Watch mode with polling
ironframe watch --poll --poll-interval 250 "src/**/*.{html,tsx}"
```

When using `build --input-css`, the CSS file must include `@import "tailwindcss"` or `@import "ironframe"` (including split imports such as `tailwindcss/theme.css`).

## Minimal API (Draft)

- `ironframe::core`
  - `Token` and `TokenKind` for lexer output
  - `AstNode` for parsed structures
  - `ParseError` and `Diagnostic`
  - `parse(input: &str) -> Result<AstNode, ParseError>`
- `ironframe::scanner`
  - `ScanTarget` and `ScanResult`
  - `ScanGlobOptions`
  - `scan(paths: &[PathBuf]) -> Result<ScanResult, ScanError>`
  - `scan_globs(patterns: &[String]) -> Result<ScanResult, ScanError>`
  - `scan_globs_with_ignore(patterns: &[String], ignore: &[String]) -> Result<ScanResult, ScanError>`
  - `scan_globs_with_options(patterns: &[String], ignore: &[String], options: &ScanGlobOptions) -> Result<ScanResult, ScanError>`
  - `extract_classes(text: &str) -> Vec<String>`
- `ironframe::generator`
  - `GeneratorConfig` and `GenerationResult`
  - `generate(classes: &[String], config: &GeneratorConfig) -> GenerationResult`
  - `emit_css(result: &GenerationResult) -> String`
- `ironframe::config`
  - `Config` and `Theme`
  - `load(path: &Path) -> Result<Config, ConfigError>`
  - `resolve_theme(config: &Config) -> Theme`
- `ironframe` (library crate)
  - binary command: `ironframe`
  - `main` with subcommands: `scan`, `build`, `watch`
  - `build` wires: `config -> scanner -> generator -> css`

## Notes

This repository references Tailwind CSS v4.1 for behavior and ideas but is not affiliated with the official implementation.
ironframe is intended for production use and prioritizes high output compatibility with Tailwind CSS while keeping a Rust-only toolchain option.

`ironframe build --input-css` supports Tailwind-style `@source` directives for:
- explicit source registration (`@source "../path"`)
- source exclusion (`@source not "../path"`)
- disabling auto detection (`@import "tailwindcss" source(none)`)
- base path override (`@import "tailwindcss" source("../src")`)
- inline safelist / blocklist via brace expansion (`@source inline(...)`, `@source not inline(...)`)

`ironframe build --input-css` also supports:
- local CSS import inlining (`@import "./tokens.css";`)
- utility inlining with `@apply` for known utilities (`@apply rounded-b-lg shadow-md;`)
- Tailwind-style framework import splitting:
  - `@import "tailwindcss";` injects theme + preflight + utilities
  - `@import "tailwindcss/theme.css";`, `@import "tailwindcss/preflight.css";`, `@import "tailwindcss/utilities.css";` can be imported individually
  - omitting `preflight.css` disables preflight injection
- build-time functions in CSS:
  - `--alpha(var(--color-lime-300) / 50%)` -> `color-mix(in oklab, var(--color-lime-300) 50%, transparent)`
  - `--spacing(4)` -> `calc(var(--spacing) * 4)`

## Example Config

`ironframe.toml`:

```toml
[theme]
name = "default"

[theme.colors.gray]
100 = "#f3f4f6"
500 = "#6b7280"
```

## License

MIT OR Apache-2.0
