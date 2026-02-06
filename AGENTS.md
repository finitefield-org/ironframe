# AGENTS.md

This is a guide for agents working in this repository. The goal is to implement Tailwind CSS v4.1-equivalent functionality in Rust.

## Purpose and Scope

- Reimplement Tailwind CSS v4.1-equivalent features in Rust
- Focus on JIT, content scanning, utility generation, variants, configuration/theme/plugins
- Do not copy official implementation code; build based on understanding and redesign

## Expected Deliverables

- Rust code (core libraries + CLI)
- Design notes and specification documents
- Sample inputs/outputs and lightweight benchmarks

## Implementation Direction (Recommended)

- Split core into `crates/`
- Example layout
  - `crates/core`: tokenization/AST/analysis
  - `crates/scanner`: content scanning
  - `crates/generator`: utility generation
  - `crates/config`: config/theme/plugins
  - `crates/cli`: CLI
- Start with “minimal JIT + limited utilities + one variant”

## Quality Bar

- Reproducibility: same input yields same output
- Performance: add measurement for scanning and generation
- Extensibility: easy to add utilities and variants

## Development Rules

- Keep changes small; update tests/samples together
- Keep README and design docs current
- Prefer minimal dependencies

## When to Consult

- When specification decisions are required (e.g., arbitrary value syntax, variant priority)
- When behavior may differ from Tailwind

## Communication Guidelines

- Share purpose and outcomes concisely
- State assumptions when uncertain
