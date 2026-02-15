# Performance Optimization Investigation (2026-02-16)

## Environment

- Baseline TSV: `bench/results/perf_2026-02-16_00-58-51.tsv`
- Optimized TSV: `bench/results/perf_2026-02-16_05-58-16.tsv`
- Commit: `dd5fdc8`
- CPU: Apple M1 Max
- rustc: `rustc 1.92.0 (ded5c06cf 2025-12-08)`
- cargo: `cargo 1.92.0 (344c4567c 2025-10-21)`

## Baseline Findings

Large unique-class workload was generator-dominant:

- `scan_unique_massive`: `0.0300s`
- `build_unique_massive`: `0.3195s`

Additional focused experiments before implementation showed:

- `w-[Npx]` classes were much slower than early-matching classes due to late fallback resolution.
- Noise-heavy extracted candidates caused non-trivial build cost even when almost no CSS was emitted.

## Implemented Changes

### 1) Fast path for sizing/gap/space utilities

File: `src/generator.rs`

- Added `is_layout_fast_path_candidate` and `generate_layout_fast_path_rule`.
- In `generate_rule`, classes in this candidate set are now handled early via:
  - `generate_custom_utility_rule` (keeps custom utility precedence)
  - then `generate_sizing_rule` / `generate_gap_rule` / `generate_space_rule`

Targeted prefixes:

- `w-`, `h-`, `min-w-`, `max-w-`, `min-h-`, `max-h-`, `size-`
- `gap-`
- `space-x-`, `space-y-`, `-space-x-`, `-space-y-`

### 2) Lazy selector allocation in hot sizing path

File: `src/generator.rs`

- Added `class_rule(class, declarations, config)` helper.
- Reworked `generate_sizing_rule`, `generate_gap_rule`, and `generate_space_rule` to avoid eager selector creation on entry.

### 3) Regression test for precedence

File: `src/generator.rs`

- Added `custom_utility_keeps_priority_over_sizing_utilities`.
- Verifies custom utility `w-4` overrides built-in sizing behavior.

## Validation

- `cargo test`: **302 passed, 0 failed**

## Benchmark Comparison (Before vs After)

Source:

- Before: `bench/results/perf_2026-02-16_00-58-51.tsv`
- After: `bench/results/perf_2026-02-16_05-58-16.tsv`

| Case | Before avg (s) | After avg (s) | Delta |
|---|---:|---:|---:|
| scan_small_html | 0.0072 | 0.0069 | -4.2% |
| scan_mixed_all | 0.0330 | 0.0315 | -4.5% |
| scan_large_html | 0.0938 | 0.0917 | -2.2% |
| scan_unique_massive | 0.0300 | 0.0232 | -22.7% |
| build_small_html | 0.0126 | 0.0077 | -38.9% |
| build_mixed_all | 0.0348 | 0.0362 | +4.0% |
| build_mixed_minify | 0.0341 | 0.0334 | -2.1% |
| build_mixed_input_css | 0.0360 | 0.0360 | +0.0% |
| build_large_html | 0.0935 | 0.0942 | +0.7% |
| build_unique_massive | 0.3195 | 0.1416 | -55.7% |
| build_unique_massive_minify | 0.3244 | 0.1210 | -62.7% |

## Post-Change Synthetic Recheck

Using the same synthetic setup as the initial investigation (1 file, 6000 classes per case, `build` command):

| Class pattern | Avg (s) |
|---|---:|
| early (`drop-shadow-[Npx]`) | 0.0580 |
| late (`w-[Npx]`) | 0.0266 |
| unknown (`definitely-unknown-N`) | 0.1539 |

`w-[Npx]` moved from a late-chain penalty case to a fast-path case and is now materially faster than the previous measurement (`0.1281s`).

## Summary

The implemented generator fast path significantly reduced the dominant unique-class build workload (`build_unique_massive`) while keeping correctness (full test pass + precedence regression test).

Remaining opportunities:

1. Reduce scanner false positives in mixed/noisy files.
2. Introduce broader dispatch indexing for non-sizing families.
3. Remove duplicate parse/sort string work in rule generation pipeline.
