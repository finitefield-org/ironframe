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

## Follow-up Optimization: Inset Fast Path + Variant Early Return (2026-02-16)

### Implemented changes

File: `src/generator.rs`

1. Expanded layout fast-path candidate prefixes to include inset-position families:
   - `inset-*`, `inset-x-*`, `inset-y-*`
   - `start-*`, `end-*`, `top-*`, `right-*`, `bottom-*`, `left-*`
   - plus negative forms (`-inset-*`, `-top-*`, etc.)
2. Added `generate_inset_rule` to `generate_layout_fast_path_rule` dispatch.
3. Optimized `generate_inset_rule` to use `class_rule(...)` and avoid eager selector allocation before successful parse.
4. Added early return in `apply_variants` when `variants.is_empty()` and `full_class == base_class` to skip unnecessary selector/header work.
5. Added regression test:
   - `custom_utility_keeps_priority_over_inset_utilities`

### Validation

- `cargo test`: **305 passed, 0 failed**

### Benchmarks

Full suite (same benchmark harness):

- Previous: `bench/results/perf_2026-02-16_06-32-42.tsv`
- Current: `bench/results/perf_2026-02-16_06-42-04.tsv`

Key deltas (avg):

- `build_unique_massive`: `0.1419s -> 0.0742s` (`-47.7%`)
- `build_unique_massive_minify`: `0.1264s -> 0.0463s` (`-63.4%`)

Note: `build_large_html` has run-to-run variance in the full suite. Controlled A/B below is used for stronger comparison.

### Controlled A/B (baseline worktree vs optimized worktree)

Baseline binary from clean `HEAD` worktree:

- `/tmp/ironframe_baseline_head/target/release/ironframe`

Optimized binary:

- `target/release/ironframe`

Measured with fixed loops and 8 runs:

| Case | Baseline avg (s) | Optimized avg (s) | Delta |
|---|---:|---:|---:|
| build_unique | 0.1382 | 0.0650 | -53.0% |
| build_unique_minify | 0.1204 | 0.0456 | -62.1% |
| build_large | 0.0845 | 0.0880 | +4.1% |

Interpretation:

- This optimization strongly improves unique-class-heavy builds (the known hot workload).
- There is a small regression on large HTML build in this controlled sample; further balancing should target dispatch narrowing for non-inset/non-sizing classes to recover this.

## Follow-up Optimization: Spacing Dispatch Fast Path (2026-02-16)

### Implemented changes

File: `src/generator.rs`

1. Added `is_spacing_fast_path_candidate` for spacing families:
   - `p-*`, `px-*`, `py-*`, `pt-*`, `pr-*`, `pb-*`, `pl-*`, `ps-*`, `pe-*`
   - `m-*`, `mx-*`, `my-*`, `mt-*`, `mr-*`, `mb-*`, `ml-*`, `ms-*`, `me-*`
   - negative margin forms (e.g. `-mt-*`, `-mx-*`)
2. Added `generate_spacing_fast_path_rule`:
   - preserves custom utility precedence via `generate_custom_utility_rule(...)` first
   - then `generate_spacing_rule(...)`
3. Inserted spacing fast-path branch in `generate_rule` before the large fallback chain.
4. Optimized `generate_spacing_rule` to use `class_rule(...)` (selector creation only when matched).
5. Added regression test:
   - `custom_utility_keeps_priority_over_spacing_utilities`

### Validation

- `cargo test`: **306 passed, 0 failed**

### Benchmarks

Comparison source:

- Previous: `bench/results/perf_2026-02-16_06-42-04.tsv`
- Current: `bench/results/perf_2026-02-16_06-54-00.tsv`

| Case | Previous avg (s) | Current avg (s) | Delta |
|---|---:|---:|---:|
| build_small_html | 0.0079 | 0.0071 | -10.1% |
| build_mixed_all | 0.0322 | 0.0298 | -7.5% |
| build_mixed_minify | 0.0307 | 0.0290 | -5.5% |
| build_large_html | 0.0912 | 0.0853 | -6.5% |
| build_unique_massive | 0.0742 | 0.0688 | -7.3% |
| build_unique_massive_minify | 0.0463 | 0.0457 | -1.3% |

Result:

- The earlier `build_large_html` regression was recovered.
- Unique-class-heavy workloads remained improved while mixed and small workloads also improved.
