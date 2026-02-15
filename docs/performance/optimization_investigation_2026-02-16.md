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

## Follow-up Optimization: Effect Utility Fast Path (2026-02-16)

### Implemented changes

File: `src/generator.rs`

1. Added `is_effect_fast_path_candidate` for effect-heavy families:
   - `rounded*`
   - `shadow*` / `inset-shadow*`
   - `ring*` / `inset-ring*`
2. Added `generate_effect_fast_path_rule` with precedence preserved:
   - `generate_custom_utility_rule(...)` first
   - then effect-family generators
3. Added preset generators to avoid full fallback-chain traversal for common classes:
   - `generate_shadow_preset_rule` (`shadow`, `shadow-sm`, `shadow-md`, etc.)
   - `generate_ring_preset_rule` (`ring`, `ring-0`, `ring-1`, `ring-2`, `ring-4`, `ring-8`)
4. Added regression test:
   - `custom_utility_keeps_priority_over_effect_utilities`

### Validation

- `cargo test`: **307 passed, 0 failed**

### Benchmarks

Comparison source:

- Previous: `bench/results/perf_2026-02-16_06-54-00.tsv`
- Current: `bench/results/perf_2026-02-16_06-58-27.tsv`

| Case | Previous avg (s) | Current avg (s) | Delta |
|---|---:|---:|---:|
| build_large_html | 0.0853 | 0.0840 | -1.5% |
| build_unique_massive | 0.0688 | 0.0641 | -6.8% |
| build_unique_massive_minify | 0.0457 | 0.0457 | +0.0% |
| build_mixed_all | 0.0298 | 0.0302 | +1.3% |

Result:

- Large and unique-heavy build workloads improved further.
- Mixed workload is roughly flat with slight run-to-run fluctuation.

## Follow-up Optimization: Sort Key Reparse Reduction (2026-02-16)

### Implemented changes

File: `src/generator.rs`

1. Parsed variants only once per class in `generate_with_overrides`, then reused parsed values for generation and sort-key creation.
2. Changed `generate_rule` to accept parsed `variants`/`base_with_modifier` and return `(generated_rule, property_rank)`.
3. Changed `build_rule_sort_key` to accept precomputed `variants`, `base`, and `property_rank` so it no longer reparses:
   - class variants (`parse_variants`)
   - generated rule declaration property (`extract_primary_declaration_property`)
4. Added `variant_injects_generated_content` to preserve property-rank behavior for `before`/`after` variants while avoiding redundant rule parsing.

### Validation

- `cargo test`: **307 passed, 0 failed**

### Benchmarks

Full suite comparison:

- Baseline (clean `HEAD` worktree): `bench/results/perf_2026-02-16_07-15-03_baseline_sortkey.tsv`
- Current (this optimization): `bench/results/perf_2026-02-16_07-16-23.tsv`

| Case | Baseline avg (s) | Current avg (s) | Delta |
|---|---:|---:|---:|
| build_large_html | 0.0902 | 0.0867 | -3.9% |
| build_mixed_all | 0.0301 | 0.0300 | -0.3% |
| build_mixed_input_css | 0.0333 | 0.0316 | -5.1% |
| build_mixed_minify | 0.0290 | 0.0294 | +1.4% |
| build_small_html | 0.0068 | 0.0071 | +4.4% |
| build_unique_massive | 0.0654 | 0.0635 | -2.9% |
| build_unique_massive_minify | 0.0455 | 0.0451 | -0.9% |

Targeted recheck (higher repetition, focused build hot paths):

- `build_unique_massive`: `0.065000s -> 0.063604s` (`-2.15%`)
- `build_unique_massive_minify`: `0.045875s -> 0.045312s` (`-1.23%`)
- `build_mixed_all`: `0.030750s -> 0.030163s` (`-1.91%`)

Result:

- This optimization produces a modest but consistent improvement in generator-heavy build cases (about `1-3%` in focused runs).
- The effect size is smaller than earlier fast-path optimizations, but it reduces repeated parsing work on the generation hot path with no correctness regression.

## Follow-up Optimization: Selector Generation Lazy Across `generate_*` (2026-02-16)

### Implemented changes

File: `src/generator.rs`

1. Added `LazyClassSelector` (backed by `OnceCell<String>`) and `Deref<Target = str>` so selector escaping/allocation happens on first use, not at function entry.
2. Replaced eager selector initialization in `generate_*` helpers from:
   - `let selector = format!(".{}", escape_selector(class));`
   to:
   - `let selector = LazyClassSelector::new(class);`
3. Kept existing rule-generation logic unchanged (`rule(&selector, ...)`, composed helpers, gradient helpers), so behavior is preserved while avoiding selector work on mismatch paths.

### Validation

- `cargo test`: **307 passed, 0 failed**

### Benchmarks

Full suite comparison:

- Baseline (clean `HEAD` worktree): `bench/results/perf_2026-02-16_07-26-01_baseline_selector_lazy.tsv`
- Current (this optimization): `bench/results/perf_2026-02-16_07-27-34.tsv`

| Case | Baseline avg (s) | Current avg (s) | Delta |
|---|---:|---:|---:|
| build_small_html | 0.0080 | 0.0064 | -20.0% |
| build_mixed_all | 0.0320 | 0.0313 | -2.2% |
| build_mixed_minify | 0.0315 | 0.0288 | -8.6% |
| build_mixed_input_css | 0.0339 | 0.0293 | -13.6% |
| build_large_html | 0.0880 | 0.0815 | -7.4% |
| build_unique_massive | 0.0638 | 0.0617 | -3.3% |
| build_unique_massive_minify | 0.0454 | 0.0442 | -2.6% |

Targeted recheck (higher repetition, focused build hot paths):

- `build_unique_massive`: `0.061333s -> 0.060875s` (`-0.75%`)
- `build_unique_massive_minify`: `0.043771s -> 0.042771s` (`-2.28%`)
- `build_mixed_all`: `0.028675s -> 0.026538s` (`-7.45%`)

Result:

- Selector lazy generation reduces wasted work on mismatch-heavy paths and improves end-to-end build performance.
- Effect size is workload-dependent: strongest on mixed workloads, smaller but positive on unique-heavy hot paths.

## Follow-up Optimization: `apply_variants` No-Variant Super Early Return (2026-02-16)

### Implemented changes

File: `src/generator.rs`

1. Added fast path in `generate_rule` to bypass `apply_variants` entirely for the dominant path:
   - `variants.is_empty() && !important_modifier`
2. Adjusted `apply_variants` to return immediately before unwrapping/parsing when:
   - `variants.is_empty() && full_class == base_class`

### Validation

- `cargo test`: **307 passed, 0 failed**

### Benchmarks

Full suite comparison (same case set/loops, before-vs-after binaries):

- Before: `bench/results/perf_2026-02-16_07-35-27_before_no_variant_fast_return.tsv`
- After: `bench/results/perf_2026-02-16_07-35-27_after_no_variant_fast_return.tsv`

| Case | Before avg (s) | After avg (s) | Delta |
|---|---:|---:|---:|
| build_large_html | 0.0832 | 0.0830 | -0.2% |
| build_mixed_all | 0.0293 | 0.0286 | -2.4% |
| build_mixed_input_css | 0.0324 | 0.0313 | -3.4% |
| build_mixed_minify | 0.0299 | 0.0283 | -5.4% |
| build_small_html | 0.0056 | 0.0064 | +14.3% |
| build_unique_massive | 0.0643 | 0.0622 | -3.3% |
| build_unique_massive_minify | 0.0437 | 0.0443 | +1.4% |

Targeted rechecks (higher repetition, build-only):

1) Sequential high-repeat sample:

- `build_small_html`: `0.006943s -> 0.006714s` (`-3.30%`)
- `build_mixed_all`: `0.031383s -> 0.029967s` (`-4.51%`)
- `build_mixed_minify`: `0.027867s -> 0.029350s` (`+5.32%`)
- `build_unique_massive`: `0.063017s -> 0.063900s` (`+1.40%`)
- `build_unique_massive_minify`: `0.046500s -> 0.044900s` (`-3.44%`)

2) Alternating base/curr recheck (drift suppression):

- `build_small_html`: `0.007330s -> 0.007102s` (`-3.11%`)
- `build_mixed_all`: `0.030536s -> 0.030625s` (`+0.29%`)
- `build_unique_massive`: `0.064116s -> 0.064197s` (`+0.13%`)

Result:

- Improvement is small and not consistently directional across all build workloads.
- This optimization appears to be neutral-to-small in practice on the current codebase/host conditions.
- Decision: reverted from the working tree and deprioritized in favor of more stable optimizations.

## Follow-up Optimization Trial: Fallback Family Dispatch (2026-02-16)

### Implemented changes (trial)

File: `src/generator.rs`

- Added `generate_family_dispatch_rule` and invoked it at the start of the large fallback `or_else` chain in `generate_rule`.
- Dispatch targeted major prefixes (`text-*`, `bg-*`, `grid-*`, transform family, `mask-*`, `backdrop-*`, `fill/stroke`, `accent/caret`, `scroll-*`, `border/outline`, `object-*`) before falling back to the original chain.

### Validation

- `cargo test`: **307 passed, 0 failed**

### Benchmarks

Full suite comparison:

- Before: `bench/results/perf_2026-02-16_07-57-28_before_family_dispatch.tsv`
- After: `bench/results/perf_2026-02-16_07-57-28_after_family_dispatch.tsv`

| Case | Before avg (s) | After avg (s) | Delta |
|---|---:|---:|---:|
| build_small_html | 0.0060 | 0.0066 | +10.0% |
| build_mixed_all | 0.0276 | 0.0295 | +6.9% |
| build_mixed_minify | 0.0281 | 0.0290 | +3.2% |
| build_mixed_input_css | 0.0336 | 0.0316 | -6.0% |
| build_large_html | 0.0855 | 0.0860 | +0.6% |
| build_unique_massive | 0.0637 | 0.0645 | +1.3% |
| build_unique_massive_minify | 0.0439 | 0.0452 | +3.0% |

Alternating high-repeat recheck (drift suppression):

- `build_small_html`: `0.006739s -> 0.006548s` (`-2.83%`)
- `build_mixed_all`: `0.028250s -> 0.028323s` (`+0.26%`)
- `build_mixed_minify`: `0.028318s -> 0.028526s` (`+0.73%`)
- `build_unique_massive`: `0.062795s -> 0.063839s` (`+1.66%`)
- `build_unique_massive_minify`: `0.044420s -> 0.043902s` (`-1.17%`)

Result:

- Effects were inconsistent and skewed toward regression on key build cases in full-suite measurements.
- Decision: not adopted; reverted from the working tree.

## Follow-up Optimization: Scanner Traversal Cost Reduction (2026-02-16)

### Implemented changes

File: `src/scanner.rs`

1. Removed `seen_paths` dedup in `scan_globs_with_options` (single-pass walker path collection no longer needs path-level dedup).
2. Added directory-level prune for `node_modules` via `WalkBuilder::filter_entry` when `include_node_modules = false`.
3. Removed file-level `node_modules` exclusion in `should_skip_file` and centralized that concern in traversal pruning.

### Validation

- `cargo test`: **307 passed, 0 failed**

### Benchmarks (alternating before/after runs)

Binary baseline: `/tmp/ironframe_before_scanner_prune`  
Binary current: `/tmp/ironframe_after_scanner_prune_v2`

| Case | Before avg (s) | After avg (s) | Delta |
|---|---:|---:|---:|
| scan_small_html | 0.006635 | 0.006669 | +0.51% |
| scan_mixed_all | 0.028781 | 0.028306 | -1.65% |
| scan_large_html | 0.085590 | 0.083229 | -2.76% |
| scan_unique_massive | 0.023595 | 0.023619 | +0.10% |
| build_mixed_all | 0.029602 | 0.029602 | +0.00% |
| build_mixed_minify | 0.030472 | 0.031000 | +1.73% |

Result:

- Scanner-focused cases (`scan_mixed_all`, `scan_large_html`) improved modestly.
- End-to-end build impact is near-neutral with small case-dependent variance.
