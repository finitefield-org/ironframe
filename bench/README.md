# Performance Benchmarks

This directory contains reproducible performance benchmark tooling.

## Scripts

- `bench/generate_perf_cases.sh`
  - Generates deterministic benchmark cases into `bench/cases/`
- `bench/run_perf_benchmarks.sh`
  - Regenerates cases, runs benchmark cases, and writes:
    - `bench/results/latest.tsv`
    - `bench/results/latest.md`

## Usage

```bash
# Generate benchmark cases only
./bench/generate_perf_cases.sh

# Run full benchmark suite
./bench/run_perf_benchmarks.sh
```

Optional environment variables:

- `RUNS` (default: `6`)
- `CASE_DIR` (default: `bench/cases`)
- `RESULT_DIR` (default: `bench/results`)
- `BUILD_RELEASE` (`1` or `0`, default: `1`)
