#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CASE_DIR="${CASE_DIR:-$ROOT_DIR/bench/cases}"
RESULT_DIR="${RESULT_DIR:-$ROOT_DIR/bench/results}"
RUNS="${RUNS:-6}"
BUILD_RELEASE="${BUILD_RELEASE:-1}"
BIN="$ROOT_DIR/target/release/ironframe"

mkdir -p "$RESULT_DIR"
"$ROOT_DIR/bench/generate_perf_cases.sh" "$CASE_DIR" >/dev/null

if [ "$BUILD_RELEASE" = "1" ] || [ ! -x "$BIN" ]; then
  cargo build --release >/dev/null
fi

timestamp="$(date +%Y-%m-%d_%H-%M-%S)"
TSV_PATH="$RESULT_DIR/perf_${timestamp}.tsv"
MD_PATH="$RESULT_DIR/perf_${timestamp}.md"
LATEST_TSV="$RESULT_DIR/latest.tsv"
LATEST_MD="$RESULT_DIR/latest.md"

echo -e "case\tworkdir\tscanned_files\tclass_count\tinner_loops\tavg_s\tmedian_s\tmin_s\tmax_s\tfiles_per_sec" > "$TSV_PATH"

bench_case() {
  local name="$1"
  local workdir="$2"
  local cmd="$3"
  local inner_loops="$4"

  local meta
  meta="$(cd "$workdir" && eval "$cmd" 2>&1 >/dev/null)"
  local scanned_files
  scanned_files="$(printf "%s\n" "$meta" | sed -n 's/.*scanned \([0-9][0-9]*\) files, found \([0-9][0-9]*\) classes.*/\1/p' | tail -n1)"
  local class_count
  class_count="$(printf "%s\n" "$meta" | sed -n 's/.*scanned \([0-9][0-9]*\) files, found \([0-9][0-9]*\) classes.*/\2/p' | tail -n1)"
  if [ -z "$scanned_files" ]; then
    scanned_files="NA"
  fi
  if [ -z "$class_count" ]; then
    class_count="NA"
  fi

  (cd "$workdir" && eval "$cmd" >/dev/null 2>/dev/null)

  local vals=()
  local i
  for i in $(seq 1 "$RUNS"); do
    local total_time
    total_time="$(
      /usr/bin/time -p bash -lc "cd '$workdir' && for j in \$(seq 1 $inner_loops); do $cmd >/dev/null 2>/dev/null; done" 2>&1 \
        | awk '/^real /{print $2}'
    )"
    vals+=("$(awk -v t="$total_time" -v n="$inner_loops" 'BEGIN{printf "%.6f", t/n}')")
  done

  local avg_s
  avg_s="$(printf "%s\n" "${vals[@]}" | awk 'NF{sum+=$1;n++} END{if(n>0) printf "%.4f", sum/n; else print "NA"}')"
  local median_s
  median_s="$(printf "%s\n" "${vals[@]}" | sort -n | awk '{a[NR]=$1} END{if(NR==0){print "NA"} else if (NR%2==1){printf "%.4f", a[(NR+1)/2]} else {printf "%.4f", (a[NR/2]+a[NR/2+1])/2}}')"
  local min_s
  min_s="$(printf "%s\n" "${vals[@]}" | awk 'NF{if(min=="" || $1<min) min=$1} END{if(min=="") print "NA"; else printf "%.4f", min}')"
  local max_s
  max_s="$(printf "%s\n" "${vals[@]}" | awk 'NF{if(max=="" || $1>max) max=$1} END{if(max=="") print "NA"; else printf "%.4f", max}')"

  local files_per_sec
  if [ "$scanned_files" = "NA" ]; then
    files_per_sec="NA"
  else
    files_per_sec="$(awk -v f="$scanned_files" -v t="$avg_s" 'BEGIN{if(t>0) printf "%.1f", f/t; else print "NA"}')"
  fi

  echo -e "$name\t$workdir\t$scanned_files\t$class_count\t$inner_loops\t$avg_s\t$median_s\t$min_s\t$max_s\t$files_per_sec" >> "$TSV_PATH"
}

bench_case "scan_small_html" "$CASE_DIR/case_small" "$BIN scan 'content/**/*.html'" 120
bench_case "scan_mixed_all" "$CASE_DIR/case_mixed" "$BIN scan 'content/**/*'" 40
bench_case "scan_large_html" "$CASE_DIR/case_large" "$BIN scan 'content/**/*.html'" 10
bench_case "scan_unique_massive" "$CASE_DIR/case_unique" "$BIN scan 'content/**/*'" 20
bench_case "build_small_html" "$CASE_DIR/case_small" "$BIN build -o out.css 'content/**/*.html'" 120
bench_case "build_mixed_all" "$CASE_DIR/case_mixed" "$BIN build -o out.css 'content/**/*'" 40
bench_case "build_mixed_minify" "$CASE_DIR/case_mixed" "$BIN build --minify -o out.min.css 'content/**/*'" 40
bench_case "build_mixed_input_css" "$CASE_DIR/case_mixed" "$BIN build -i template/app.css -o out.input.css 'content/**/*'" 30
bench_case "build_large_html" "$CASE_DIR/case_large" "$BIN build -o out.css 'content/**/*.html'" 10
bench_case "build_unique_massive" "$CASE_DIR/case_unique" "$BIN build -o out.css 'content/**/*'" 20
bench_case "build_unique_massive_minify" "$CASE_DIR/case_unique" "$BIN build --minify -o out.min.css 'content/**/*'" 20

{
  echo "# Performance Benchmark Results"
  echo
  echo "- Date: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- Commit: $(git rev-parse --short HEAD)"
  echo "- CPU: $(sysctl -n machdep.cpu.brand_string 2>/dev/null || echo 'unknown')"
  echo "- rustc: $(rustc --version)"
  echo "- cargo: $(cargo --version)"
  echo "- Binary: \`$BIN\`"
  echo "- Runs per case: $RUNS (with inner loop normalization)"
  echo
  echo "| Case | Scanned files | Classes | Avg (ms) | Median (ms) | Min (ms) | Max (ms) | Files/s |"
  echo "|---|---:|---:|---:|---:|---:|---:|---:|"
  tail -n +2 "$TSV_PATH" | while IFS=$'\t' read -r case workdir scanned classes loops avg median min max fps; do
    avg_ms="$(awk -v x="$avg" 'BEGIN{printf "%.2f", x*1000}')"
    median_ms="$(awk -v x="$median" 'BEGIN{printf "%.2f", x*1000}')"
    min_ms="$(awk -v x="$min" 'BEGIN{printf "%.2f", x*1000}')"
    max_ms="$(awk -v x="$max" 'BEGIN{printf "%.2f", x*1000}')"
    echo "| $case | $scanned | $classes | $avg_ms | $median_ms | $min_ms | $max_ms | $fps |"
  done
  echo
  echo "## Raw TSV"
  echo
  echo "\`$TSV_PATH\`"
} > "$MD_PATH"

cp "$TSV_PATH" "$LATEST_TSV"
cp "$MD_PATH" "$LATEST_MD"

echo "TSV: $TSV_PATH"
echo "Markdown: $MD_PATH"
echo "Latest TSV: $LATEST_TSV"
echo "Latest Markdown: $LATEST_MD"
