#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="${1:-$ROOT_DIR/bench/cases}"

rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR/case_small/content"
mkdir -p "$OUT_DIR/case_mixed/content"
mkdir -p "$OUT_DIR/case_large/content"
mkdir -p "$OUT_DIR/case_unique/content"

# case_small: 25 html files
for i in $(seq 1 25); do
  shade=$(( (i % 9 + 1) * 100 ))
  col=$(( i % 6 + 1 ))
  cat > "$OUT_DIR/case_small/content/page_$i.html" <<EOF
<div class="p-$((i%8)) m-$((i%6)) text-red-$shade bg-blue-$shade hover:bg-green-$shade md:grid-cols-$col rounded-lg shadow-md ring-$((i%4))"></div>
EOF
done

# case_mixed: html + tsx + md + yaml + noisy files
mkdir -p "$OUT_DIR/case_mixed/content/html"
mkdir -p "$OUT_DIR/case_mixed/content/tsx"
mkdir -p "$OUT_DIR/case_mixed/content/md"
mkdir -p "$OUT_DIR/case_mixed/content/yaml"
mkdir -p "$OUT_DIR/case_mixed/content/node_modules/lib"
mkdir -p "$OUT_DIR/case_mixed/content/assets"
for i in $(seq 1 400); do
  shade=$(( (i % 9 + 1) * 100 ))
  col=$(( i % 12 + 1 ))
  row=$(( i % 6 + 1 ))
  cat > "$OUT_DIR/case_mixed/content/html/page_$i.html" <<EOF
<section class="p-$((i%10)) m-$((i%8)) text-red-$shade bg-blue-$shade hover:bg-green-$shade md:grid-cols-$col lg:grid-rows-$row rounded-lg shadow-md ring-$((i%4)) opacity-$(( (i%10)*10 )) translate-x-$((i%8)) [--gutter-width:$((i%12+1))px]"></section>
EOF
done
for i in $(seq 1 220); do
  shade=$(( (i % 9 + 1) * 100 ))
  cat > "$OUT_DIR/case_mixed/content/tsx/view_$i.tsx" <<EOF
import clsx from "clsx";
export const View$i = ({ active }: { active: boolean }) => (
  <div className={clsx("p-$((i%10))", "text-red-$shade", active && "hover:bg-blue-$shade", {"md:mt-$((i%8))": active, "rounded-lg": true})}>x</div>
);
EOF
done
for i in $(seq 1 120); do
  cat > "$OUT_DIR/case_mixed/content/md/doc_$i.md" <<EOF
# doc $i
<div class="prose max-w-none text-sm md:gap-$((i%6))">content</div>
EOF
done
for i in $(seq 1 120); do
  shade=$(( (i % 9 + 1) * 100 ))
  cat > "$OUT_DIR/case_mixed/content/yaml/data_$i.yaml" <<EOF
card:
  class: "bg-cyan-$shade text-cyan-$shade"
  accentClass: "border-cyan-$shade"
  offsetClass: "md:mt-$((i%10))"
EOF
done
for i in $(seq 1 60); do
  echo '<div class="text-orange-500"></div>' > "$OUT_DIR/case_mixed/content/node_modules/lib/file_$i.html"
done
echo ".bg-red-500{}" > "$OUT_DIR/case_mixed/content/styles.css"
echo "{}" > "$OUT_DIR/case_mixed/content/package-lock.json"
echo "binary-ish" > "$OUT_DIR/case_mixed/content/assets/logo.png"
mkdir -p "$OUT_DIR/case_mixed/template"
cat > "$OUT_DIR/case_mixed/template/app.css" <<'EOF'
@import "tailwindcss";

@theme {
  --color-brand-500: #10b981;
}

.card {
  @apply rounded-lg shadow-md;
}

.button {
  @variant hover {
    color: --alpha(var(--color-brand-500) / 70%);
  }
}
EOF

# case_large: 3000 html files
for i in $(seq 1 3000); do
  shade=$(( (i % 9 + 1) * 100 ))
  cat > "$OUT_DIR/case_large/content/page_$i.html" <<EOF
<div class="p-$((i%10)) m-$((i%8)) text-red-$shade bg-blue-$shade hover:bg-green-$shade md:gap-$((i%8)) lg:mt-$((i%12)) rounded-lg shadow-md ring-$((i%4))"></div>
EOF
done

# case_unique: few files with many unique classes
classes=""
for i in $(seq 1 2500); do
  classes+=" w-[${i}px] h-[${i}px] top-[${i}px] left-[${i}px]"
  classes+=" md:w-[${i}px] lg:h-[${i}px]"
done
cat > "$OUT_DIR/case_unique/content/massive.html" <<EOF
<div class="$classes"></div>
EOF
cat > "$OUT_DIR/case_unique/content/view.tsx" <<'EOF'
export const View = () => <div className="p-4 m-2 text-red-500" />;
EOF
cat > "$OUT_DIR/case_unique/content/data.yaml" <<'EOF'
card:
  class: "bg-cyan-500 text-cyan-900"
EOF

small_count="$(find "$OUT_DIR/case_small/content" -type f | wc -l | tr -d ' ')"
mixed_count="$(find "$OUT_DIR/case_mixed/content" -type f | wc -l | tr -d ' ')"
large_count="$(find "$OUT_DIR/case_large/content" -type f | wc -l | tr -d ' ')"
unique_count="$(find "$OUT_DIR/case_unique/content" -type f | wc -l | tr -d ' ')"
printf "Generated cases at %s\n" "$OUT_DIR"
printf "case_small files: %s\n" "$small_count"
printf "case_mixed files: %s\n" "$mixed_count"
printf "case_large files: %s\n" "$large_count"
printf "case_unique files: %s\n" "$unique_count"
