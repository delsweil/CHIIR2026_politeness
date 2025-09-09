#!/usr/bin/env bash
# mk_schedule_final_exp.sh — build a randomized schedule of (profile, recipe, model, seed)
# Total conversations: 5 profiles * 6 recipes * 3 models * 200 reps = 18,000
# Output: randomly ordered master TSV + 20 files Final_study_1.tsv ... Final_study_20.tsv (each 900 rows)

set -euo pipefail

OUT_MASTER="run_schedule_final.tsv"
BASE_SEED=${1:-20250}

PROFILES=("C1" "C2" "C3" "C4" "C5")
RECIPES=("apple_pie" "parisian_gnocchi" "fried_chicken" "duck_a_lorange" "savory_cheese_souffle" "pesto_alla_genovese")
MODELS=("qwen2.5:7b-instruct" "deepseek-r1:8b" "llama3.1:8b-instruct-q4_K_M")

# Build full grid (5 * 6 * 3 * 200 = 18,000 lines), 1 conversation per line.
# Seeds will be BASE_SEED+1 ... BASE_SEED+18000.
> "$OUT_MASTER"
seed=$BASE_SEED
for p in "${PROFILES[@]}"; do
  for r in "${RECIPES[@]}"; do
    for m in "${MODELS[@]}"; do
      for i in $(seq 1 200); do
        seed=$((seed+1))
        printf "%s\t%s\t%s\t%d\n" "$p" "$r" "$m" "$seed" >> "$OUT_MASTER"
      done
    done
  done
done

# Shuffle to randomize execution order
shuf "$OUT_MASTER" -o "$OUT_MASTER"

# Split into 20 files, each 900 rows: Final_study_1.tsv ... Final_study_20.tsv
tmpdir="$(mktemp -d)"
split -d -l 900 --additional-suffix=.tsv "$OUT_MASTER" "$tmpdir/Final_study_"
# Expecting files Final_study_00.tsv ... Final_study_19.tsv
for idx in $(seq 0 19); do
  src="$(printf "%s/Final_study_%02d.tsv" "$tmpdir" "$idx")"
  dest="$(printf "Final_study_%d.tsv" $((idx+1)))"
  mv "$src" "$dest"
done
rmdir "$tmpdir"

echo "Wrote randomized master schedule to $OUT_MASTER (n=$(wc -l < "$OUT_MASTER"))"
echo "Wrote 20 split files: Final_study_1.tsv ... Final_study_20.tsv (each 900 rows)"
