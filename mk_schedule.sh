#!/usr/bin/env bash
# mk_schedule.sh — build a randomized schedule of (profile, recipe, seed)

OUT=run_schedule.tsv
BASE_SEED=${1:-20250}

PROFILES=("C1" "C2" "C3" "C4" "C5")
RECIPES=("apple_pie" "parisian_gnocchi" "fried_chicken" "duck_a_lorange" "savory_cheese_souffle" "pesto_alla_genovese")

# Build full grid (5 * 6 * 10 = 300 lines), one conversation per line.
# We’ll assign seeds BASE_SEED+1 ... BASE_SEED+300.
> "$OUT"
seed=$BASE_SEED
for p in "${PROFILES[@]}"; do
  for r in "${RECIPES[@]}"; do
    for i in $(seq 1 10); do
      seed=$((seed+1))
      echo -e "${p}\t${r}\t${seed}" >> "$OUT"
    done
  done
done

# Shuffle to randomize execution order
shuf "$OUT" -o "$OUT"

echo "Wrote randomized schedule to $OUT (n=$(wc -l < $OUT))"
