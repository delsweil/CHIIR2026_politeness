#!/bin/bash
# run_all_batches_skip.sh
set -euo pipefail

ROOT="/home/david/final_first_three_batches/.."   # adjust to parent dir with Final_study_* subdirs
STEP_LOOKUP="step_lookup.csv"
MODEL="deepseek-r1:8b"

# batches to skip
SKIP=("Final_study_1" "Final_study_10")

for batch_dir in "$ROOT"/Final_study_*; do
  [ -d "$batch_dir" ] || continue
  bname=$(basename "$batch_dir")

  # skip if in SKIP list
  if printf '%s\n' "${SKIP[@]}" | grep -qx "$bname"; then
    echo "[skip] $bname"
    continue
  fi

  files_glob="$batch_dir/${bname}_dialogs_flat_*.csv"
  out_csv="agent_nuggets_by_step_${bname}.csv"
  log="nugget_counter_${bname}.log"

  echo "[run] $bname"
  nohup Rscript nugget_counter_orig.R \
    files="$files_glob" \
    step_lookup="$STEP_LOOKUP" \
    out="$out_csv" \
    model="$MODEL" \
    max_rows=100000 \
    > "$log" 2>&1 &
done

echo "Started $(jobs -p | wc -l) jobs. Tail logs with: tail -f nugget_counter_*.log"
