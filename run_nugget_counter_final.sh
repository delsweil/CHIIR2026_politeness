#!/bin/bash
# run_all_batches_seq.sh
# Sequentially run nugget_counter_orig.R across all Final_study_* batches,
# skipping 1, 10, 11 and any batch already completed (CSV/.done present).
# Safe to re-run; it resumes where it left off.

set -euo pipefail

# === CONFIG ===
ROOT="/home/david/sim_runs/final_20250909_212121/"   # parent directory that contains Final_study_* dirs
RSCRIPT="nugget_counter_orig.R"                   # path to your R script
STEP_LOOKUP="step_lookup.csv"                     # path to step lookup
MODEL="deepseek-r1:8b"                            # model
MAX_ROWS=100000                                   # cap rows per batch
OUTDIR="nugget_outputs"                           # where to write CSVs/logs/.done

# Optional: point to a remote Ollama (uncomment and set)
# OLLAMA_URL="http://hcai.ur.de:11434/api/generate"

# Skip these batches
SKIP=("Final_study_1" "Final_study_10")

mkdir -p "$OUTDIR"

# helper: check if value is in SKIP array
in_skip() {
  local x="$1"
  for s in "${SKIP[@]}"; do
    [[ "$x" == "$s" ]] && return 0
  done
  return 1
}

# iterate batches in sorted order
mapfile -t BATCH_DIRS < <(find "$ROOT" -maxdepth 1 -type d -name 'Final_study_*' | sort)

for batch_dir in "${BATCH_DIRS[@]}"; do
  [[ -d "$batch_dir" ]] || continue
  bname="$(basename "$batch_dir")"

  if in_skip "$bname"; then
    echo "[skip] $bname (in skip list)"
    continue
  fi

  files_glob="$batch_dir"/line_*/${bname}_dialogs_flat_*.csv
  out_csv="$OUTDIR/agent_nuggets_by_step_${bname}.csv"
  log="$OUTDIR/nugget_counter_${bname}.log"
  done_marker="$OUTDIR/${bname}.done"

  # checkpoint: skip if already completed
  if [[ -f "$done_marker" && -s "$out_csv" ]]; then
    echo "[done] $bname (found $done_marker and CSV), skipping"
    continue
  fi
  # also skip if CSV exists and has >1 line (header + at least one row)
  if [[ -s "$out_csv" && $(wc -l < "$out_csv") -gt 1 ]]; then
    echo "[done] $bname (CSV already populated), skipping"
    touch "$done_marker"  # normalize checkpoint
    continue
  fi

  # sanity: ensure there are input files
  shopt -s nullglob
  matches=( $files_glob )
  shopt -u nullglob
  if (( ${#matches[@]} == 0 )); then
    echo "[warn] $bname: no files match $files_glob — skipping"
    continue
  fi

  echo "============================================================"
  echo "[run]  $bname"
  echo "       inputs: ${#matches[@]} files"
  echo "       out:    $out_csv"
  echo "       log:    $log"
  echo "------------------------------------------------------------"

  # build the command
  cmd=( Rscript "$RSCRIPT"
        files="$files_glob"
        step_lookup="$STEP_LOOKUP"
        out="$out_csv"
        model="$MODEL"
        max_rows="$MAX_ROWS" )

  # add ollama override if provided
  if [[ "${OLLAMA_URL:-}" != "" ]]; then
    cmd+=( ollama_url="$OLLAMA_URL" )
  fi

  {
    echo "[start] $(date -Is)  $bname"
    echo "[cmd] ${cmd[*]}"
    "${cmd[@]}"
    status=$?
    echo "[end] $(date -Is)  $bname (status=$status)"
    if (( status == 0 )); then
      touch "$done_marker"
      echo "[ok] wrote: $out_csv ; lines=$(wc -l < "$out_csv")"
    else
      echo "[ERR] batch $bname failed (status=$status)"
      exit $status
    fi
  } |& tee -a "$log"

done

echo "All eligible batches processed. Outputs in: $(realpath "$OUTDIR")"
