#!/usr/bin/env bash
set -euo pipefail

# sim_driver_final.sh — sequential driver for Final_study_1.tsv ... Final_study_20.tsv
# Runs simulation.R for each line (no parallelism). Adds thread caps & optional pacing.

# ---- Config (override via env) ----
USER_MODEL="${USER_MODEL:-llama3:70b}"            # user model passed to simulation.R
ENERGY="${ENERGY:-1}"                             # 1 = ON, 0 = OFF
SCHEDULE_GLOB="${SCHEDULE_GLOB:-Final_study_*.tsv}"
OUT_ROOT="${OUT_ROOT:-$HOME/sim_runs/final_$(date +%Y%m%d_%H%M%S)}"

# Performance/capacity controls
OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"           # cap OpenMP
MKL_NUM_THREADS="${MKL_NUM_THREADS:-1}"           # cap MKL
OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-1}" # cap OpenBLAS
R_NUM_THREADS="${R_NUM_THREADS:-1}"               # some builds respect this
LINE_PAUSE_SEC="${LINE_PAUSE_SEC:-0}"             # sleep between lines
FILE_PAUSE_SEC="${FILE_PAUSE_SEC:-0}"             # sleep between files

export OMP_NUM_THREADS MKL_NUM_THREADS OPENBLAS_NUM_THREADS R_NUM_THREADS

# -----------------------------------
mkdir -p "$OUT_ROOT"

MASTER_LOG="$OUT_ROOT/master_$(date +%s).log"
echo "========== FINAL SCHEDULE DRIVER ==========" | tee -a "$MASTER_LOG"
echo "UTC time:        $(date -u +'%F %T %Z')" | tee -a "$MASTER_LOG"
echo "Schedule glob:   $SCHEDULE_GLOB"         | tee -a "$MASTER_LOG"
echo "User model:      $USER_MODEL"            | tee -a "$MASTER_LOG"
echo "Energy:          $ENERGY"                | tee -a "$MASTER_LOG"
echo "Out root:        $OUT_ROOT"              | tee -a "$MASTER_LOG"
echo "Threads cap:     OMP=$OMP_NUM_THREADS MKL=$MKL_NUM_THREADS OPENBLAS=$OPENBLAS_NUM_THREADS R=$R_NUM_THREADS" | tee -a "$MASTER_LOG"
echo "Pacing:          line=${LINE_PAUSE_SEC}s file=${FILE_PAUSE_SEC}s" | tee -a "$MASTER_LOG"
echo "==========================================" | tee -a "$MASTER_LOG"

shopt -s nullglob
files=( $SCHEDULE_GLOB )
if (( ${#files[@]} == 0 )); then
  echo "ERROR: No schedule files match '$SCHEDULE_GLOB' in $(pwd)" | tee -a "$MASTER_LOG"
  exit 1
fi

for schedule in "${files[@]}"; do
  base="$(basename "$schedule" .tsv)"
  file_out="$OUT_ROOT/$base"
  file_log="$file_out/${base}.log"
  ckpt="$file_out/.done.lines"
  lockfile="/tmp/${base}.lock"

  mkdir -p "$file_out"
  touch "$ckpt"

  # single-instance guard per file
  exec 9>"$lockfile"
  if ! flock -n 9; then
    echo "[ $(date +'%F %T') ] SKIP $base — another process holds $lockfile" | tee -a "$MASTER_LOG"
    continue
  fi

  echo "---------- Processing $schedule ----------" | tee -a "$file_log" "$MASTER_LOG"
  echo "Start: $(date +'%F %T')" | tee -a "$file_log"
  echo "Output dir: $file_out" | tee -a "$file_log"
  echo "Checkpoint: $ckpt" | tee -a "$file_log"
  echo "------------------------------------------" | tee -a "$file_log"

  line=0
  start_file=$(date +%s)
  while IFS=$'\t' read -r PROF RCP MODEL SEED REST || [[ -n "${PROF-}" ]]; do
    ((line+=1))
    [[ -z "${PROF-}" ]] && continue
    [[ "${PROF:0:1}" == "#" ]] && continue
    if grep -qx "$line" "$ckpt"; then
      echo "[ $(date +'%F %T') ] $base line $line — skipped (done)" | tee -a "$file_log"
      continue
    fi

    line_dir="$(printf "%s/line_%04d" "$file_out" "$line")"
    mkdir -p "$line_dir"

    ts=$(date +%s)
    echo "[ $(date +'%F %T') ] $base line $line — start: profile=$PROF recipe=$RCP model=$MODEL seed=$SEED" | tee -a "$file_log"

    # Single-threaded, sequential invocation
    stdbuf -oL -eL Rscript simulation.R       n=1       recipe="$RCP"       user="$USER_MODEL"       agent="$MODEL"       profiles="$PROF"       energy="$ENERGY"       outdir="$line_dir"       seed="$SEED"       >> "$file_log" 2>&1

    dur=$(( $(date +%s) - ts ))
    echo "$line" >> "$ckpt"
    echo "[ $(date +'%F %T') ] $base line $line — done in ${dur}s" | tee -a "$file_log"

    # Optional pacing between lines
    if (( LINE_PAUSE_SEC > 0 )); then sleep "$LINE_PAUSE_SEC"; fi
  done < <(tr -d '\r' < "$schedule")

  total=$(( $(date +%s) - start_file ))
  echo "Completed $base in ${total}s" | tee -a "$file_log" "$MASTER_LOG"

  # Optional pacing between files
  if (( FILE_PAUSE_SEC > 0 )); then sleep "$FILE_PAUSE_SEC"; fi

  rm -f "$lockfile" || true
done

echo "[ $(date +'%F %T') ] All schedule files processed." | tee -a "$MASTER_LOG"
