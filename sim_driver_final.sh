#!/usr/bin/env bash
# Resilient sequential driver for Final_study_*.tsv
# - Continues on errors (does NOT 'set -e')
# - Logs per-line success/failure
# - Optional timeout and retries
# - Resume-safe via .done.lines; failures tracked in .failed.lines

set -uo pipefail

# ---- Config (override via env) ----
USER_MODEL="${USER_MODEL:-llama3:70b}"
ENERGY="${ENERGY:-1}"
SCHEDULE_GLOB="${SCHEDULE_GLOB:-Final_study_*.tsv}"
OUT_ROOT="${OUT_ROOT:-$HOME/sim_runs/final_$(date +%Y%m%d_%H%M%S)}"

# Capacity controls
OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"
MKL_NUM_THREADS="${MKL_NUM_THREADS:-1}"
OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-1}"
R_NUM_THREADS="${R_NUM_THREADS:-1}"

# Reliability controls
LINE_TIMEOUT_SEC="${LINE_TIMEOUT_SEC:-0}"   # 0 = no timeout; else kill a line after N seconds
RETRIES="${RETRIES:-0}"                     # retry this many times on failure
RETRY_SLEEP_SEC="${RETRY_SLEEP_SEC:-5}"     # pause between retries

# Pacing
LINE_PAUSE_SEC="${LINE_PAUSE_SEC:-0}"
FILE_PAUSE_SEC="${FILE_PAUSE_SEC:-0}"

export OMP_NUM_THREADS MKL_NUM_THREADS OPENBLAS_NUM_THREADS R_NUM_THREADS

mkdir -p "$OUT_ROOT"

MASTER_LOG="$OUT_ROOT/master_$(date +%s).log"
echo "========== FINAL SCHEDULE DRIVER (resilient) ==========" | tee -a "$MASTER_LOG"
echo "UTC time:        $(date -u +'%F %T %Z')" | tee -a "$MASTER_LOG"
echo "Schedule glob:   $SCHEDULE_GLOB"         | tee -a "$MASTER_LOG"
echo "User model:      $USER_MODEL"            | tee -a "$MASTER_LOG"
echo "Energy:          $ENERGY"                | tee -a "$MASTER_LOG"
echo "Out root:        $OUT_ROOT"              | tee -a "$MASTER_LOG"
echo "Threads cap:     OMP=$OMP_NUM_THREADS MKL=$MKL_NUM_THREADS OPENBLAS=$OPENBLAS_NUM_THREADS R=$R_NUM_THREADS" | tee -a "$MASTER_LOG"
echo "Pacing:          line=${LINE_PAUSE_SEC}s file=${FILE_PAUSE_SEC}s" | tee -a "$MASTER_LOG"
echo "Timeout:         per-line=${LINE_TIMEOUT_SEC}s Retries=${RETRIES}" | tee -a "$MASTER_LOG"
echo "=======================================================" | tee -a "$MASTER_LOG"

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
  ckpt_done="$file_out/.done.lines"
  ckpt_fail="$file_out/.failed.lines"
  lockfile="/tmp/${base}.lock"

  mkdir -p "$file_out"
  touch "$ckpt_done" "$ckpt_fail"

  # Single-instance guard per file
  exec 9>"$lockfile"
  if ! flock -n 9; then
    echo "[ $(date +'%F %T') ] SKIP $base — another process holds $lockfile" | tee -a "$MASTER_LOG"
    continue
  fi

  echo "---------- Processing $schedule ----------" | tee -a "$file_log" "$MASTER_LOG"

  line=0
  start_file=$(date +%s)
  while IFS=$'\t' read -r PROF RCP MODEL SEED REST || [[ -n "${PROF-}" ]]; do
    ((line+=1))
    [[ -z "${PROF-}" ]] && continue
    [[ "${PROF:0:1}" == "#" ]] && continue
    if grep -qx "$line" "$ckpt_done"; then
      continue
    fi

    line_dir="$(printf "%s/line_%04d" "$file_out" "$line")"
    mkdir -p "$line_dir"

    ts=$(date +%s)
    echo "[ $(date +'%F %T') ] $base line $line — start: profile=$PROF recipe=$RCP model=$MODEL seed=$SEED" | tee -a "$file_log"

    run_cmd=( Rscript simulation.R
      n=1 recipe="$RCP" user="$USER_MODEL" agent="$MODEL"
      profiles="$PROF" energy="$ENERGY" outdir="$line_dir" seed="$SEED"
    )

    if (( LINE_TIMEOUT_SEC > 0 )); then
      wrapper=( timeout --preserve-status "${LINE_TIMEOUT_SEC}s" stdbuf -oL -eL )
    else
      wrapper=( stdbuf -oL -eL )
    fi

    attempt=0
    success=0
    while (( attempt <= RETRIES )); do
      ((attempt++))
      "${wrapper[@]}" "${run_cmd[@]}" >> "$file_log" 2>&1
      status=$?
      if (( status == 0 )); then
        success=1
        break
      else
        echo "[ $(date +'%F %T') ] $base line $line — ERROR (exit $status)" | tee -a "$file_log"
        if (( attempt <= RETRIES )); then
          sleep "$RETRY_SLEEP_SEC"
        fi
      fi
    done

    dur=$(( $(date +%s) - ts ))
    if (( success == 1 )); then
      echo "$line" >> "$ckpt_done"
      echo "[ $(date +'%F %T') ] $base line $line — DONE in ${dur}s" | tee -a "$file_log"
    else
      echo "$line" >> "$ckpt_fail"
      echo "[ $(date +'%F %T') ] $base line $line — FAILED after ${dur}s (see log). Continuing." | tee -a "$file_log"
    fi

    (( LINE_PAUSE_SEC > 0 )) && sleep "$LINE_PAUSE_SEC"
  done < <(tr -d '\r' < "$schedule")

  total=$(( $(date +%s) - start_file ))
  echo "Completed $base in ${total}s" | tee -a "$file_log" "$MASTER_LOG"

  (( FILE_PAUSE_SEC > 0 )) && sleep "$FILE_PAUSE_SEC"
  rm -f "$lockfile" || true
done

echo "[ $(date +'%F %T') ] All schedule files processed." | tee -a "$MASTER_LOG"
