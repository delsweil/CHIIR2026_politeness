#!/usr/bin/env bash
set -euo pipefail

# --- CONFIG ---
ROOT_RUN="/home/david/sim_runs/final_20250909_212121"   # parent folder that contains Final_study_1..20
R_SCRIPT="/home/david/CHIIR2026_politeness/nugget_counter_orig.R"
STEP_LOOKUP="/home/david/CHIIR2026_politeness/step_lookup.csv"
MODEL="deepseek-r1:8b"
MAX_ROWS=100000
OUT_ROOT="/home/david/CHIIR2026_politeness/nugget_outputs"
LOG="/home/david/CHIIR2026_politeness/run_all_batches_master.log"

# Batches to include (auto-detect all Final_study_* if empty)
INCLUDE=()   # e.g. ("Final_study_2" "Final_study_3")  leave empty to auto-detect
# Batches to exclude (if you’ve already run 1,10,11)
EXCLUDE=("Final_study_2" "Final_study_3" "Final_study_4" "Final_study_5" "Final_study_6" "Final_study_7" "Final_study_8" "Final_study_9" "Final_study_12" "Final_study_13" "Final_study_14" "Final_study_15" "Final_study_16" "Final_study_17" "Final_study_18" "Final_study_19" "Final_study_20")

mkdir -p "$OUT_ROOT"
: > "$LOG"

echo "### Run started: $(date)" | tee -a "$LOG"
echo "ROOT_RUN=$ROOT_RUN" | tee -a "$LOG"

# --- discover batches if INCLUDE not set ---
if [ ${#INCLUDE[@]} -eq 0 ]; then
  mapfile -t INCLUDE < <(find "$ROOT_RUN" -maxdepth 1 -type d -name 'Final_study_*' -printf '%f\n' | sort -V)
fi

# --- build an exclusion set ---
should_exclude() {
  local b="$1"
  for x in "${EXCLUDE[@]}"; do
    [[ "$b" == "$x" ]] && return 0
  done
  return 1
}

# --- iterate batches ---
for B in "${INCLUDE[@]}"; do
  if should_exclude "$B"; then
    echo "[skip] $B (in EXCLUDE)" | tee -a "$LOG"
    continue
  fi

  BATCH_DIR="$ROOT_RUN/$B"
  if [[ ! -d "$BATCH_DIR" ]]; then
    echo "[warn] $B: missing dir $BATCH_DIR — skipping" | tee -a "$LOG"
    continue
  fi

  # pattern must match your real files:
  #   Final_study_X/line_YYYY/dialogs_flat_*.csv
  PATTERN="line_*/dialogs_flat_*.csv"

  # quick check: how many files match?
  mapfile -t FILES < <(find "$BATCH_DIR" -type f -path "$BATCH_DIR/line_*/dialogs_flat_*.csv" 2>/dev/null | sort)
  N=${#FILES[@]}
  if [[ $N -eq 0 ]]; then
    echo "[warn] $B: no files match $BATCH_DIR/$PATTERN — skipping" | tee -a "$LOG"
    continue
  fi
  echo "[info] $B: found $N dialogs_flat files" | tee -a "$LOG"

  OUT_CSV="$OUT_ROOT/${B}_agent_nuggets_by_step.csv"
  LOG_B="$OUT_ROOT/${B}.log"

  echo "[run ] $B -> $OUT_CSV" | tee -a "$LOG"
  nohup Rscript "$R_SCRIPT" \
    dir="$BATCH_DIR" \
    pattern="$PATTERN" \
    step_lookup="$STEP_LOOKUP" \
    out="$OUT_CSV" \
    model="$MODEL" \
    max_rows="$MAX_ROWS" \
    > "$LOG_B" 2>&1

  echo "[done] started $B (nohup). Tail: tail -f $LOG_B" | tee -a "$LOG"
done

echo "### All eligible batches queued at: $(date)" | tee -a "$LOG"
echo "Outputs in: $OUT_ROOT" | tee -a "$LOG"
