#!/usr/bin/env bash
set -euo pipefail

# ---- Config you can tweak once per job ----
USER_MODEL="llama3:70b"
AGENT_MODEL="deepseek-r1:8b"
ENERGY=1                                   # 1 = ON, 0 = OFF
OUTDIR="$HOME/sim_runs/test_$(date +%Y%m%d_%H%M%S)"
SCHEDULE="run_schedule.tsv"                # TSV: profile<TAB>recipe<TAB>seed
LOG="sim_run_$$.log"                       # one log per job, $$ = PID
CKPT="$OUTDIR/.done.lines"                 # checkpoint of finished line numbers

mkdir -p "$OUTDIR"

# Single-instance guard (avoid accidental double starts)
exec 9>/tmp/sim_driver.lock
flock -n 9 || { echo "Another driver seems to be running (lock held)."; exit 1; }

echo "========== RUN CONFIG ==========" | tee -a "$LOG"
echo "UTC time:      $(date -u +'%F %T %Z')" | tee -a "$LOG"
echo "User model:    $USER_MODEL"           | tee -a "$LOG"
echo "Agent model:   $AGENT_MODEL"          | tee -a "$LOG"
echo "Energy:        $ENERGY"               | tee -a "$LOG"
echo "Out dir:       $OUTDIR"               | tee -a "$LOG"
echo "Schedule:      $SCHEDULE"             | tee -a "$LOG"
echo "Log:           $LOG"                  | tee -a "$LOG"
echo "Checkpoint:    $CKPT"                 | tee -a "$LOG"
echo "================================"     | tee -a "$LOG"

# Sanity checks
if [[ ! -s "$SCHEDULE" ]]; then
  echo "ERROR: schedule file '$SCHEDULE' not found or empty." | tee -a "$LOG"
  exit 1
fi

touch "$CKPT"

line=0
start_all=$(date +%s)

# Read schedule; skip blank lines and lines that start with '#'
# Also trim CR in case file is edited on Windows.
while IFS=$'\t' read -r PROF RCP SEED REST || [[ -n "${PROF-}" ]]; do
  ((line+=1))
  # sanitize / skip
  [[ -z "${PROF-}" ]] && continue
  [[ "${PROF:0:1}" == "#" ]] && continue
  # resume support: skip lines already completed
  if grep -qx "$line" "$CKPT"; then
    echo "[ $(date +'%F %T') ] Line $line — skipped (already completed)" | tee -a "$LOG"
    continue
  fi

  ts_start=$(date +%s)
  TS=$(date +'%F %T')
  echo "[ $TS ] Line $line — start: profile=$PROF recipe=$RCP seed=$SEED" | tee -a "$LOG"

  # Run exactly one conversation for this line
  # Use stdbuf so logs stream immediately in tail/screen
  stdbuf -oL -eL Rscript simulation.R \
    n=1 \
    recipe="$RCP" \
    user="$USER_MODEL" \
    agent="$AGENT_MODEL" \
    profiles="$PROF" \
    energy="$ENERGY" \
    outdir="$OUTDIR" \
    seed="$SEED" \
    >> "$LOG" 2>&1

  ts_end=$(date +%s)
  dur=$((ts_end - ts_start))
  echo "$line" >> "$CKPT"

  TS2=$(date +'%F %T')
  echo "[ $TS2 ] Line $line — done:  profile=$PROF recipe=$RCP seed=$SEED (${dur}s)" | tee -a "$LOG"
done < <(tr -d '\r' < "$SCHEDULE")

total=$(( $(date +%s) - start_all ))
echo "[ $(date +'%F %T') ] All scheduled runs completed in ${total}s." | tee -a "$LOG"
