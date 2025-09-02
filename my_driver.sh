#!/usr/bin/env bash
set -euo pipefail

# ---- Config you can tweak once per job ----
USER_MODEL="llama3:70b"
AGENT_MODEL="deepseek-r1:8b"
ENERGY=1                                   # 1 = ON, 0 = OFF
OUTDIR="$HOME/sim_runs/test_$(date +%Y%m%d_%H%M%S)"
LOG="sim_run_$$.log"                       # one log per job, $$ = PID
SCHEDULE="run_schedule.tsv"                # tab-separated: profile<TAB>recipe<TAB>seed

mkdir -p "$OUTDIR"

echo "========== RUN CONFIG ==========" | tee -a "$LOG"
echo "UTC time:      $(date -u +'%F %T %Z')" | tee -a "$LOG"
echo "User model:    $USER_MODEL"           | tee -a "$LOG"
echo "Agent model:   $AGENT_MODEL"          | tee -a "$LOG"
echo "Energy:        $ENERGY"               | tee -a "$LOG"
echo "Out dir:       $OUTDIR"               | tee -a "$LOG"
echo "Schedule:      $SCHEDULE"             | tee -a "$LOG"
echo "Log:           $LOG"                  | tee -a "$LOG"
echo "================================"     | tee -a "$LOG"

# Sanity checks
if [[ ! -s "$SCHEDULE" ]]; then
  echo "ERROR: schedule file '$SCHEDULE' not found or empty." | tee -a "$LOG"
  exit 1
fi

# Iterate the randomized schedule.
# We run ONE conversation per (profile, recipe, seed)—so n=1 and pass a single-profile string.
line=0
while IFS=$'\t' read -r PROF RCP SEED; do
  ((line+=1))
  TS=$(date +'%F %T')
  echo "[ $TS ] Line $line — start: profile=$PROF recipe=$RCP seed=$SEED" | tee -a "$LOG"

  Rscript simulation.R \
    n=1 \
    recipe="$RCP" \
    user="$USER_MODEL" \
    agent="$AGENT_MODEL" \
    profiles="$PROF" \
    energy="$ENERGY" \
    outdir="$OUTDIR" \
    seed="$SEED" \
    >> "$LOG" 2>&1

  TS2=$(date +'%F %T')
  echo "[ $TS2 ] Line $line — done:  profile=$PROF recipe=$RCP seed=$SEED" | tee -a "$LOG"
done < "$SCHEDULE"

echo "[ $(date +'%F %T') ] All scheduled runs completed." | tee -a "$LOG"
