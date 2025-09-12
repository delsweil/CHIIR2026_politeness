#!/bin/bash
# copy_three_batches.sh
# Usage: ./copy_three_batches.sh OUT_ROOT TARGET_DIR

OUT="$1"
DEST="$2"

mkdir -p "$DEST"

# which batches to copy
BATCHES=("Final_study_1" "Final_study_10" "Final_study_11")

for b in "${BATCHES[@]}"; do
  src="$OUT/$b"
  if [ -d "$src" ]; then
    echo "Copying $b ..."
    cp -r "$src"/line_* "$DEST/"
  else
    echo "Skipping $b (not found)"
  fi
done

echo "Done. Conversations copied to $DEST"
