#!/bin/bash
# copy_three_batches.sh
# Usage: ./copy_three_batches.sh OUT_ROOT TARGET_DIR

OUT="$1"
DEST="$2"

mkdir -p "$DEST"

# batches you want to copy
BATCHES=("Final_study_1" "Final_study_10" "Final_study_11")

for b in "${BATCHES[@]}"; do
  src="$OUT/$b"
  if [ -d "$src" ]; then
    echo "Copying files from $b ..."
    # loop over all line_* folders
    for line in "$src"/line_*; do
      [ -d "$line" ] || continue
      for f in "$line"/*; do
        base=$(basename "$f")
        # prefix filename with batch+line so they don't overwrite each other
        cp "$f" "$DEST/${b}_${base}"
      done
    done
  else
    echo "Skipping $b (not found)"
  fi
done

echo "Done. All files copied flat into $DEST"
