#!/usr/bin/env bash

# This script is the entry point for the automatic discovery of
# gaps between various versions of the processor.
# It takes all the models pairs from RES_DIR and produces
# graphs comparing them in CEX_DIR. LIMIT variable is used to limit the number of
# counterexamples to be considered for each pair of model
#
# If fast is given it uses the fast models.
# Usage:
#   ./compare_one.sh <spec_basename>

# Useful paths
SCRIPT_DIR=$(dirname "$(readlink -f "$0")")

LOGS_DIR=$SCRIPT_DIR/logs/example
RES_DIR=$SCRIPT_DIR/results/example
CEX_DIR=$SCRIPT_DIR/counterexamples/example
TMP_DIR=$SCRIPT_DIR/tmp

MM_DIR=$SCRIPT_DIR/alvie/code

cd $SCRIPT_DIR

mkdir -p $LOGS_DIR

# Loads the list of all available models
readarray ZERO_MODELS <<< "$(ls $RES_DIR/*-attacker-*-0-*.dot)"

# Move to the project's directory
cd $MM_DIR

# Compile the project
dune build

run() {
  local status=0

  (eval $2 & wait $!;
  status=$?;
  if [[ $status -ne 0 ]]; then
    echo -e "$1 ... [KO - $3]"
  else
    echo "$1 ... [OK - $3]"
  fi) &
}

echo -e "\nComparison started: refer to files in $LOGS_DIR for details"

for m1 in "${ZERO_MODELS[@]}"
do
    m1_name="$(basename $m1 .dot)"
    commit=${m1_name:0:7}
    readarray ONE_MODELS <<< "$(ls $RES_DIR/$commit-attacker-*-1-*.dot)"

    for m2 in "${ONE_MODELS[@]}"
    do
        m1_nint=${m1//int/nint}
        m2_nint=${m2//int/nint}
        m2_name="$(basename $m2 .dot)"

        # No need of comparing a model with itself
        if [ "$m1_name" = "$m2_name" ]; then
          continue
        fi

        cexlimit=""
        cexfile="$CEX_DIR/$commit-attacker"
        logfile="$LOGS_DIR/compare-$commit-attacker.log"
        name="$commit-attacker"

        # Call the comparison process
         run "$name" "_build/default/bin/fa.exe --tmpdir \"$TMP_DIR\" --m1-int \"${m1%%[[:space:]]}\" --m2-int \"${m2%%[[:space:]]}\" --witness-file-basename \"$cexfile\" $cexlimit > \"$logfile\" 2>&1" "$logfile"
    done
done

wait

echo ""
