#!/usr/bin/env bash

# This script is the entry point for the automatic discovery of
# gaps between various versions of the processor.
# It takes all the models pairs from RES_DIR and follwing the naming convention in learn_all produces
# witness graphs comparing them in CEX_DIR.
# LIMIT variable is used to limit the number of
# counterexamples to be considered for each pair of model
#
# If fast is given it uses the fast models.
# Usage:
#   ./compare_all.sh [fast]

# Useful paths
SCRIPT_DIR=$(dirname "$(readlink -f "$0")")

LOGS_DIR=$SCRIPT_DIR/logs/$1
RES_DIR=$SCRIPT_DIR/results/$1
CEX_DIR=$SCRIPT_DIR/counterexamples/$1
TMP_DIR=$SCRIPT_DIR/tmp

MM_DIR=$SCRIPT_DIR/alvie/code

# LIMIT = 10
# LIMIT=100
# NO LIMIT!
LIMIT=-1

cd $SCRIPT_DIR

mkdir -p $LOGS_DIR

# Loads the list of all available models
readarray ZERO_MODELS <<< "$(ls $RES_DIR/*-0-*-int.dot)"

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
    echo "$1 ... [OK]"
  fi) &
}

echo -e "\nComparison started: refer to files in $LOGS_DIR for details"

if ((LIMIT >= 0)); then
  echo "Max number of counterexamples for each pair is $LIMIT"
else
  echo "No limit on the max number of counterexamples for each pair"
fi

for m1 in "${ZERO_MODELS[@]}"
do
    m1_name="$(basename $m1 .dot)"
    commit=${m1_name:0:7}
    att=${m1_name:8:2}
    att=${att//-/}
    readarray ONE_MODELS <<< "$(ls $RES_DIR/$commit-${att}-*-1-*-int.dot)"


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
        cexfile="$CEX_DIR/$att/$commit-$att"
        logfile="$LOGS_DIR/compare-$commit-$att.log"
        name="$commit-$att"

        # Call the comparison process
        # run "$name" "_build/default/bin/compare.exe --tmpdir \"$TMP_DIR\" --m1 \"${m1%%[[:space:]]}\" --m2 \"${m2%%[[:space:]]}\" --cex-file \"$cexfile\" $cexlimit > \"$logfile\" 2>&1" "$logfile"
        run "$name" "_build/default/bin/fa.exe --tmpdir \"$TMP_DIR\" --m1-int \"${m1%%[[:space:]]}\" --m2-int \"${m2%%[[:space:]]}\"  --m1-nint \"${m1_nint%%[[:space:]]}\" --m2-nint \"${m2_nint%%[[:space:]]}\" --witness-file-basename \"$cexfile\" --debug $cexlimit > \"$logfile\" 2>&1" "$logfile"
    done
done

wait

echo ""
