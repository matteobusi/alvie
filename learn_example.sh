#!/usr/bin/env bash

# Learns the models of the running example from the paper
#
# Usage:
#   ./learn_example.sh
#
EPS=0.01
DELTA=0.01

# Useful paths
SCRIPT_DIR=$(dirname "$(readlink -f "$0")")

LOGS_DIR=$SCRIPT_DIR/logs/example
RES_DIR=$SCRIPT_DIR/results/example
TMP_DIR=$SCRIPT_DIR/tmp/example

SCG_DIR=$SCRIPT_DIR/sancus-core-gap
SPEC_DIR=$SCRIPT_DIR/spec-lib/example
MM_DIR=$SCRIPT_DIR/alvie/code


mkdir -p $RES_DIR
mkdir -p $LOGS_DIR
# mkdir -p $TMP_DIR

declare -a EXPERIMENTS=(
  "attacker enclave 0 bf89c0b"
  "attacker enclave 1 bf89c0b"
)

# Move to the project's directory
cd $MM_DIR

# Compile the project
dune build

run() {
  local status=0

  echo $2

  (eval $2 & wait $!;
  status=$?;
  if [[ $status -ne 0 ]]; then
    echo -e "$1 ... [KO - $3]"
  else
    echo "$1 ... [OK - $3]"
  fi) &
}

# Then, iterate over all the possible combinations and learn
echo -e "\nLearning started: refer to files in $LOGS_DIR for details"

for experiment in "${EXPERIMENTS[@]}"
do
    read -a exp_arr <<< "$experiment"
    attack_name=${exp_arr[0]%%[[:space:]]}
    enclave_name=${exp_arr[1]%%[[:space:]]}
    secret=${exp_arr[2]%%[[:space:]]}
    commit=${exp_arr[3]%%[[:space:]]}

    name="$commit-$attack_name-$enclave_name-$secret-$EPS-$DELTA"
    logfile="$LOGS_DIR/learn-$name.log"
    resfile="$RES_DIR/$name.dot"

    # Run the learning only if it has not been done already!
    if [ -f "$resfile" ]; then
      echo "$name/$name ... [OK - Done before]"
    else
      # Invoke the learning process in background and send the stderr/stdout to the log file
      run "$name" "_build/default/bin/learn.exe --att-spec \"$SPEC_DIR/$attack_name.atdl\" --encl-spec \"$SPEC_DIR/$enclave_name.etdl\" --res \"$resfile\" --tmpdir \"$TMP_DIR\" --commit $commit --sancus \"$SCG_DIR\" --secret $secret --epsilon $EPS --delta $DELTA --oracle pac > $logfile 2>&1" "$logfile"
    fi
done

# wait_and_report
wait

echo ""
