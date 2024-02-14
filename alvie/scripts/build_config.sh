#!/usr/bin/env bash

SCG_DIR="$1"
SECURITY="$2"
MASTER_KEY="$3"

cd $SCG_DIR

rm -rf build/
mkdir -p build
cd build
cmake -DRESET_ON_VIOLATION=1 -DNEMESIS_RESISTANT=1 -DSECURITY=$SECURITY -DMASTER_KEY=$MASTER_KEY ..
