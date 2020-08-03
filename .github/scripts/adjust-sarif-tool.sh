#!/bin/bash
MODE="$1"
shopt -s globstar

for file in **/*.sarif; do
    echo "Processing $file"
    jq "setpath([\"runs\",0,\"tool\",\"driver\",\"name\"]; \"clang-${MODE}\")" $file | sponge $file
done
