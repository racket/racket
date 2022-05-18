#!/bin/bash
MODE="$1"
shopt -s globstar

for file in **/*.sarif; do
    echo "Processing $file"
    sed -i -e 's|"uri": "file:///__w/racket/racket/|"uri": "|g' $file
    jq "setpath([\"runs\",0,\"tool\",\"driver\",\"name\"]; \"clang-${MODE}\")" $file | sponge $file
done
