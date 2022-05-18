#!/bin/bash
MODE="$1"
shopt -s globstar

for file in **/*.sarif; do
    echo "Processing $file"
    cat $file | \
      jq "setpath([\"runs\",0,\"tool\",\"driver\",\"name\"]; \"clang-${MODE}\")" | \
      jq '(.. | .uri? | select(. != null)) |= sub( "^file:///__w/racket/racket/"; "" )' | \
      sponge $file
done
