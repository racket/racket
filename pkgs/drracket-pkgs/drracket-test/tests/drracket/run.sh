#!/bin/sh

## use 
##   env RACKET=something ./run.sh
## to use a racket not in the path

files=`ls *.rkt | grep -v info.rkt | grep -v follow-log.rkt`

if [ "$RACKET" == "" ]; then
  RACKET=racket;
fi

for file in $files; do
  echo STARTING $file
  "$RACKET" $file 
done
