#!/bin/bash
set -e

. ~/.profile

export PC_ROOT=~/Dev/local/pkg.rlo/planet-compat/root/

cd $(dirname $0)
raco make update.rkt static.rkt
racket update.rkt
racket static.rkt
s3cmd -c ~/.s3cfg-plt sync --recursive --delete-removed ${PC_ROOT}/cache/ s3://planet-compats.racket-lang.org/
