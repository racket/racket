#!/bin/sh

PATH=~/local/new-plt/racket/bin:$PATH

cd ~/local/new-plt/pkgs/plt-services/meta/pkg-index/planet-compat
racket update.rkt
racket static.rkt
s3cmd -c ~/.s3cfg-plt sync --recursive --delete-removed root/cache/ s3://planet-compats.racket-lang.org/
