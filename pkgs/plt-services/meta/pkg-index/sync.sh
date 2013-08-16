#!/bin/sh

rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../meta/pkg-index/ plt-etc:local/galaxy/meta/pkg-index/

rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../../../racket/collects/pkg/ plt-etc:local/galaxy/pkg/

rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../../../pkgs/racket-pkgs/racket-test/tests/pkg plt-etc:local/galaxy/tests/pkg/

for i in official planet-compat ; do
    rsync -a --progress -h --delete plt-etc:local/galaxy/meta/pkg-index/$i/root/ $i/root/
done

rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../../../racket/collects/pkg plt-etc:local/plt/collects/pkg/

rsync -a --progress -h --delete --exclude compiled ../../../web-server-pkgs/web-server-lib/web-server ../../../../racket/collects/net plt-etc:local/plt/collects/
