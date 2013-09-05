#!/bin/sh

# planet-compat
rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../meta/pkg-index/ plt-etc:local/new-plt/pkgs/plt-services/meta/pkg-index/
rsync -a --progress -h --delete plt-etc:local/galaxy-roots/planet-compat/ planet-compat/root/
rsync -a --progress -h --delete plt-etc:local/galaxy-roots/official/ official/root/

# official
rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../meta/pkg-index/ plt-etc:local/galaxy/meta/pkg-index/

rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../../../racket/collects/pkg/ plt-etc:local/galaxy/pkg/

rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../../../pkgs/racket-pkgs/racket-test/tests/pkg plt-etc:local/galaxy/tests/pkg/

rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../../../racket/collects/pkg plt-etc:local/plt/collects/pkg/

rsync -a --progress -h --delete --exclude compiled ../../../web-server-pkgs/web-server-lib/web-server ../../../../racket/collects/net plt-etc:local/plt/collects/
