#!/bin/sh

for i in pkg tests/pkg meta/pkg-index ; do
    rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../$i/ plt-etc:local/galaxy/$i/
done

for i in official planet-compat ; do
    rsync -a --progress -h --delete plt-etc:local/galaxy/meta/pkg-index/$i/root/ $i/root/
done

rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../pkg/ plt-etc:local/plt/collects/pkg/

rsync -a --progress -h --delete --exclude compiled ../../web-server ../../net plt-etc:local/plt/collects/
