#!/bin/sh

for i in planet2 tests/planet2 meta/planet2-index ; do
    rsync -n -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../$i/ plt-etc:local/galaxy/$i/
done

exit 1

for i in official planet-compat ; do
    rsync -a --progress -h --delete plt-etc:local/galaxy/meta/planet2-index/$i/root/ $i/root/
done

