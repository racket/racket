#!/bin/bash

cd /opt/plt/builds
for i in * ; do
    if [ -f ${i}/archive.db ] ; then
        /opt/plt/plt/bin/racket -t /opt/svn/drdr/archive-repair.rkt -- $i
    fi
done
