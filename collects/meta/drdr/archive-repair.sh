#!/bin/bash

cd /opt/plt/builds

du -h */archive.db | awk '{print $1}' | sort -n | uniq -c
echo

for rev in $(du -h */archive.db | sort -n | tail | awk '{print $2}' | awk -F/ '{print $1}' | tac) ; do
    du -h ${rev}/archive.db
    /opt/plt/plt/bin/racket -t /opt/svn/drdr/archive-repair.rkt -- $rev > /dev/null
done
echo

du -h */archive.db | awk '{print $1}' | sort -n | uniq -c
echo

df -h
