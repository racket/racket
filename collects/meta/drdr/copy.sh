#!/bin/sh

rsync -avz . ${1}drdr:/opt/svn/drdr/ --exclude=compiled --delete --exclude=data --exclude=builds
