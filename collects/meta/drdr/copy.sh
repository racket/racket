#!/bin/bash
rsync -avz . plt-drdr:/opt/svn/drdr/ --exclude=.svn
