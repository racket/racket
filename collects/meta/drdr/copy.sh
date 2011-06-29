#!/bin/bash
rsync -avz . drdr:/opt/svn/drdr/ --exclude=compiled --delete --exclude=data
