#!/bin/sh

# Make sure the timestamp is updated on every gnerated boot file,
# since the timestamp could stay the same if the file didn't change,
# just so `check_boot.sh` will know that build is up-to-date.
# Then, delete `boot_pending`.

MACH=$1
SCHEME_WORKAREA=$2

ready_mach()
{
    INIT=$SCHEME_WORKAREA/boot/$MACH/$1
    if [ -e $INIT ] ; then
        touch $INIT
    fi
}

ready_mach scheme.boot
ready_mach petite.boot
ready_mach scheme.h
ready_mach equates.h
ready_mach gc-ocd.inc
ready_mach gc-oce.inc
ready_mach gc-par.inc
ready_mach heapcheck.inc

rm -f boot_pending
