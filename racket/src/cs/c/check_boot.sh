#!/bin/sh

# Create the file `boot_pending` if file timestamps suggest that
# we need to recreate boot files starting with the current `pb`.

MACH=$1
SCHEME_DIR=$2
SCHEME_WORKAREA=$3

check_pb()
{
    SRC="$SCHEME_DIR"/boot/pb/$1
    DEST=$SCHEME_WORKAREA/pb/boot/pb/$1
    if [ ! -e $DEST ] ; then
        touch boot_pending
    elif [ "`/usr/bin/find "$SRC" -newer "$DEST"`" != "" ] ; then
        touch boot_pending
    fi
}

check_pb scheme.boot
check_pb petite.boot
check_pb scheme.h
check_pb equates.h
check_pb gc-ocd.inc
check_pb gc-oce.inc
check_pb gc-par.inc
check_pb heapcheck.inc

check_mach()
{
    SRC="$SCHEME_DIR"/boot/pb/$1
    INIT=$SCHEME_WORKAREA/boot/$MACH/$1
    if [ ! -e $INIT ] ; then
        touch boot_pending
    elif [ "`/usr/bin/find "$SRC" -newer "$INIT"`" != "" ] ; then
        touch boot_pending
    fi
}

check_mach scheme.boot
check_mach petite.boot
check_mach scheme.h
check_mach equates.h
check_mach gc-ocd.inc
check_mach gc-oce.inc
check_mach gc-par.inc
check_mach heapcheck.inc
