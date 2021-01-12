#!/bin/sh

# Force all workarea boot files back to links to immediate "boot"

MACH=$1
SCHEME_WORKAREA=$2

ready_mach()
{
    INIT=$SCHEME_WORKAREA/boot/$MACH/$1
    RELINIT=../../../boot/$MACH/$1
    DEST=$SCHEME_WORKAREA/$MACH/boot/$MACH/$1
    if [ ! -f "$DEST" ]; then
        mkdir -p `dirname $DEST`
        ln -s "$RELINIT" "$DEST"
    elif [ "`/usr/bin/find "$INIT" -newer "$DEST"`" != "" ] ; then
        rm "$DEST"
        ln -s "$RELINIT" "$DEST"
    fi
}

ready_mach scheme.boot
ready_mach petite.boot
ready_mach scheme.h
ready_mach equates.h
ready_mach gc-ocd.inc
ready_mach gc-oce.inc
ready_mach gc-par.inc
