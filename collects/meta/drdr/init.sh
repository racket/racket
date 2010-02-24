#!/bin/bash
export PLTSTDERR=info
PLTROOT=/opt/plt/plt
LOGS=/opt/plt/logs
MZ=${PLTROOT}/bin/mzscheme
DRDR=/opt/svn/drdr

cd ${DRDR}

while true ; do
    ${PLTROOT}/bin/mzc -k render.ss
    nohup ${MZ} -t render.ss 2>&1 >> ${LOGS}/render.log
done &

while true ; do
 ${PLTROOT}/bin/mzc -k main.ss
 nohup ${MZ} -t main.ss 2>&1 >> ${LOGS}/drdr.log
 killall -9 Xvfb mzscheme mred-text
done &
