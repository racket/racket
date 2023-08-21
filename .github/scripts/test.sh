#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

runmats() {
  echo zuo . all "$@"
  bin/zuo ${MACH}/mats all "$@" noisy=t 2>&1 | tee -a Make.out | grep '^matting '
}

# Split these out so that we get output every 10 minutes on Windows builds.
runmats o=0
runmats o=0 cp0=t
runmats o=3
runmats o=3 cp0=t
runmats o=3 cp0=t eval=interpret

if [ -f ${MACH}/mats/summary ]; then
  cat ${MACH}/mats/summary
  diff -q ${DIR}/summary ${MACH}/mats/summary
  exit $?
else
  exit 1
fi
