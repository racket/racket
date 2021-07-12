#!/bin/bash

# Catch INT to kill subprocesses
trap end_make INT

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

if [ -z ${MACH} ]; then
    echo "Please define MACH" 1>&2
    exit 1
fi

ERR=0
MATS=${MACH}/mats
SUMMARYFILE=${MACH}/summary
declare -a PIDS

# Clean and create summary file
rm -f "${SUMMARYFILE}"
touch "${SUMMARYFILE}"

end_make() {
    echo "Caught Ctrl-C" 1>&2
    for i in "${!PIDS[@]}"; do
        local pid="${PID[$i]}"
        echo "Killing pid ${pid}" 1>&2
        kill ${pid}
        rm -r "${MATS}"."${i}"
    done
    exit 1
}

runmats() {
    local id=$1
    local matsdest="${MATS}.${id}"
    cp -r "${MATS}" "${matsdest}"
    echo make allxhelp "${@:2}"

    # Use -l here so we don't overload machines with less than the number
    # of parallel tests we force to run
    make -l$(nproc) -C "${matsdest}" allxhelp "${@:2}" 2>&1 | tee -a Make.out | grep '^matting ' &
    PIDS[$id]="$!"
}

ARGS=("o=0"
      "o=0 cp0=t"
      "o=3"
      "o=3 cp0=t"
      "o=3 cp0=t eval=interpret")

# Run all test classes in parallel
for i in ${!ARGS[@]}; do
    runmats $i ${ARGS[$i]}
done

# Wait for all jobs
for i in "${!PIDS[@]}"; do
    pid="${PIDS[$i]}"
    wait "${pid}"

    # Append current contents to final summary file
    if [ -f "${MATS}.${i}/summary" ]; then
        cat "${MATS}.${i}/summary" >> "${SUMMARYFILE}"
    else
        echo "ERROR: No summary file for mats.${i}" 1>&2
        end_make
        ERR=1
    fi
done

# All done - sort lines of file for comparison
sort "${SUMMARYFILE}" > "${SUMMARYFILE}.sorted"

if [[ "${ERR}" = "1" ]]; then
    exit 1
else
    cat "${SUMMARYFILE}"
    diff -q ${DIR}/summary "${SUMMARYFILE}.sorted"    
    exit $?
fi
