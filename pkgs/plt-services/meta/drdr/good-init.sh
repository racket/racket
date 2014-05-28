#!/bin/sh

export PLTSTDERR="info"
PLTROOT="/opt/plt/plt"
LOGS="/opt/plt/logs"
R="$PLTROOT/bin/racket"
DRDR="/opt/svn/drdr"

cd "$DRDR"

kill_all() {
  cat "$LOGS/"*.pid > /tmp/leave-pids-$$
  KILL=`pgrep '^(Xorg|Xnest|Xvfb|Xvnc|fluxbox|racket|gracket(-text)?)$' | grep -w -v -f /tmp/leave-pids-$$`
  rm /tmp/leave-pids-$$
  kill -15 $KILL
  sleep 2
  kill -9 $KILL
  sleep 1
}

run_loop () { # <basename> <kill?>
  while true; do
    if [ "x$2" = "xyes" ]; then
      echo "clearing unattached shm regions"
      ipcs -ma | awk '0 == $6 {print $2}' | xargs -n 1 ipcrm -m
    fi
    echo "$1: compiling"
    "$PLTROOT/bin/raco" make "$1.rkt"
    echo "$1: running"
    "$R" -t "$1.rkt" &
    echo "$!" > "$LOGS/$1.pid"
    wait "$!"
    echo "$1: died"
    rm "$LOGS/$1.pid"
    if [ "x$2" = "xyes" ]; then
      echo "killing processes"
      kill_all
    fi
  done
}

exec

run_loop render &
run_loop main yes &
