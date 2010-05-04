#!/bin/bash
export PLTSTDERR="info"
PLTROOT="/opt/plt/plt"
LOGS="/opt/plt/logs"
MZ="$PLTROOT/bin/mzscheme"
DRDR="/opt/svn/drdr"

cd "$DRDR"

kill_all() {
  cat "$LOGS/"*.pid > /tmp/leave-pids-$$
  KILL=`pgrep '^(Xvfb|fluxbox|racket|gracket(-text)?)$' | grep -w -v -f /tmp/leave-pids-$$`
  rm /tmp/leave-pids-$$
  kill -15 $KILL
  sleep 2
  kill -9 $KILL
}

run_loop () { # <basename> <kill?>
  while true; do
    echo "$1: compiling"
    "$PLTROOT/bin/mzc" -k "$1.ss"
    echo "$1: running"
    "$MZ" -t "$1.ss" 2>&1 >> "$LOGS/$1.log" &
    echo "$!" > "$LOGS/$1.pid"
    wait "$!"
    echo "$1: died"
    rm "$LOGS/$1.pid"
    if [[ "x$2" = "xyes" ]]; then
      echo "killing processes"
      kill_all
    fi
  done
}

exec 2>&1 >> "$LOGS/meta.log"

run_loop render &
run_loop main yes &
