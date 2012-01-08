# these should be writable
cache="/tmp/racket-build-status-cache"
cachelock="$cache-lock"

printf 'Content-type: text/plain\r\n\r\n'

# cache status reports (avoids excessive work during builds)

# use a lockfile as a cheap hack to time cache refreshing
if ! lockfile -r 0 -l 5 -s 0 "$cachelock" >& /dev/null \
   && [[ -e "$cache" ]]; then
  cat "$cache"; exit
fi

{

files=""
if [[ -e "$lockfile"        ]]; then L="Y";  else L="N";  fi
if [[ -e "$statusfile"      ]]; then S="Y";  else S="N";  fi
if [[ -e "$statusfile_last" ]]; then S1="Y"; else S1="N"; fi

if [[ "$L$S" = "NY" ]]; then
  printf 'Last build crashed abnormally.\n'
elif [[ "$S" = "Y" ]]; then
  time_for_file() {
    local t="$(($(date +"%s") - $(stat -c "%Z" "$1")))"
    printf "%d:%02d:%02d" "$((t/3600))" "$(((t%3600)/60))" "$((t%60))"
  }
  printf 'A build is running (%s)\n' "$(time_for_file "$lockfile")"
  printf 'Status: %s (%s)\n' "$(cat "$statusfile")" \
                             "$(time_for_file "$statusfile")"
  shopt -s nullglob
  if [[ "x$(echo "$bglogfile"*)" != "x" ]]; then
    printf '\n%s build jobs running:\n' "$(ls "$bglogfile"* | wc -l)"
    for bg in "$bglogfile"*; do
      s="$(grep "^### <<< .* >>>" "$bg" | tail -1 \
           | sed -e 's/^### <<< \(.*\) >>>/\1/')"
      if [[ "x$s" = "x" ]]; then
        printf '  %s: (just starting)\n' "${bg#$bglogfile-}"
      else
        s="${bg#$bglogfile-}: $s"
        s="$(echo "$s" \
           | sed -e 's/^\(.*\): \(.*\) \[\1(\(.*\))\]$/\3(\1): \2/')"
        echo "  $s"
      fi
    done
  fi
else
  printf 'No build is running.\n'
  if [[ "$S1" = "Y" ]]; then
    last="$(cat "$statusfile_last")"
    if [[ "x$last" = "xDone ("*")" ]]; then
      last="${last#Done (}"
      last="${last%)}"
      printf 'Last build successfully ended at %s\n' "$last"
    else
      printf 'Last build was unsuccessful (while: %s)\n' "$last"
    fi
  fi
  if [[ "$L" = "Y" ]]; then
    printf '(Builds temporarily disabled)\n'
  fi
fi

} > "$cache" 2>&1
cat "$cache"
