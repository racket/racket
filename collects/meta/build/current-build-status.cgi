# these should be writable (for the web server)
cache="/tmp/racket-build-status-cache"
cachelock="$cache-lock"
requestfile="/tmp/racket-build-request"
requeststatusfile="/tmp/racket-build-request-status"

printf 'Content-type: text/plain\r\nAccess-Control-Allow-Origin: *\r\n\r\n'

if [[ "$PATH_INFO" = "/request" ]]; then
  error() { echo "Error: $*"; exit 0; }
  if [[ -e "$lockfile" ]]; then
    if [[ -e "$statusfile" ]]; then error "a build is in progress"
    else error "builds temporarily disabled"; fi
  fi
  request_rx='^([^&@]+@racket-lang[.]org)&([^&]+)&([0-9]+)$'
  if [[ ! "$QUERY_STRING" =~ $request_rx ]]; then error "invalid request"; fi
  username="${BASH_REMATCH[1]}"
  branch="${BASH_REMATCH[2]}"
  cookie="${BASH_REMATCH[3]}"
  date="$(date +'%Y-%m-%d %H:%M')"
  prevuser=""
  if [[ -e "$requestfile" ]]; then
    prevuser="$(cat "$requestfile" | head -1)"
    rm -f "$requestfile" || error "could not remove previous request file"
    rm -f "$requeststatusfile"
  fi
  touch "$requestfile" || error "could not create request file"
  { echo "$username"; echo "$branch"; echo "$date"; echo "$cookie"; } \
    > "$requestfile"
  if [[ "x$prevuser" = "x" ]]; then
    echo "Request created for $username"
  elif [[ "x$prevuser" = "x$username" ]]; then
    echo "Request re-created for $username"
  else
    echo "Request created for $username, overwriting request for $prevuser"
  fi
  exit 0
fi

###############################################################################
# status reporting

# cache status reports (avoids excessive work during builds)
# use a lockfile as a cheap hack to time cache refreshing
if ! lockfile -r 0 -l 25 -s 0 "$cachelock" >& /dev/null \
   && [[ -e "$cache" ]]; then
  cat "$cache"; exit
fi

{

check_exists() { if [[ -e "$2" ]]; then eval "$1=Y"; else eval "$1=N"; fi; }
check_exists L  "$lockfile"
check_exists S  "$statusfile"
check_exists SL "$statusfile_last"
check_exists R  "$requestfile"
check_exists RS "$requeststatusfile"

if [[ "$L$S" = "YY" ]]; then
  time_for_file() {
    local t="$(($(date +"%s") - $(stat -c "%Z" "$1")))"
    printf "%d:%02d:%02d" "$((t/3600))" "$(((t%3600)/60))" "$((t%60))"
  }
  printf '{{{LINKTO: %s}}}' "current-$(basename "$buildlogfile")"
  printf 'A build is running (%s)\n' "$(time_for_file "$lockfile")"
  printf 'Status: %s (%s)\n' "$(cat "$statusfile")" \
                             "$(time_for_file "$statusfile")"
  shopt -s nullglob
  if [[ "x$(echo "$bglogfile"*)" != "x" ]]; then
    printf '\n%s build jobs running:\n' "$(ls "$bglogfile"* | wc -l)"
    for bg in "$bglogfile"*; do
      s="$(grep "^### <<< .* >>>" "$bg" | tail -1 \
           | sed -e 's/([^()]* build) //' \
           | sed -e 's/^### <<< \(.*\) >>>/\1/')"
      printf '{{{LINKTO: %s}}}' "current-$(basename "$bg")"
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
  if [[ "$L" = "Y" ]]; then
    # lockfile exists, but no statusfile
    printf '(Builds temporarily disabled.)\n'
  elif [[ "$S" = "Y" ]]; then
    # statusfile exists, but no lockfile
    printf '(Last build crashed abnormally: status file not removed.)\n'
  fi
  if [[ "$R" = "Y" ]]; then
    echo ""
    { read R_user; read R_branch; read R_date; } < "$requestfile"
    printf 'Pending build request for %s' "$R_user"
    if [[ "x$R_branch" != "xmaster" ]]; then
      printf ' (%s branch)' "$R_branch"
    fi
    echo " made at $R_date"
    if [[ "$RS" = "Y" ]]; then awk '{ print "  " $0 }' < "$requeststatusfile"
    else echo "  The request is fresh, and was not noticed by the system."; fi
  fi
  if [[ "$SL" = "Y" ]]; then
    echo ""
    last="$(cat "$statusfile_last")"
    printf '{{{LINKTO: %s}}}' "current-$(basename "$buildlogfile")"
    if [[ "x$last" = "xDone ("*")" ]]; then
      last="${last#Done (}"
      last="${last%)}"
      printf 'Last build successfully ended at %s\n' "$last"
    elif [[ "x$last" = "x("*" build) Done ("*")" ]]; then
      last="${last#(}"
      build="${last% build) Done*}"
      last="${last#*) Done (}"
      last="${last%)}"
      printf 'Last %s build successfully ended at %s\n' "$build" "$last"
    else
      printf 'Last build was unsuccessful (%s)\n' "$last"
    fi
  fi
fi

} > "$cache.$$" 2>&1
mv "$cache.$$" "$cache"
cat "$cache"
