# -*- mode: shell-script; sh-basic-offset: 2; indent-tabs-mode: nil -*-
# ex: ts=2 sw=2 noet filetype=sh

# To enable this, add the following line to ~/.bash_completion
#
#     source $PLTHOME/collects/meta/contrib/completion/racket-completion.bash
#
# Change $PLTHOME to whatever references your Racket installation.  You
# will need to make sure that you have bash completions enabled, usually
# with "source /etc/bash_completion".

# Identify bash as the completion client
export RACKET_COMPLETION_CLIENT=bash

# This completes only *.{rkt,ss,scm,scrbl} files unless there are none,
# in which case it completes other things.
_racket_filedir() {
  COMPREPLY=()
  _filedir "@(rkt|rktl|ss|scm|scrbl)"
  if [[ "${#COMPREPLY[@]}" -eq 0 ]]; then _filedir; fi
}

_find_exe() {
  local exename="$1"
  local dir="$(dirname "${COMP_WORDS[0]}")"
  local exe="$(basename "${COMP_WORDS[0]}")"
  if [[ "$dir" != "." || "${COMP_WORDS[0]}" = "$dir/$exe" ]]; then
    echo "$dir/$exename"
  else
    echo "$exename"
  fi
}

_racket() {
  local cur prev singleopts doubleopts
  COMPREPLY=()
  cur="$(_get_cword)"
  prev="${COMP_WORDS[COMP_CWORD-1]}"
  doubleopts="--help --version --eval --load --require --lib --script"
  doubleopts+=" --require-script --main --repl --no-lib --version --warn"
  doubleopts+=" --syslog --collects --search --addon --no-compiled"
  doubleopts+=" --no-init-file"
  singleopts="-h -e -f -t -l -p -r -u -k -m -i -n -v -W -L -X -S -A -I -U"
  singleopts+=" -N -j -d -b -c -q"
  warnlevels="none fatal error warning info debug"

  # if "--" is already given, complete all kind of files, but no options
  for (( i=0; i < ${#COMP_WORDS[@]}-1; i++ )); do
    if [[ "${COMP_WORDS[i]}" == "--" ]]; then _racket_filedir; return; fi
  done

  # -k takes *two* integer arguments
  if [[ 2 < "${#COMP_WORDS[@]}" ]]; then
    if [[ "${COMP_WORDS[COMP_CWORD-2]}" == "-k" ]]; then return; fi
  fi

  case "$cur" in
    "--"* )
      COMPREPLY=( $(compgen -W "$doubleopts" -- "$cur") )
      ;;
    "-"* )
      COMPREPLY=( $(compgen -W "$singleopts" -- "$cur") )
      ;;
    * )
      case "$prev" in
        # these do not take anything completable as arguments
        "--help" | "-h" | "-e" | "--eval" | "-p" | "-k" )
          ;;
        # these take dirs (not files) as arguments
        "-X" | "-S" | "-A" | "--collects" | "--search" | "--addon" )
          _filedir -d
          ;;
        # these take warnlevels as arguments
        "-W" | "--warn" | "-L" | "--syslog" )
          COMPREPLY=( $(compgen -W "$warnlevels" -- "$cur") )
          ;;
        # otherwise, just a file
        * )
          _racket_filedir
          ;;
      esac
      ;;
  esac
}

complete -F _racket racket
complete -F _racket gracket
complete -F _racket gracket-text

_raco_cmd="$(_find_exe "raco")"

_raco_planet() {
  local cur="${COMP_WORDS[COMP_CWORD]}"
  local planetcmds=$(
    printf '%s\n' "--help"
    "${_raco_cmd}" planet --help 2>&1 | awk '/^  *raco planet / { print $3 }'
  )
  COMPREPLY=( $(compgen -W "$planetcmds" -- "$cur") )
}

_raco_cmds=$()
_racket_cmd="$(_find_exe "racket")"

_raco_help() {
  local cur="${COMP_WORDS[COMP_CWORD]}"
  if [[ ${#_raco_cmds[@]} -eq 0 ]]; then
    _raco_cmds=$(
      echo "help"
      "$_racket_cmd" -e '(begin (require raco/all-tools)
                                (for ([(k v) (all-tools)]) (printf "~a\n" k)))'
    )
  fi
  COMPREPLY=( $(compgen -W "$_raco_cmds" -- "$cur") )
}

_racket_collects=()

_complete_collects() {
  local cur="$1"
  if [[ "${#_racket_collects[@]}" -eq 0 ]]; then
    _racket_collects=(
      $( $_racket_cmd -e '(require (submod shell-completion/list-collects main))' )
    )
  fi
  local wordlist=""
  for dir in "${_racket_collects[@]}"; do
    wordlist="$wordlist $dir"
  done
  COMPREPLY=( $(compgen -W "$wordlist" -- "$cur") )
}

_raco_setup()
{
  local cur="${COMP_WORDS[COMP_CWORD]}"
  local prev="${COMP_WORDS[COMP_CWORD-1]}"

  if [[ "$COMP_CWORD" -eq 2 ]]; then
    _complete_collects ${cur}
  else
    case "${prev}" in
      # specifying a particular collection
      "-l" ) _complete_collects "$cur" ;;
      *    ) _filedir ;;
    esac
  fi
}

_raco() {
  COMPREPLY=()
  local cur="${COMP_WORDS[COMP_CWORD]}"

  #
  #  Complete the arguments to some of the basic commands.
  #
  local makeopts="--disable-inline --no-deps -p --prefix --no-prim -v -vv --help -h"

  if [[ "$COMP_CWORD" -eq 1 ]]; then
    # removing the empty string on the next line breaks things.  such as my brain.
    _raco_cmds="$(
      echo "help"
      $_racket_cmd -e '(begin (require raco/all-tools)
                              (for ([(k v) (all-tools)]) (printf "~a\n" k)))')"
    COMPREPLY=($(compgen -W "$_raco_cmds" -- "$cur"))
  elif [[ "$COMP_CWORD" -ge 2 ]]; then
    # Here we'll handle the main raco commands
    local prev="${COMP_WORDS[1]}"
    case "$prev" in
      "make" )
        case "$cur" in
          "-"* ) COMPREPLY=( $(compgen -W "$makeopts" -- "$cur") ) ;;
          *    ) _filedir ;;
        esac ;;
      "planet" ) _raco_planet ;;
      "help"   ) _raco_help ;;
      "setup"  ) _raco_setup ;;
      *        ) _filedir ;;
    esac
  else
    _filedir
  fi
}

complete -F _raco raco
