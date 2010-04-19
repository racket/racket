# -*- mode: shell-script; sh-basic-offset: 2; indent-tabs-mode: nil -*-
# ex: ts=2 sw=2 noet filetype=sh

# to enable this, add the following line to ~/.bash_completion you
# will need to make sure that you've enable bash completion more
# generally, usually via '. /etc/bash_completion'
#
#     source $PLTHOME/collects/meta/contrib/completion/racket-completion.bash
#
# Change $PLTHOME to whatever references your Racket installation

# this completes only *.{rkt,ss,scm,scrbl} files unless there are
# none, in which case it completes other things
_smart_filedir()
{
  COMPREPLY=()
  _filedir '@(rkt|ss|scm|scrbl)'
  if [[ ${#COMPREPLY[@]} -eq 0 ]]; then
    _filedir
  fi
  return 0
}

_racket()
{
  local cur prev singleopts doubleopts
  COMPREPLY=()
  cur=`_get_cword`
  prev="${COMP_WORDS[COMP_CWORD-1]}"
  doubleopts="--help --version --eval --load --require --lib --script --require-script\
 --main --repl --no-lib --version --warn --syslog --collects --search --addon --no-compiled --no-init-file"
  singleopts="-h -e -f -t -l -p -r -u -k  -m -i -n -v -W -L -X -S -A -I -U -N -j -d -b -c -q"
  warnlevels="none fatal error warning info debug"

  # if '--' is already given, complete all kind of files, but no options
  for (( i=0; i < ${#COMP_WORDS[@]}-1; i++ )); do
    if [[ ${COMP_WORDS[i]} == -- ]]; then
      _smart_filedir
      return 0
    fi
  done

  # -k takes *two* integer arguments
  if [[ 2 < ${#COMP_WORDS[@]} ]]; then
    if [[ ${COMP_WORDS[COMP_CWORD-2]} == -k ]]; then
      return 0
    fi
  fi

  
  case "${cur}" in
    --*)	
      COMPREPLY=( $(compgen -W "${doubleopts}" -- ${cur}) )
      ;;
    -*)
      COMPREPLY=( $(compgen -W "${singleopts}" -- ${cur}) )
      ;;
    *)
      case "${prev}" in
        # these do not take anything completable as arguments
        --help|-h|-e|--eval|-p|-k)
          ;;
        # these take dirs (not files) as arguments
        -X|-S|-A|--collects|--search|--addon)
          _filedir '-d'
          ;;
        # these take warnlevels as arguments
        -W|--warn|-L|--syslog)
          COMPREPLY=( $(compgen -W "${warnlevels}" -- ${cur}) )
          ;;
        # otherwise, just a file
        *)
          _smart_filedir 
          ;;
      esac
      ;;
  esac
  
  return 0
}
complete  -F _racket $filenames racket
complete  -F _racket $filenames gracket
complete  -F _racket $filenames gracket-text

_rico_planet()
{
    local cur="${COMP_WORDS[COMP_CWORD]}"
    local planetcmds=$( echo '' '--help' ;  for x in `rico planet --help 2>&1 | sed -n -e 's/^  \(.[^ ]*\).*/\1/p'` ; do echo ${x} ; done )
    COMPREPLY=( $(compgen -W "${planetcmds}" -- ${cur}) )
}

_rico()
{
    COMPREPLY=()
    local cur="${COMP_WORDS[COMP_CWORD]}"

    #
    #  Complete the arguments to some of the basic commands.
    #
    local makeopts="--disable-inline --no-deps -p --prefix --no-prim -v -vv --help -h"

    if [ $COMP_CWORD -eq 1 ]; then
      # removing the empty string on the next line breaks things.  such as my brain.
      local cmds=$( echo '' '--help' ;  for x in `racket -e '(begin (require rico/all-tools) (for ([(k v) (all-tools)]) (printf "~a\n" k)))'` ; do echo ${x} ; done )
      COMPREPLY=($(compgen -W "${cmds}" -- ${cur}))  
    elif [ $COMP_CWORD -eq 2 ]; then
      # Here we'll handle the main rico commands
      local prev="${COMP_WORDS[1]}"
      case "${prev}" in
        make)
          case "${cur}" in
            -*)
              COMPREPLY=( $(compgen -W "${makeopts}" -- ${cur}) )
              ;;
            *)
              _filedir
              ;;
          esac
          ;;
        planet)
          _rico_planet
          ;;
        --help)
          ;;
        *)
          _filedir
          ;;
      esac
    else
      _filedir
    fi
    return 0
}

complete -F _rico rico
