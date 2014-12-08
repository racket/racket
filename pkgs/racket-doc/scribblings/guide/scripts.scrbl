#lang scribble/doc
@(require scribble/manual scheme/cmdline "guide-utils.rkt")

@title[#:tag "scripts"]{Scripts}

Racket files can be turned into executable scripts on Unix and Mac
OS X.  On Windows, a compatibility layer like Cygwin support the
same kind of scripts, or scripts can be implemented as batch files.

@section{Unix Scripts}

In a Unix environment (including Linux and Mac OS X), a Racket file can
be turned into an executable script using the shell's @as-index{@tt{#!}}
convention. The first two characters of the file must be @litchar{#!};
the next character must be either a space or @litchar{/}, and the
remainder of the first line must be a command to execute the script. For
some platforms, the total length of the first line is restricted to 32
characters, and sometimes the space is required.

@margin-note{Use @racketmodfont{#lang} @racketmodname[racket/base] instead
of @racketmodfont{#lang} @racketmodname[racket] to produce scripts with a
faster startup time.}

The simplest script format uses an absolute path to a @exec{racket}
executable followed by a module declaration. For example, if
@exec{racket} is installed in @filepath{/usr/local/bin}, then a file
containing the following text acts as a ``hello world'' script:

@verbatim[#:indent 2]{
  #! /usr/local/bin/racket
  #lang racket/base
  "Hello, world!"
}

In particular, if the above is put into a file @filepath{hello} and
the file is made executable (e.g., with @exec{chmod a+x hello}), then
typing @exec{./hello} at the shell prompt produces the output
@tt{"Hello, world!"}.

The above script works because the operating system automatically puts
the path to the script as the argument to the program started by the
@tt{#!} line, and because @exec{racket} treats a single non-flag
argument as a file containing a module to run.

Instead of specifying a complete path to the @exec{racket}
executable, a popular alternative is to require that @exec{racket}
is in the user's command path, and then ``trampoline'' using
@exec{/usr/bin/env}:

@verbatim[#:indent 2]{
  #! /usr/bin/env racket
  #lang racket/base
  "Hello, world!"
}

In either case, command-line arguments to a script are available via
@racket[current-command-line-arguments]:

@verbatim[#:indent 2]{
  #! /usr/bin/env racket
  #lang racket/base
  (printf "Given arguments: ~s\n"
          (current-command-line-arguments))
}

If the name of the script is needed, it is available via
@racket[(find-system-path 'run-file)], instead of via
@racket[(current-command-line-arguments)].

Usually, then best way to handle command-line arguments is to parse
them using the @racket[command-line] form provided by
@racketmodname[racket]. The @racket[command-line] form extracts
command-line arguments from @racket[(current-command-line-arguments)]
by default:

@verbatim[#:indent 2]{
  #! /usr/bin/env racket
  #lang racket

  (define verbose? (make-parameter #f))

  (define greeting
    (command-line
     #:once-each
     [("-v") "Verbose mode" (verbose? #t)]
     #:args 
     (str) str))

  (printf "~a~a\n"
          greeting
          (if (verbose?) " to you, too!" ""))
}

Try running the above script with the @DFlag{help} flag to see what
command-line arguments are allowed by the script.

An even more general trampoline uses @exec{/bin/sh} plus some lines
that are comments in one language and expressions in the other. This
trampoline is more complicated, but it provides more control over
command-line arguments to @exec{racket}:

@verbatim[#:indent 2]|{
  #! /bin/sh
  #|
  exec racket -e '(printf "Running...\n")' -u "$0" ${1+"$@"}
  |#
  #lang racket/base
  (printf "The above line of output had been produced via\n")
  (printf "a use of the `-e' flag.\n")
  (printf "Given arguments: ~s\n"
          (current-command-line-arguments))
}|

Note that @litchar{#!} starts a line comment in Racket, and
@litchar{#|}...@litchar{|#} forms a block comment. Meanwhile,
@litchar{#} also starts a shell-script comment, while @exec{exec
racket} aborts the shell script to start @exec{racket}. That way,
the script file turns out to be valid input to both @exec{/bin/sh} and
@exec{racket}.

@section{Windows Batch Files}

A similar trick can be used to write Racket code in Windows
@as-index{@tt{.bat}} batch files:

@verbatim[#:indent 2]|{
  ; @echo off
  ; Racket.exe "%~f0" %*
  ; exit /b
  #lang racket/base
  "Hello, world!"
  }|

@;{
Original trick from Ben Goetter, who used:

  ; @echo off && REM -*- racket -*-
  ; "%RACKET%" "%~f0" %*
  ; exit /b
  #lang racket
  ...

it might be worth documenting the Emacs "-*-" convention and a way to
set environment variables -- but that would be needed in the unix part
too.
;}
