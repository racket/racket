#lang scribble/doc
@(require scribble/manual "common.rkt"
          (for-label racket/base raco/command-name racket/cmdline))

@title[#:tag "command"]{Adding a @exec{raco} Command}

The set of commands supported by @exec{raco} can be extended by
installed packages, @|PLaneT| packages, and other collections. A
command is added by defining @indexed-racket[raco-commands] in the
@filepath{info.rkt} library of a collection (see @secref["info.rkt"]),
and then @exec{raco setup} (as called directly or as part of a package
or @|PLaneT| installation) must index the @filepath{info.rkt} file.

The value bound to @racket[raco-commands] must be a list of command
specifications, where each specification is a list of four values:

@racketblock[
   (list _command-string
         _implementation-module-path
         _description-string
         _prominence)
]

The @racket[_command-string] is the command name. Any unambiguous
prefix of a command name can be supplied to @exec{raco} to invoke the
command.

The @racket[_implementation-module-path] names the implementation though a
module path (in the sense of @racket[module-path?]). The module is loaded and
invoked through @racket[dynamic-require] to run the command. The
module can access command-line arguments through the
@racket[current-command-line-arguments] parameter, which is adjusted
before loading the command module to include only the arguments to the
command. The @racket[current-command-name] parameter is also set to
the command name used to load the command. When @exec{raco help} is
used on a command, the command is launched with an initial
@DFlag{help} argument in @racket[current-command-line-arguments].

The @racket[_description-string] is a short string used to describe the
command in response to @exec{raco help}. The description should not be
capitalized or end with a period.

The @racket[_prominence] value should be a read number or
@racket[#f]. A @racket[#f] value means that the command should not be
included in the short list of ``frequently used commands.'' A number
indicates the relative prominence of the command; the @exec{help}
command has a value of @racket[110], and probably no command should be
more prominent. The @exec{pack} tool, which is currently ranked as the
least-prominent of the frequently used commands, has a value of
@racket[10].

As an example, the @filepath{info.rkt} of the @filepath{compiler} collection
might contain the

@racketblock[
 (define raco-commands
   '(("make" compiler/commands/make "compile source to bytecode" 100)
     ("decompile" compiler/commands/decompile "decompile bytecode" #f)))
]

so that @exec{make} is treated as a frequently used command, while
@exec{decompile} is available as an infrequently used command.

@section{Command Argument Parsing}

@defmodule[raco/command-name]{The @racketmodname[raco/command-name]
library provides functions to help a @exec{raco} command identify
itself to users.}

@defparam[current-command-name name (or/c string? #f)]{

The name of the command currently being loaded via
@racket[dynamic-require], or @racket[#f] if @exec{raco} is not loading
any command.

A command implementation can use this parameter to determine whether
it was invoked via @exec{raco} or through some other means.}

@defproc[(short-program+command-name) string?]{

Returns a string that identifies the current command. When
@racket[current-command-name] is a string, then the result is the
short name of the @exec{raco} executable followed by a space and the
command name. Otherwise, it is the short name of the current
executable, as determined by stripping the path from the result of
@racket[(find-system-path 'run-file)].

The result of this function is suitable for use with
@racket[command-line]. For example, the @exec{decompile} tool parses
command-line arguments with

@racketblock[
 (define source-files
   (command-line
    #:program (short-program+command-name)
    #:args source-or-bytecode-file
    source-or-bytecode-file))
]

so that @exec{raco decompile --help} prints

@verbatim[#:indent 2]{
 raco decompile [ <option> ... ] [<source-or-bytecode-file>] ...
 where <option> is one of
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
}}

@defproc[(program+command-name) string?]{

Like @racket[short-program+command-name], but the path (if any) is not
stripped from the current executable's name.}
