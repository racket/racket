#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/restart
                     mzlib/cmdline))

@mzlib[#:mode title restart]

@deprecated[@racketmodname[racket/sandbox]]{
The @racket[racket/sandbox] library provides a more general way to
simulate running a new Racket process.
}

@defproc[(restart-mzscheme [init-argv (vectorof string?)]
                           [adjust-flag-table (any/c . -> . any/c)]
                           [argv (vectorof string?)]
                           [init-namespace (-> any)])
         boolean?]{

Simulates starting Racket with the vector of command-line strings
@racket[argv]. The @racket[init-argv], @racket[adjust-flag-table], and
@racket[init-namespace] arguments are used to modify the default
settings for command-line flags, adjust the parsing of command-line
flags, and customize the initial namespace, respectively.

The vector of strings @racket[init-argv] is read first with the
standard Racket command-line parsing. Flags that load files or
evaluate expressions (e.g., @Flag{f} and @Flag{e}) are ignored, but
flags that set Racket's modes (e.g., @Flag{c} or @Flag{j})
effectively set the default mode before @racket[argv] is parsed.

Before @racket[argv] is parsed, the procedure
@racket[adjust-flag-table] is called with a command-line flag table as
accepted by @racket[parse-command-line]. The return value must also be
a table of command-line flags, and this table is used to parse
@racket[argv]. The intent is to allow @racket[adjust-flag-table] to
add or remove flags from the standard set.

After @racket[argv] is parsed, a new thread and a namespace are
created for the ``restarted'' Racket. (The new namespace is
installed as the current namespace in the new thread.) In the new
thread, restarting performs the following actions:

@itemize[

 @item{The @racket[init-namespace] procedure is called with no
       arguments.  The return value is ignored.}

 @item{Expressions and files specified by @racket[argv] are evaluated
       and loaded.  If an error occurs, the remaining expressions and
       files are ignored, and the return value for
       @racket[restart-mzscheme] is set to @racket[#f].}

 @item{The @racket[read-eval-print-loop] procedure is called, unless a
       flag in @racket[init-argv] or @racket[argv] disables it. When
       @racket[read-eval-print-loop] returns, the return value for
       @racket[restart-mzscheme] is set to @racket[#t].}

]

Before evaluating command-line arguments, an exit handler is installed
that immediately returns from @racket[restart-mzscheme] with the value
supplied to the handler. This exit handler remains in effect when
@racket[read-eval-print-loop] is called (unless a command-line
argument changes it). If @racket[restart-mzscheme] returns normally,
the return value is determined as described above.

Note that an error in a command-line expression followed by
@racket[read-eval-print-loop] produces a @racket[#t] result. This is
consistent with Racket's stand-alone behavior.}
