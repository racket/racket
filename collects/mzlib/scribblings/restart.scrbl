#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/restart
                     mzlib/cmdline))

@mzlib[#:mode title restart]

@margin-note{See @scheme[scheme/sandbox] for a more general way to
             simulate running a new Racket process.}

@defproc[(restart-mzscheme [init-argv (vectorof string?)]
                           [adjust-flag-table (any/c . -> . any/c)]
                           [argv (vectorof string?)]
                           [init-namespace (-> any)])
         boolean?]{

Simulates starting Racket with the vector of command-line strings
@scheme[argv]. The @scheme[init-argv], @scheme[adjust-flag-table], and
@scheme[init-namespace] arguments are used to modify the default
settings for command-line flags, adjust the parsing of command-line
flags, and customize the initial namespace, respectively.

The vector of strings @scheme[init-argv] is read first with the
standard Racket command-line parsing. Flags that load files or
evaluate expressions (e.g., @Flag{f} and @Flag{e}) are ignored, but
flags that set Racket's modes (e.g., @Flag{c} or @Flag{j})
effectively set the default mode before @scheme[argv] is parsed.

Before @scheme[argv] is parsed, the procedure
@scheme[adjust-flag-table] is called with a command-line flag table as
accepted by @scheme[parse-command-line]. The return value must also be
a table of command-line flags, and this table is used to parse
@scheme[argv]. The intent is to allow @scheme[adjust-flag-table] to
add or remove flags from the standard set.

After @scheme[argv] is parsed, a new thread and a namespace are
created for the ``restarted'' Racket. (The new namespace is
installed as the current namespace in the new thread.) In the new
thread, restarting performs the following actions:

@itemize[

 @item{The @scheme[init-namespace] procedure is called with no
       arguments.  The return value is ignored.}

 @item{Expressions and files specified by @scheme[argv] are evaluated
       and loaded.  If an error occurs, the remaining expressions and
       files are ignored, and the return value for
       @scheme[restart-mzscheme] is set to @scheme[#f].}

 @item{The @scheme[read-eval-print-loop] procedure is called, unless a
       flag in @scheme[init-argv] or @scheme[argv] disables it. When
       @scheme[read-eval-print-loop] returns, the return value for
       @scheme[restart-mzscheme] is set to @scheme[#t].}

]

Before evaluating command-line arguments, an exit handler is installed
that immediately returns from @scheme[restart-mzscheme] with the value
supplied to the handler. This exit handler remains in effect when
@scheme[read-eval-print-loop] is called (unless a command-line
argument changes it). If @scheme[restart-mzscheme] returns normally,
the return value is determined as described above.

Note that an error in a command-line expression followed by
@scheme[read-eval-print-loop] produces a @scheme[#t] result. This is
consistent with Racket's stand-alone behavior.}
