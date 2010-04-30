#lang scribble/doc
@(require "base.rkt")

@title[#:tag  "ui"]{User Interfaces}

RacUnit provides a textual and a graphical user interface

@section{Textual User Interface}

@defmodule[racunit/text-ui]

The textual UI is in the @racketmodname[racunit/text-ui] module.
It is run via the @racket[run-tests] function.

@defproc[(run-tests (test (or/c test-case? test-suite?))
                    (verbosity (symbols 'quiet 'normal 'verbose) 'normal))
         natural-number/c]{

The given @racket[test] is run and the result of running it
output to the @racket[current-output-port].  The output is
compatable with the (X)Emacs next-error command (as used,
for example, by (X)Emacs's compile function)

The optional @racket[verbosity] is one of @racket['quiet],
@racket['normal], or @racket['verbose].  Quiet output
displays only the number of successes, failures, and errors.
Normal reporting suppresses some extraneous check
information (such as the expression).  Verbose reports all
information.

@racket[run-tests] returns the number of unsuccessful tests.}


@section{Graphical User Interface}

@defmodule[racunit/gui]

RacUnit also provides a GUI test runner, available from the
@racketmodname[racunit/gui] module.

@defproc[(test/gui [test (or/c test-case? test-suite?)] ...)
         any]{

Creates a new RacUnit GUI window and runs each @racket[test]. The
GUI is updated as tests complete.

}

@defproc[(make-gui-runner)
         (-> (or/c test-case? test-suite?) ... any)]{

Creates a new RacUnit GUI window and returns a procedure that, when
applied, runs the given tests and displays the results in the GUI.

}
