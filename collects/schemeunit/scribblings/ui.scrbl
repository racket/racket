#lang scribble/doc
@(require "base.ss")

@title[#:tag  "ui"]{User Interfaces}

SchemeUnit provides a textual and a graphical user interface

@section{Textual User Interface}

@defmodule[schemeunit/text-ui]

The textual UI is in the @schememodname[schemeunit/text-ui] module.
It is run via the @scheme[run-tests] function.

@defproc[(run-tests (test (or/c test-case? test-suite?))
                    (verbosity (symbols 'quite 'normal 'verbose) 'normal))
         natural-number/c]{

The given @scheme[test] is run and the result of running it
output to the @scheme[current-output-port].  The output is
compatable with the (X)Emacs next-error command (as used,
for example, by (X)Emac's compile function)

The optional @scheme[verbosity] is one of @scheme['quiet],
@scheme['normal], or @scheme['verbose].  Quiet output
displays only the number of successes, failures, and errors.
Normal reporting suppresses some extraneous check
information (such as the expression).  Verbose reports all
information.

@scheme[run-tests] returns the number of unsuccessful tests.}


@section{Graphical User Interface}

@defmodule[schemeunit/gui]

SchemeUnit also provides a GUI test runner, available from the
@schememodname[schemeunit/gui] module.

@defproc[(test/gui [test (or/c test-case? test-suite?)] ...)
         any]{

Creates a new SchemeUnit GUI window and runs each @scheme[test]. The
GUI is updated as tests complete.

}

@defproc[(make-gui-runner)
         (-> (or/c test-case? test-suite?) ... any)]{

Creates a new SchemeUnit GUI window and returns a procedure that, when
applied, runs the given tests and displays the results in the GUI.

}
