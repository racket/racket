#lang scribble/doc
@(require scribble/manual
          (for-label racket/base
                     test-engine/racket-tests
                     (prefix-in gui: test-engine/racket-gui)))

@title{Test Support}

@author["Kathryn Gray"]

@table-of-contents[]

@; ----------------------------------------------------------------------

@section{Using Check Forms}

@defmodule[test-engine/racket-tests]

This module provides test forms for use in Racket programs, as well
as parameters to configure the behavior of test reports.

Each check form may only occur at the top-level; results are collected
and reported by the test function.  Note that the check forms only
register checks to be performed.  The checks are actually run by the
@racket[test] function.

@defform[(check-expect (test any/c) (expected any/c))]{
Checks whether the value of the @racket[test] expression is structurally
equal to the value produced by the @racket[expected] expression.

It is an error for @racket[test] or @racket[expected] to produce a function
value or an inexact number.} 

@defform[(check-within (test any/c) (expected any/c) (delta number?))]{
Checks whether the value of the @racket[test] expression is structurally
equal to the value produced by the @racket[expected] expression; every
number in the first expression must be within @racket[delta] of the
corresponding number in the second expression.

It is an error for @racket[test] or @racket[expected] to produce a function
value.} 

@defform*[ [(check-error (test any/c))
            (check-error (test any/c) (msg string?))] ]{
Checks that evaluating @racket[test] signals an error, where
the error message matches the string (if any).}

@defform[(check-member-of (test any/c) (expected any/c) ...)]{
Checks whether the value of the @racket[test] expression is structurally
equal to any of the values produced by the @racket[expected] expressions.

It is an error for @racket[test] or any of the @racket[expected] expression
to produce a function value or an inexact number.}

@defform[(check-range (test number/c) (min number/c) (max number/c))]{
Checks whether value of @racket[test] is between the values of the
@racket[min] and @racket[max] expressions [inclusive].}

@defproc[(test) void?]{

Runs all of the tests specified by check forms in the current module
and reports the results.  When using the gui module, the results are
provided in a separate window, otherwise the results are printed to
the current output port.}

@defparam[test-format format (any/c . -> . string?)]{

A parameter that stores the formatting function for the values tested
by the check forms.}


@defboolparam[test-silence silence?]{

A parameter that stores a boolean, defaults to #f, that can be used to
suppress the printed summary from test.}


@defboolparam[test-execute execute?]{

A parameter that stores a boolean, defaults to #t, that can be used to
suppress evaluation of test expressions.
}

@section{GUI Interface}

@defmodule[test-engine/racket-gui]

@; FIXME: need to actually list the bindings here, so they're found in
@; the index

This module requires GRacket and produces an independent window when
displaying test results.  It provides the same bindings as
@racket[test-engine/racket-tests].

@section{Integrating languages with Test Engine}

@italic{(To be written.)}
