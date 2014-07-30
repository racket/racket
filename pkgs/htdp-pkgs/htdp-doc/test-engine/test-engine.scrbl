#lang scribble/doc

@(require (for-label racket/base test-engine/racket-tests (prefix-in gui: test-engine/racket-gui)))

@(require scribble/manual scribble/eval racket/sandbox)
@(define-syntax-rule (mk-eval defs ...)
  ;; ==> 
  (let ([me (make-base-eval)])
    (call-in-sandbox-context me (lambda () (error-print-source-location #f)))
    (interaction-eval #:eval me defs) 
    ...
    me))

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

@defform[(check-expect expr expected-expr)]{
Checks whether the value of the @racket[expr] expression is
@racket[equal?] to the value produced by the @racket[expected-expr].

It is an error for @racket[expr] or @racket[expected-expr] to produce a function
value or an inexact number.}

@defform[(check-random expr expected-expr)]{
Checks whether the value of the @racket[expr] expression is
@racket[equal?] to the value produced by the @racket[expected-expr].

The form supplies the same random-number generator to both parts. If both
parts request @racket[random] numbers from the same interval in the same
order, they receive the same random numbers. 

@examples[#:eval (mk-eval (require test-engine/racket-tests))

(check-random (random 10) (random 10))

(check-random 
  (begin (random 100) (random 200))
  (begin (random 100) (random 200)))

(test)
]

If the two parts call @racket[random] for different intervals, they are
likely to fail: 

@examples[#:eval (mk-eval (require test-engine/racket-tests))
(check-random 
  (begin (random 100) (random 200))
  (begin (random 200) (random 100)))

(test)
]

It is an error for @racket[expr] or @racket[expected-expr] to produce a function
value or an inexact number.}

@defform[(check-satisfied expr property?)]{
Checks whether the value of the @racket[expr] expression satisfies 
the @racket[property?] predicate (which must evaluate to a function of one
argument). 

@examples[#:eval (mk-eval (require test-engine/racket-tests))

(check-satisfied 1 odd?)

(check-satisfied 1 even?)

(test)
]} 

@defform[(check-within expr expected-expr delta-expr)
          #:contracts ([delta-expr number?])]{
Checks whether the value of the @racket[test] expression is structurally
equal to the value produced by the @racket[expected] expression; every
number in the first expression must be within @racket[delta] of the
corresponding number in the second expression.

It is an error for @racket[expr] or @racket[expected] to produce a function
value.} 

@defform*[ [(check-error expr)
            (check-error expr msg-expr)]
            #:contracts ([msg-expr string?]) ]{
Checks that evaluating @racket[expr] signals an error, where
the error message matches the string (if any).}

@defform[(check-member-of expr expected-expr ...)]{
Checks whether the value of the @racket[expr] expression is @racket[equal?]
to any of the values produced by the @racket[expected-expr]s.

It is an error for @racket[expr] or any of the @racket[expected-expr]s
to produce a function value or an inexact number.}

@defform[(check-range expr min-expr max-expr)
         #:contracts ([expr number?]
                      [min-expr number?]
                      [max-expr number?])]{
Checks whether value of @racket[expr] is between the values of
@racket[min-expr] and @racket[max-expr] inclusive.}

@defform[(test)]{

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

This module requires produces an independent window when
displaying test results.  It provides the same bindings as
@racket[test-engine/racket-tests].
