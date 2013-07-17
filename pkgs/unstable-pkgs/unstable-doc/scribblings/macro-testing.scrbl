#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label racket/base 
                     racket/contract
                     racket/struct-info
                     rackunit
                     unstable/macro-testing))

@(define the-eval (make-base-eval))
@(the-eval '(require rackunit unstable/macro-testing (for-syntax racket/base racket/struct-info)))

@title[#:tag "macro-testing"]{Macro Testing}
@unstable-header[]

@defmodule[unstable/macro-testing]

@defform/subs[(phase1-eval ct-expr maybe-quote maybe-catch?)
              ([maybe-quote (code:line)
                            (code:line #:quote quote-id)]
               [maybe-catch? (code:line)
                             (code:line #:catch? catch?)])]{

Evaluates @racket[ct-expr] at compile time and quotes the result using
@racket[quote-id], which defaults to @racket[quote]. Another suitable
argument for @racket[quote-id] is @racket[quote-syntax].

If @racket[catch?] is @racket[#t], then if the evaluation of
@racket[ct-expr] raises a compile-time exception, it is caught and
converted to a run-time exception.

@examples[#:eval the-eval
(struct point (x y))
(phase1-eval (extract-struct-info (syntax-local-value #'point)))
(phase1-eval (extract-struct-info (syntax-local-value #'point))
             #:quote quote-syntax)
]
}

@defform[(convert-compile-time-error expr)]{

Equivalent to @racket[(#%expression expr)] except if expansion of
@racket[expr] causes a compile-time exception to be raised; in that
case, the compile-time exception is converted to a run-time exception
raised when the expression is evaluated.

Use @racket[convert-compile-time-error] to write tests for
compile-time error checking like syntax errors:

@examples[#:eval the-eval
(check-exn #rx"missing an \"else\" expression"
           (lambda () (convert-compile-time-error (if 1 2))))
(check-exn #rx"missing formals and body"
           (lambda () (convert-compile-time-error (lambda))))
]

Without the use of @racket[convert-compile-time-error], the checks
above would not be executed because the test program would not compile.
}

@defform[(convert-syntax-error expr)]{

Like @racket[convert-compile-time-error], but only catches compile-time
@racket[exn:fail:syntax?] exceptions and sets
@racket[error-print-source-location] to @racket[#f] around the
expansion of @racket[expr] to make the message easier to match
exactly.

@examples[#:eval the-eval
(check-exn #rx"^lambda: bad syntax$"
           (lambda () (convert-syntax-error (lambda))))
]
}

@(close-eval the-eval)
