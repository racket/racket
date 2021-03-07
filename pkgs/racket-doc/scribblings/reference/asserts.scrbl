#lang scribble/doc
@(require "mz.rkt" (for-label racket/unreachable racket/unsafe/ops))

@title[#:tag "asserts"]{Assertions}

@;------------------------------------------------------------------------
@section{Unreachable Code}

@note-lib-only[racket/unreachable]

@history[#:added "8.0.0.11"]

@defproc[(assert-unreachable) any]{
 Creates an @racket[exn:fail:contract] value and @racket[raise]s it as
 an exception to report unreachable code reached.
 
 Safe version of @racket[unsafe-assert-unreachable].
}

@defform[(with-assert-unreachable
           body ...+)]{
 Equivalent to @racketblock[
 (if (variable-reference-from-unsafe? (#%variable-reference))
     (unsafe-assert-unreachable)
     (let-values () body ...+))]
 It serves as an alternative to @racket[assert-unreachable] for more
 informative error message:
 @racketblock[
  (define (my-car x)
    (unless (pair? x)
      (with-assert-unreachable
        (raise-argument-error 'my-car "pair?" x)))
    (car x))
 ]
}


