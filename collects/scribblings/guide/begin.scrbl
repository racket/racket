#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "begin"]{Sequencing}

Racket programmers prefer to write programs with as few side-effects
as possible, since purely functional code is more easily tested and
composed into larger programs. Interaction with the external
environment, however, requires sequencing, such as when writing to a
display, opening a graphical window, or manipulating a file on disk.

@;------------------------------------------------------------------------
@section{Effects Before: @racket[begin]}

@refalso["begin"]{@racket[begin]}

A @racket[begin] expression sequences expressions:

@specform[(begin expr ...+)]{}

The @racket[_expr]s are evaluated in order, and the result of all but
the last @racket[_expr] is ignored. The result from the last
@racket[_expr] is the result of the @racket[begin] form, and it is in
tail position with respect to the @racket[begin] form.

@defexamples[
(define (print-triangle height)
  (if (zero? height)
      (void)
      (begin
        (display (make-string height #\*))
        (newline)
        (print-triangle (sub1 height)))))
(print-triangle 4)
]

Many forms, such as @racket[lambda] or @racket[cond] support a
sequence of expressions even without a @racket[begin]. Such positions are
sometimes said to have an @deftech{implicit begin}.

@defexamples[
(define (print-triangle height)
  (cond
    [(positive? height)
     (display (make-string height #\*))
     (newline)
     (print-triangle (sub1 height))]))
(print-triangle 4)
]

The @racket[begin] form is special at the top level, at module level,
or as a @racket[body] after only internal definitions. In those
positions, instead of forming an expression, the content of
@racket[begin] is spliced into the surrounding context.

@defexamples[
(let ([curly 0])
  (begin
    (define moe (+ 1 curly))
    (define larry (+ 1 moe)))
  (list larry curly moe))
]

This splicing behavior is mainly useful for macros, as we discuss
later in @secref["macros"].

@;------------------------------------------------------------------------
@section{Effects After: @racket[begin0]}

@refalso["begin"]{@racket[begin0]}

A @racket[begin0] expression has the same syntax as a @racket[begin]
expression:

@specform[(begin0 expr ...+)]{}

The difference is that @racket[begin0] returns the result of the first
@racket[expr], instead of the result of the last @racket[expr]. The
@racket[begin0] form is useful for implementing side-effects that
happen after a computation, especially in the case where the
computation produces an unknown number of results.

@defexamples[
(define (log-times thunk)
  (printf "Start: ~s\n" (current-inexact-milliseconds))
  (begin0
    (thunk)
    (printf "End..: ~s\n" (current-inexact-milliseconds))))
(log-times (lambda () (sleep 0.1) 0))
(log-times (lambda () (values 1 2)))
]

@;------------------------------------------------------------------------
@section[#:tag "when+unless"]{Effects If...: @racket[when] and @racket[unless]}

@refalso["when+unless"]{@racket[when] and @racket[unless]}

The @racket[when] form combines an @racket[if]-style conditional with
sequencing for the ``then'' clause and no ``else'' clause:

@specform[(when test-expr then-body ...+)]

If @racket[_test-expr] produces a true value, then all of the
@racket[_then-body]s are evaluated. The result of the last
@racket[_then-body] is the result of the @racket[when] form.
Otherwise, no @racket[_then-body]s are evaluated and the
result is @|void-const|.

The @racket[unless] form is similar:

@specform[(unless test-expr then-body ...+)]

The difference is that the @racket[_test-expr] result is inverted: the
@racket[_then-body]s are evaluated only if the @racket[_test-expr]
result is @racket[#f].

@defexamples[
(define (enumerate lst)
  (if (null? (cdr lst))
      (printf "~a.\n" (car lst))
      (begin
        (printf "~a, " (car lst))
        (when (null? (cdr (cdr lst)))
          (printf "and "))
        (enumerate (cdr lst)))))
(enumerate '("Larry" "Curly" "Moe"))
]

@def+int[
(define (print-triangle height)
  (unless (zero? height)
    (display (make-string height #\*))
    (newline)
    (print-triangle (sub1 height))))
(print-triangle 4)
]
