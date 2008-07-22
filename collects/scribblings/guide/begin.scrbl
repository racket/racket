#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "begin"]{Sequencing}

Scheme programmers prefer to write programs with as few side-effects
as possible, since purely functional code is more easily tested and
composed into larger programs. Interaction with the external
environment, however, requires sequencing, such as when writing to a
display, opening a graphical window, or manipulating a file on disk.

@;------------------------------------------------------------------------
@section{Effects Before: @scheme[begin]}

@refalso["begin"]{@scheme[begin]}

A @scheme[begin] expression sequences expressions:

@specform[(begin expr ...+)]{}

The @scheme[_expr]s are evaluated in order, and the result of all but
the last @scheme[_expr] is ignored. The result from the last
@scheme[_expr] is the result of the @scheme[begin] form, and it is in
tail position with respect to the @scheme[begin] form.

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

Many forms, such as @scheme[lambda] or @scheme[cond] support a
sequence of expressions even without a @scheme[begin]. Such positions are
sometimes said to have an @defterm{implicit begin}.

@defexamples[
(define (print-triangle height)
  (cond
    [(positive? height)
     (display (make-string height #\*))
     (newline)
     (print-triangle (sub1 height))]))
(print-triangle 4)
]

The @scheme[begin] form is special at the top level, at module level,
or as a @scheme[body] after only internal definitions. In those
positions, instead of forming an expression, the content of
@scheme[begin] is spliced into the surrounding context.

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
@section{Effects After: @scheme[begin0]}

@refalso["begin"]{@scheme[begin0]}

A @scheme[begin0] expression has the same syntax as a @scheme[begin]
expression:

@specform[(begin0 expr ...+)]{}

The difference is that @scheme[begin0] returns the result of the first
@scheme[expr], instead of the result of the last @scheme[expr]. The
@scheme[begin0] form is useful for implementing side-effects that
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
@section[#:tag "when+unless"]{Effects If...: @scheme[when] and @scheme[unless]}

@refalso["when+unless"]{@scheme[when] and @scheme[unless]}

The @scheme[when] form combines an @scheme[if]-style conditional with
sequencing for the ``then'' clause and no ``else'' clause:

@specform[(when test-expr then-expr ...)]

If @scheme[_test-expr] produces a true value, then all of the
@scheme[_then-expr]s are evaluated. Otherwise, no @scheme[_then-expr]s
are evaluated. The result is @|void-const| in any case.

The @scheme[unless] form is similar:

@specform[(unless test-expr then-expr ...)]

The difference is that the @scheme[_test-expr] result is inverted: the
@scheme[_then-expr]s are evaluated only if the @scheme[_test-expr]
result is @scheme[#f].

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
