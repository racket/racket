#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/function))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/function))

@title{Functions}
@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defmodule[unstable/function]

This module provides tools for higher-order programming and creating functions.

@section{Higher Order Predicates}

@defproc[((conjoin [f (-> A ... boolean?)] ...) [x A] ...) boolean?]{

Combines calls to each function with @racket[and].  Equivalent to
@racket[(and (f x ...) ...)]

@defexamples[
#:eval the-eval
(define f (conjoin exact? integer?))
(f 1)
(f 1.0)
(f 1/2)
(f 0.5)
]

}

@defproc[((disjoin [f (-> A ... boolean?)] ...) [x A] ...) boolean?]{

Combines calls to each function with @racket[or].  Equivalent to
@racket[(or (f x ...) ...)]

@defexamples[
#:eval the-eval
(define f (disjoin exact? integer?))
(f 1)
(f 1.0)
(f 1/2)
(f 0.5)
]

}

@(close-eval the-eval)
