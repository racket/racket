#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base racket/promise racket/list
                     math plot
                     (only-in typed/racket/base
                              Flonum Real Boolean Any Listof Integer case-> -> U
                              Sequenceof Positive-Flonum Nonnegative-Flonum))
          "utils.rkt")

@(define untyped-eval (make-untyped-math-eval))
@interaction-eval[#:eval untyped-eval (require racket/list)]

@title[#:tag "stats"]{Statistical Functions}
@(author-neil)

@defmodule[math/statistics]

xxx intro

something about accepting weighted samples whenever it makes sense
(time it doesn't make sense: autocorrelation)

@local-table-of-contents[]

@section{Counting}

@defthing[samples->hash Any]{
}

@defthing[count-samples Any]{
}

@section{Expected Values}

@section{Running Expected Values}

@section{Correlation}

@section{Order Statistics}

@(close-eval untyped-eval)
