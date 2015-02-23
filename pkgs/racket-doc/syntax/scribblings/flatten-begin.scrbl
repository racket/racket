#lang scribble/doc
@(require "common.rkt"
          scribble/eval
          (for-label syntax/flatten-begin))

@(define flatten-eval (make-base-eval))
@(flatten-eval '(require syntax/flatten-begin))

@title[#:tag "flatten-begin"]{Flattening @racket[begin] Forms}

@defmodule[syntax/flatten-begin]

@defproc[(flatten-begin [stx syntax?]) (listof syntax?)]{

Extracts the sub-expressions from a @racket[begin]-like form,
reporting an error if @racket[stx] does not have the right shape
(i.e., a syntax list). The resulting syntax objects have annotations
transferred from @racket[stx] using @racket[syntax-track-origin].

@examples[#:eval flatten-eval
  (flatten-begin #'(begin 1 2 3))
  (flatten-begin #'(begin (begin 1 2) 3))
  (flatten-begin #'(+ (- 1 2) 3))
]}

@defproc[(flatten-all-begins [stx syntax?]) (listof syntax?)]{

Extracts the sub-expressions from a @racket[begin] form and
recursively flattens @racket[begin] forms nested in the original one.
An error will be reported if @racket[stx] is not a @racket[begin]
form. The resulting syntax objects have annotations
transferred from @racket[stx] using @racket[syntax-track-origin].

@examples[#:eval flatten-eval
  (flatten-all-begins #'(begin 1 2 3))
  (flatten-all-begins #'(begin (begin 1 2) 3))
]

@history[#:added "6.1.0.3"]}

@close-eval[flatten-eval]
