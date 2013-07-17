#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label racket unstable/define
                     (only-in mzlib/etc define-syntax-set)))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/define (for-syntax racket/base)))

@title{Definitions}
@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defmodule[unstable/define]

Provides macros for creating and manipulating definitions.

@defform[(at-end expr)]{

When used at the top level of a module, evaluates @racket[expr] at the end of
the module.  This can be useful for calling functions before their definitions.

@defexamples[
#:eval the-eval
(module Failure scheme
  (f 5)
  (define (f x) x))
(require 'Failure)
(module Success scheme
  (require unstable/define)
  (at-end (f 5))
  (define (f x) x))
(require 'Success)
]

}

@defform[(in-phase1 e)]{

Executes @racket[e] during phase 1 (the syntax transformation phase)
relative to its context, during pass 1 if it occurs in a head expansion
position.

}

@defform[(in-phase1/pass2 e)]{

Executes @racket[e] during phase 1 (the syntax transformation phase)
relative to its context, during pass 2 (after head expansion).

}

@(close-eval the-eval)
