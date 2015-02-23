#lang scribble/doc
@(require "common.rkt" (for-label syntax/wrap-modbeg))

@title[#:tag "wrap-modbeg"]{Wrapping Module-Body Expressions}

@defmodule[syntax/wrap-modbeg]

@history[#:added "6.0.0.1"]

@defproc[(make-wrapping-module-begin [wrap-form syntax?]
                                     [module-begin-form syntax? #'#%plain-module-begin])
         (syntax? . -> . syntax?)]{

Provided @racket[for-syntax].

Constructs a function that is suitable for use as a
@racket[#%module-begin] replacement, particularly to replace the
facet of @racket[#%module-begin] that wraps each top-level
expression to print the expression's result(s).

The function takes a syntax object and returns a syntax object using
@racket[module-begin-form].  Assuming that @racket[module-begin-form]
resembles @racket[#%plain-module-begin], each top-level expression
@racket[_expr] will be wrapped as @racket[(wrap-form _expr)], while
top-level declarations (such as @racket[define-values] and
@racket[require] forms) are left as-is. Expressions are detected after
macro expansion and @racket[begin] splicing, and expansion is
interleaved with declaration processing as usual.}
