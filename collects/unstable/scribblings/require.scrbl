#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/require))

@title{Requiring Modules}

@defmodule[unstable/require]

This module provides tools for importing from modules.

@defform[(require/provide module-path ...)]{

Re-exports all bindings provided by each @scheme[module-path].  Equivalent to:

@schemeblock[
(require module-path ...)
(provide (all-from-out module-path ...))
]

}

@defform[(quote-require require-spec ...)]{

Produces the names exported by the @scheme[require-spec]s as a list of symbols.

@examples[
#:eval (eval/require 'unstable/require)
(quote-require racket/bool racket/function)
]

}
