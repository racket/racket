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

@defform[(define-planet-package name package)]{

Defines a shortcut @scheme[name] for importing modules from planet package
@scheme[package].  Subsequently, @scheme[(name module)] is equivalent to
@scheme[(planet package/module)] as a require path.  For instance, to import the
@scheme[text] and @scheme[web] modules from this package:

@schemeblock[
(define-planet-package my-package cce/scheme)
(require (my-package web) (my-package text))
]

The above @scheme[require] is equivalent to:

@schemeblock[
(require (planet cce/scheme/web) (planet cce/scheme/text))
]

}

@defform[(define-collection name collect)]{

Defines a shortcut @scheme[name] for importing modules from @scheme[collect] and
its subcollections.  Subsequently, @scheme[(name)] is equivalent to
@scheme[collect] as a require path, and @scheme[(name path)] is equivalent to
@scheme[collect/path].

@schemeblock[
(define-collection macro syntax)
(require (macro parse))
]

The above @scheme[require] is equivalent to the below:

@schemeblock[
(require syntax/parse)
]

}

@defform[
(this-package-in path)
]{

This
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{require transformer}
imports the file at @scheme[path] in the current planet package.  For instance,
in the package @schememodname[(planet cce/scheme:7)], writing:
@schemeblock[(require (this-package-in function))]
... is equivalent to writing:
@schemeblock[(require (planet cce/scheme:7/function))]

}
