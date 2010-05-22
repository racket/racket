#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/require-provide))

@title[#:style 'quiet #:tag "cce-require-provide"]{Require and Provide}

@defmodule[unstable/cce/require-provide]

This module provides tools for managing the imports and exports of modules.

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
#:eval (evaluator 'unstable/cce/require-provide)
(quote-require scheme/bool scheme/function)
]

}

@defform[(local-require require-spec ...)]{

This form performs a require into a local definition context.  It achieves this
by lifting a @scheme[#%require] form to the top level and introducing the
bindings locally with rename transformers.  For many purposes this is the same
as a regular @scheme[require]; however, only bindings for the current phase are
made available, and all names are introduced as syntax bindings even if the
exported identifiers included value bindings.

}

@defform[(do-local-require rename require-spec ...)]{

This form generalizes @scheme[do-local-require] to use an arbitrary macro
@scheme[rename] (of the same syntactic form as @scheme[define-renamings]) to
introduce local bindings.

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
