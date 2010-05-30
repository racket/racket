#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/bnf
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme/base scribble/manual unstable/cce/planet))

@title[#:style 'quiet #:tag "cce-planet"]{@|PLaneT| Packages}

@defmodule[unstable/cce/planet]

This module provides tools relating to @|PLaneT| packages.  In addition to the
binding described below, it provides @scheme[define-planet-package] and
@scheme[this-package-in] from @schememodname[unstable/cce/require-provide], and
@scheme[make-planet-path], @scheme[syntax-source-planet-package],
@scheme[syntax-source-planet-package-owner],
@scheme[syntax-source-planet-package-name],
@scheme[syntax-source-planet-package-major],
@scheme[syntax-source-planet-package-minor], and
@scheme[syntax-source-planet-package-symbol] from
@schememodname[unstable/planet-syntax].

@defform*[[
(this-package-version-symbol)
(this-package-version-symbol path)
]]{

Produces a symbol corresponding to a @scheme[planet] module path for the current
planet package, possibly with a @nonterm{path} (from the grammar of
@scheme[planet] module specs) into the package.  This is similar to
@scheme[this-package-version] and similar tools from
@schememodname[planet/util].

}
