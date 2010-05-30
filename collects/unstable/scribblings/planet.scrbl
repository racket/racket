#lang scribble/manual
@(require scribble/eval
          scribble/bnf
          "utils.rkt"
          (for-label racket/base scribble/manual unstable/planet planet/util))

@title[#:style 'quiet #:tag "cce-planet"]{@|PLaneT| Packages}

@defmodule[unstable/planet]

This module provides tools relating to @|PLaneT| packages.  In addition to the
binding described below, it provides @scheme[define-planet-package] and
@scheme[this-package-in] from @schememodname[unstable/require], and
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
