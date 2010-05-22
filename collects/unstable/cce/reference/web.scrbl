#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme xml unstable/cce/web))

@title[#:style 'quiet #:tag "cce-web"]{XML and CSS}

@defmodule[unstable/cce/web]

This module provides tools for programmatic creation of static web pages.  It is
based on the XML collection; see documentation for @scheme[xexpr?].

@deftogether[(
@defthing[css/c flat-contract?]
@defproc[(css? [v any/c]) boolean?]
)]{
This contract and predicate pair recognizes CSS-expressions, which are
described by the following grammar:

@schemegrammar*[
#:literals (cons list)
[css (list style ...)]
[style-def (cons selector (list property ...))]
[property (list name value)]
[selector text]
[name text]
[value text]
]

Here, @scheme[text] is any of the datatypes described in @secref["cce-text"].
}

@defthing[xexpr/c flat-contract?]{
This flat contract corresponds to @scheme[xexpr?].  It is reprovided from
@schememodname[xml].  In versions of PLT Scheme before the implementation of
@scheme[xexpr/c], this module provides its own definition.
}

@deftogether[(
@defproc[(write-css [css css/c] [out output-port? (current-output-port)])
         void?]
@defproc[(write-xexpr [css css/c] [out output-port? (current-output-port)])
         void?]
)]{
These functions write CSS-expressions and
X-expressions, respectively, to output ports, by their
canonical text representations.
}

@deftogether[(
@defproc[(create-stylesheet [file path-string?] [css css/c]) void?]
@defproc[(create-webpage [file path-string?] [xexpr xexpr/c]) void?]
)]{
These functions write style sheets (represented as CSS-expressions) or
webpages (represented as X-expressions) to files.
}
