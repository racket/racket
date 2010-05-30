#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/planet-syntax))

@title{Planet Package Macros}

@defmodule[unstable/planet-syntax]

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@deftogether[(
@defproc[(syntax-source-planet-package [stx syntax?])
         (or/c (list/c string?
                       string?
                       exact-nonnegative-integer?
                       exact-nonnegative-integer?)
                #f)]
@defproc[(syntax-source-planet-package-owner [stx syntax?]) (or/c string? #f)]
@defproc[(syntax-source-planet-package-name [stx syntax?]) (or/c string? #f)]
@defproc[(syntax-source-planet-package-major [stx syntax?])
         (or/c exact-nonnegative-integer? #f)]
@defproc[(syntax-source-planet-package-minor [stx syntax?])
         (or/c exact-nonnegative-integer? #f)]
@defproc[(syntax-source-planet-package-symbol
          [stx syntax?]
          [text (or/c text? #f) #f])
         (or/c symbol? #f)]
)]{

These functions extract the planet package with which @scheme[stx] is
associated, if any, based on its source location information and the currently
installed set of planet packages.  They produce, respectively, the planet
package s-expression, its owner, name, major version number, minor version
number, or a symbol corresponding to a @scheme[planet] module path.  They each
produce @scheme[#f] if @scheme[stx] is not associated with a planet package.

@defexamples[
#:eval (eval/require 'unstable/planet-syntax)
(define loc
  (list (build-path (current-directory) "file.ss")
        #f #f #f #f))
(define stx (datum->syntax #f 'stx loc))
(syntax-source-planet-package stx)
(syntax-source-planet-package-owner stx)
(syntax-source-planet-package-name stx)
(syntax-source-planet-package-major stx)
(syntax-source-planet-package-minor stx)
(syntax-source-planet-package-symbol stx)
(syntax-source-planet-package-symbol stx "there")
]

}

@defproc[(make-planet-path [stx syntax?] [id (or/c identifier? #f)]) syntax?]{

Constructs a syntax object representing a require spec for the planet package
from which @scheme[stx] arises, with suffix @scheme[id] (if any).

@defexamples[
#:eval (eval/require 'unstable/planet-syntax)
(define loc
  (list (build-path (current-directory) "file.ss")
        #f #f #f #f))
(define stx (datum->syntax #f 'stx loc))
(make-planet-path stx #f)
(make-planet-path stx #'there)
]

}
