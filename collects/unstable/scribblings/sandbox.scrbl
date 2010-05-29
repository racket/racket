#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label racket racket/sandbox unstable/sandbox))

@title{Sandboxed Evaluation}

@defmodule[unstable/sandbox]

This module provides tools for sandboxed evaluation.

@deftogether[(
@defproc[(make-trusted-evaluator
          [language (or/c module-path?
                          (list/c 'special symbol?)
                          (cons/c 'begin list?))]
          [input-program any/c] ...
          [#:requires requires (listof (or/c module-path? path?))]
          [#:allow-read allow (listof or/c module-path? path?)])
         (any/c . -> . any)]
@defproc[(make-trusted-module-evaluator
          [module-decl (or/c syntax? pair?)]
          [#:language lang (or/c #f module-path?)]
          [#:allow-read allow (listof (or/c module-path? path?))])
         (any/c . -> . any)]
)]{
These procedures wrap calls to @scheme[make-evaluator] and
@scheme[make-module-evaluator], respectively, with
@scheme[call-with-trusted-sandbox-configuration].
}

@deftogether[(
@defproc[(make-scribble-evaluator
          [language (or/c module-path?
                          (list/c 'special symbol?)
                          (cons/c 'begin list?))]
          [input-program any/c] ...
          [#:requires requires (listof (or/c module-path? path?))]
          [#:allow-read allow (listof or/c module-path? path?)])
         (any/c . -> . any)]
@defproc[(make-scribble-module-evaluator
          [module-decl (or/c syntax? pair?)]
          [#:language lang (or/c #f module-path?)]
          [#:allow-read allow (listof (or/c module-path? path?))])
         (any/c . -> . any)]
)]{
These procedures wrap calls to @scheme[make-trusted-evaluator] and
@scheme[make-trusted-module-evaluator], respectively, with parameterizations
setting @scheme[sandbox-output] and @scheme[sandbox-error-output] to
@scheme['string].
}

@defproc[(make-sandbox-namespace-specs [make-ns (-> namespace?)]
                                       [path module-path?] ...)
         (cons/c (-> namespace?) (listof module-path?))]{

This function produces a value for the parameter
@scheme[sandbox-namespace-specs] such that new sandbox evaluators start with a
namespace constructed by @scheme[make-ns] and share a set of instances of the
modules referred to by the given @scheme[path]s.

}
