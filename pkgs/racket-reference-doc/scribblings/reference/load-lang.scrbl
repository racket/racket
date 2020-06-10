#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "load-lang"]{The @racketmodname[racket/load] Language}

@defmodulelang[racket/load]

The @racketmodname[racket/load] language supports evaluation where
each top-level form in the module body is separately passed to
@racket[eval] in the same way as for @racket[load].

The namespace for evaluation shares the @tech{module registry} with
the @racketmodname[racket/load] module instance, but it has a separate
top-level environment, and it is initialized with the bindings of
@racketmodname[racket]. A single namespace is created for each
instance of the @racketmodname[racket/load] module (i.e., multiple
modules using the @racketmodname[racket/load] language share a
namespace). The @racket[racket/load] library exports only
@racketidfont{#%module-begin} and @racketidfont{#%top-interaction}
forms that effectively swap in the evaluation namespace and call
@racket[eval].

For example, the body of a module using @racket[racket/load] can
include @racket[module] forms, so that running the following module
prints @racketresultfont{5}:

@racketmod[
racket/load

(module m racket/base
  (provide x)
  (define x 5))

(module n racket/base
  (require 'm)
  (display x))

(require 'n)
]

Definitions in a module using @racket[racket/load] are evaluated in
the current namespace, which means that @racket[load] and
@racket[eval] can see the definitions. For example, running the
following module prints @racketresultfont{6}:

@racketmod[
racket/load

(define x 6)
(display (eval 'x))
]

Since all forms within a @racketmodname[racket/load] module are
evaluated in the top level, bindings cannot be exported from the
module using @racket[provide]. Similarly, since evaluation of the
module-body forms is inherently dynamic, compilation of the module
provides essentially no benefit. For these reasons, use
@racketmodname[racket/load] for interactive exploration of top-level
forms only, and not for constructing larger programs.
