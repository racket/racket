#lang scribble/doc
@(require "mz.ss")

@title[#:tag "load-lang"]{The @schememodname[racket/load] Language}

@defmodulelang[racket/load]

The @schememodname[racket/load] language supports traditional Scheme
evaluation, where each top-level form in the module body is separately
passed to @scheme[eval] in the same way as for @scheme[load].

The namespace for evaluation shares the @tech{module registry} with
the @schememodname[racket/load] module instance, but it has a separate
top-level environment, and it is initialized with the bindings of
@schememodname[scheme]. A single namespace is created for each
instance of the @schememodname[racket/load] module (i.e., multiple
modules using the @schememodname[racket/load] language share a
namespace). The @scheme[racket/load] library exports only
@schemeidfont{#%module-begin} and @schemeidfont{#%top-interaction}
forms that effectively swap in the evaluation namespace and call
@scheme[eval].

For example, the body of a module using @scheme[racket/load] can
include @scheme[module] forms, so that running the following module
prints @schemeresultfont{5}:

@schememod[
racket/load

(module m racket/base
  (provide x)
  (define x 5))

(module n racket/base
  (require 'm)
  (display x))

(require 'n)
]

Definitions in a module using @scheme[racket/load] are evaluated in
the current namespace, which means that @scheme[load] and
@scheme[eval] can see the definitions. For example, running the
following module prints @schemeresultfont{6}:

@schememod[
racket/load

(define x 6)
(display (eval 'x))
]

Since all forms within a @schememodname[racket/load] module are
evaluated in the top level, bindings cannot be exported from the
module using @scheme[provide]. Similarly, since evaluation of the
module-body forms is inherently dynamic, compilation of the module
provides essentially no benefit. For these reasons, use
@schememodname[racket/load] for interactive exploration of top-level
forms only, and not for constructing larger programs.
