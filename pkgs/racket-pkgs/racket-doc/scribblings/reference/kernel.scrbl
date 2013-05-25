#lang scribble/manual
@(require (for-label (except-in racket/base
                                lambda λ #%app #%module-begin)
                     (only-in racket/base
                              [#%plain-lambda lambda]
                              [#%plain-lambda λ]
                              [#%plain-app #%app]
                              [#%plain-module-begin #%module-begin])))

@title{Kernel Forms and Functions}

@defmodulelang[racket/kernel]{The @racketmodname[racket/kernel] library
is a @tech{cross-phase persistent} module that provides a minimal set of syntactic
forms and functions.}

``Minimal'' means that @racketmodname[racket/kernel] includes only
forms that are built into the Racket compiler and only functions that
are built into the run-time system. Currently, the set of bindings is
not especially small, nor is it particularly well-defined, since the
set of built-in functions can change frequently. Use
@racketmodname[racket/kernel] with care, and beware that its use can
create compatibility problems.

The @racket[racket/kernel] module exports all of the bindings in the
grammar of fully expanded programs (see @secref["fully-expanded"]),
but it provides @racket[#%plain-lambda] as @racket[lambda] and @racket[λ],
@racket[#%plain-app] as @racket[#%app], and
@racket[#%plain-module-begin] as @racket[#%module-begin]. Aside from
@racket[#%datum] (which expands to @racket[quote]),
@racketmodname[racket/kernel] provides no other syntactic bindings.

The @racket[racket/kernel] module also exports many of the function
bindings from @racketmodname[racket/base], and it exports a few other
functions that are not exported by @racketmodname[racket/base] because
@racketmodname[racket/base] exports improved variants. The exact set
of function bindings exported by @racket[racket/kernel] is unspecified
and subject to change across versions.


@section[#:style '(hidden toc-hidden)]{}

@defmodule[racket/kernel/init]{The @racketmodname[racket/kernel/init]
library re-provides all of @racketmodname[racket/kernel]. It also
provides @racket[#%top-interaction], which makes
@racketmodname[racket/kernel/init] useful with the @Flag{I}
command-line flag for @exec{racket}.}
