#lang scribble/manual
@(require scribble/eval
          "utils.rkt"
          (for-label racket/base
                     racket/runtime-path
                     unstable/lazy-require
                     compiler/cm-accomplice))

@title[#:tag "lazy-require"]{Lazy Require}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/lazy-require]

@defform/subs[#:literals (unquote)
              (lazy-require maybe-requires
                            [mod (imported-fun-id ...)] ...)
              ([mod module-path
                   (unquote module-path-expr)]
               [maybe-requires code:blank
                               (code:line #:requires-for-path (require-spec ...))])
              #:contracts ([module-path-expr module-path?])]{

Defines each @racket[imported-fun-id] as a function that, when called,
dynamically requires the export named @racket['imported-fun-id] from
the module specified by @racket[mod] and calls it with the same
arguments.

The module @racket[mod] can be specified as a @racket[_module-path]
(see @racket[require]) or as an @racket[unquote]-escaped expression
that computes a module path. As with
@racket[define-runtime-module-path-index], a @racket[module-path-expr]
is evaluated both in phase level 0 and phase level 1 relative to its 
enclosing phase level.

If the enclosing relative phase level is not 0, then
@racket[module-path-expr] is also placed in a submodule (with a use of
@racket[define-runtime-module-path-index] at phase level 0 within the
submodule); supply extra @racket[require-spec]s with
@racket[#:requires-for-path] to bind within the submodule for
@racket[module-path-expr]. Introduced submodules have the names
@racket[lazy-require-]@racket[_n]@racketidfont{-}@racket[_m], where
@racket[_n] is a phase-level number and @racket[_m] is a number.

When the use of a lazily-required function triggers module loading,
@racket[register-external-module] declares a potential compilation
dependency (in case the function is used in the process of compiling a
module).}


@defform[(begin-on-demand #:export (fun-id ...)
            body ...+)]{

Defines each @racket[fun-id] as a function that, when called,
dynamically loads and executes the @racket[body] forms. The
@racket[body] forms must contain definitions for each @racket[fun-id],
and the value of each @racket[fun-id] must be a function.

A @racket[body] form may be any module-level form except
@racket[provide]. In particular, @racket[require] forms are allowed.

The @racket[body] forms are placed within a submodule that extends the
scope of the enclosing module (ie, @racket[module*] with @racket[#f]
in the language position). Consequently, any references to sibling
submodules must include a with @racket[".."] module path element.
}
