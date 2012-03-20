#lang scribble/manual
@(require scribble/eval
          "utils.rkt"
          (for-label racket/base
                     racket/runtime-path
                     unstable/lazy-require))

@title[#:tag "lazy-require"]{Lazy Require}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/lazy-require]

@defform/subs[#:literals (unquote)
              (lazy-require [mod (imported-fun-id ...)] ...)
              ([mod module-path
                   (unquote module-path-expr)])
              #:contracts ([module-path-expr module-path?])]{

Defines each @racket[imported-fun-id] as a function that, when called,
dynamically requires the export named @racket['imported-fun-id] from
the module specified by @racket[mod] and calls it with the same
arguments.

The module @racket[mod] can be specified as a @racket[_module-path]
(see @racket[require]) or as an @racket[unquote]-escaped expression
that computes a module path. As with
@racket[define-runtime-module-path-index], a @racket[module-path-expr]
is evaluated both in phase 0 and phase 1.
}

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
