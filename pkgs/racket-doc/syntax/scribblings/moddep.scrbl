#lang scribble/doc
@(require "common.rkt" (for-label syntax/moddep))

@title[#:tag "moddep"]{Inspecting Modules and Module Dependencies}

@defmodule[syntax/moddep]

Re-exports @racketmodname[syntax/modread],
@racketmodname[syntax/modcode], @racketmodname[syntax/modcollapse],
and @racketmodname[syntax/modresolve], in addition to the following:

@defproc[(show-import-tree [module-path-v module-path?]
                           [#:dag? dag? any/c #f]
                           [#:path-to path-to-module-path-v (or/c #f module-path?) #f]
                           [#:show show
                                   (string? any/c string? (or/c #f exact-integer?) . -> . any)
                                   (lambda (indent path require-mode phase)
                                     (printf "~a~a~a ~a\n" indent path require-mode phase))])
         void?]{

A debugging aid that prints (by default) the import hierarchy starting
from a given module path. Supply an alternate @racket[show] function
to handle each path instead of having it printed; the second argument
is a result of @racket[resolved-module-path-name].

If @racket[dag?] is true, then a module is passed to @racket[show]
only the first time is encountered in the hierarchy at a given phase.

If @racket[path-to-module-path-v] is a module path, then only the
spines of the tree that reach @racket[path-to-module-path-v] are
shown.

@history[#:changed "6.12.0.4" @elem{Added the @racket[#:dag?] and
                                    @racket[#:path-to] arguments.}
         #:changed "7.0.0.10" @elem{Added the @racket[#:show] argument.}]}
