#lang scribble/doc
@(require "common.rkt" (for-label syntax/moddep))

@title[#:tag "moddep"]{Inspecting Modules and Module Dependencies}

@defmodule[syntax/moddep]

Re-exports @racketmodname[syntax/modread],
@racketmodname[syntax/modcode], @racketmodname[syntax/modcollapse],
and @racketmodname[syntax/modresolve], in addition to the following:

@defproc[(show-import-tree [module-path-v module-path?]
                           [#:dag? dag? any/c #f]
                           [#:path-to path-to-module-path-v (or/c #f module-path?) #f])
         void?]{

A debugging aid that prints the import hierarchy starting from a given
module path.

If @racket[dag?] is true, then a module is printed only the first time
is encountered in the hierarchy.

If @racket[path-to-module-path-v] is a module path, then only the
spines of the tree that reach @racket[path-to-module-path-v] are
shown.

@history[#:changed "6.12.0.4" @elem{Added the @racket[#:dag?] and
                                    @racket[#:path-to] arguments.}]}
