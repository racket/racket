#lang scribble/doc
@(require "common.rkt" (for-label syntax/moddep))

@title[#:tag "moddep"]{Inspecting Modules and Module Dependencies}

@defmodule[syntax/moddep]

Re-exports @racketmodname[syntax/modread],
@racketmodname[syntax/modcode], @racketmodname[syntax/modcollapse],
and @racketmodname[syntax/modresolve], in addition to the following:

@defproc[(show-import-tree [module-path-v module-path?]) void?]{

A debugging aid that prints the import hierarchy starting from a given
module path.}
