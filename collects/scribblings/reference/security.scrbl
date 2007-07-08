#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:style 'toc]{Security and Reflection}

MzScheme offers several mechanisms for managing security, each of
which relies on @tech{thread}- and @tech{continuation}-specific
@tech{parameters} (see @secref["mz:parameters"]).

@local-table-of-contents[]

@;------------------------------------------------------------------------
@include-section["security-guards.scrbl"]
@include-section["custodians.scrbl"]
@include-section["thread-groups.scrbl"]
@include-section["struct-inspectors.scrbl"]
@include-section["code-inspectors.scrbl"]
@include-section["namespaces.scrbl"]
