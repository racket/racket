#reader(lib "docreader.ss" "scribble")
@require[(lib "bnf.ss" "scribble")]
@require["mz.ss"]

@title[#:style 'toc]{Security}

MzScheme offers several mechanisms for managing security, each of
which relies on @tech{thread}- and @tech{continuation}-specific
@tech{parameters} (see @secref["mz:parameters"]).

@local-table-of-contents[]

@;------------------------------------------------------------------------
@include-section["custodians.scrbl"]
