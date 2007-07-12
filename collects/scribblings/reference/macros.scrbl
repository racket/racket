#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:style 'toc]{Macros}

See @secref["mz:syntax-model"] for general information on how programs
are parsed. In particular, the subsection @secref["mz:expand-steps"]
describes how parsing triggers macros, and
@secref["mz:transformer-model"] describes how macro transformers are
called.

@local-table-of-contents[]

@include-section["stx-patterns.scrbl"]
@include-section["stx-ops.scrbl"]
@include-section["stx-comp.scrbl"]
@include-section["stx-trans.scrbl"]
@include-section["stx-props.scrbl"]
@include-section["stx-certs.scrbl"]
@include-section["stx-expand.scrbl"]
