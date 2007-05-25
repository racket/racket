#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "to-scheme" #:style 'toc]{A Little Scheme}

This chapter provides a quick introduction to Scheme as background for
the rest of the guide. Readers with some Scheme experience can safely
skip to @secref["datatypes"].

@local-table-of-contents[]

@include-section["simple-data.scrbl"]
@include-section["syntax.scrbl"]
@include-section["lists.scrbl"]
@include-section["truth.scrbl"]
