#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "scheme-forms" #:style 'toc]{Expressions and Definitions}

The @secref["to-scheme"] chapter introduced some of Scheme's syntactic
forms: definitions, procedure applications, conditionals, procedures,
local binding, and some iteration forms. This section provides a more
complete coverage of the basic Scheme syntactic forms.

@local-table-of-contents[]

@include-section["binding.scrbl"]
@include-section["apply.scrbl"]
@include-section["lambda.scrbl"]
@include-section["define.scrbl"]
@include-section["for.scrbl"]

