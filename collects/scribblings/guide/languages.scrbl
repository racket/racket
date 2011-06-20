#lang scribble/doc
@(require scribble/manual "guide-utils.rkt")

@title[#:tag "languages" #:style 'toc]{Creating Languages}

The @tech{macro} facilities defined in the preceding chapter let a
programmer define syntactic extensions to a language, but a macro is
limited in two ways:

@itemlist[

 @item{a macro cannot restrict the syntax available in its context or
       change the meaning of surrounding forms; and}

 @item{a macro can extend the syntax of a language only within the
       parameters of the language's lexical conventions, such as using
       parentheses to group the macro name with its subforms and using
       the core syntax of identifiers, keywords, and literals.}

]

@guideother{The distinction between the @tech{reader} and
@tech{expander} layer is introduced in @secref["lists-and-syntax"].}

That is, a macro can only extend a language, and it can do so only at
the @tech{expander} layer. Racket offers additional facilities for
defining a starting point of the @tech{expander} layer, for extending
the @tech{reader} layer, for defining the starting point of the
@tech{reader} layer, and for packaging a @tech{reader} and
@tech{expander} starting point into a conveniently named language.

@local-table-of-contents[]

@;------------------------------------------------------------------------
@include-section["module-languages.scrbl"]
@include-section["reader-extension.scrbl"]
@include-section["hash-languages.scrbl"]
