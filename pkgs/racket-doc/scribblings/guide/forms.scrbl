#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "scheme-forms" #:style 'toc]{Expressions and Definitions}

The @secref["to-scheme"] chapter introduced some of Racket's syntactic
forms: definitions, procedure applications, conditionals, and so
on. This section provides more details on those forms, plus a few
additional basic forms.

@local-table-of-contents[]

@section[#:tag "syntax-notation"]{Notation}

This chapter (and the rest of the documentation) uses a slightly
different notation than the character-based grammars of the
@secref["to-scheme"] chapter. The grammar for a use of a syntactic
form @racketkeywordfont{something} is shown like this:

@specform[(#,(racketkeywordfont "something") [id ...+] an-expr ...)]

The italicized meta-variables in this specification, such as
@racket[_id] and @racket[_an-expr], use the syntax of Racket
identifiers, so @racket[_an-expr] is one meta-variable. A naming
convention implicitly defines the meaning of many meta-variables:

@itemize[

 @item{A meta-variable that ends in @racket[_id] stands for an
       identifier, such as @racketidfont{x} or
       @racketidfont{my-favorite-martian}.}

 @item{A meta-identifier that ends in @racket[_keyword] stands
       for a keyword, such as @racket[#:tag].}

 @item{A meta-identifier that ends with @racket[_expr] stands for any
       sub-form, and it will be parsed as an expression.}

 @item{A meta-identifier that ends with @racket[_body] stands for any
       sub-form; it will be parsed as either a local definition or an
       expression. A @racket[_body] can parse as a definition only if
       it is not preceded by any expression, and the last
       @racket[_body] must be an expression; see also @secref["intdefs"].}

]

Square brackets in the grammar indicate a parenthesized sequence of
forms, where square brackets are normally used (by convention). That
is, square brackets @italic{do not} mean optional parts of the
syntactic form.

A @racketmetafont{...} indicates zero or more repetitions of the
preceding form, and @racketmetafont{...+} indicates one or more
repetitions of the preceding datum. Otherwise, non-italicized
identifiers stand for themselves.

Based on the above grammar, then, here are a few conforming uses of
@racketkeywordfont{something}:

@racketblock[
(#,(racketkeywordfont "something") [x])
(#,(racketkeywordfont "something") [x] (+ 1 2))
(#,(racketkeywordfont "something") [x my-favorite-martian x] (+ 1 2) #f)
]

Some syntactic-form specifications refer to meta-variables that are
not implicitly defined and not previously defined. Such meta-variables
are defined after the main form, using a BNF-like format for
alternatives:

@specform/subs[(#,(racketkeywordfont "something-else") [thing ...+] an-expr ...)
               ([thing thing-id
                       thing-keyword])]

The above example says that, within a @racketkeywordfont{something-else}
form, a @racket[_thing] is either an identifier or a keyword.

@;------------------------------------------------------------------------

@include-section["binding.scrbl"]
@include-section["apply.scrbl"]
@include-section["lambda.scrbl"]
@include-section["define.scrbl"]
@include-section["let.scrbl"]
@include-section["cond.scrbl"]
@include-section["begin.scrbl"]
@include-section["set.scrbl"]
@include-section["quote.scrbl"]
@include-section["qq.scrbl"]
@include-section["case.scrbl"]
@include-section["parameterize.scrbl"]
