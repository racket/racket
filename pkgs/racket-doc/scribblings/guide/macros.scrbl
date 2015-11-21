#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "macros" #:style 'toc]{Macros}

A @deftech{macro} is a syntactic form with an associated
@deftech{transformer} that @deftech{expands} the original form
into existing forms. To put it another way, a macro is an
extension to the Racket compiler. Most of the syntactic forms of
@racketmodname[racket/base] and @racketmodname[racket] are
actually macros that expand into a small set of core constructs.

Like many languages, Racket provides pattern-based macros that
make simple transformations easy to implement and reliable to
use. Racket also supports arbitrary macro transformers that are
implemented in Racket---or in a macro-extended variant of Racket.

(For a bottom-up introduction of Racket macro, you may refer to: @(hyperlink "http://www.greghendershott.com/fear-of-macros/" "Fear of Macros"))

@local-table-of-contents[]

@;------------------------------------------------------------------------
@include-section["pattern-macros.scrbl"]
@include-section["proc-macros.scrbl"]

