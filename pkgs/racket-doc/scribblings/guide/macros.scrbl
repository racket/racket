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

This chapter provides an introduction to Racket macros, but see
@hyperlink["http://www.greghendershott.com/fear-of-macros/"]{@italic{Fear of
Macros}} for an introduction from a different perspective.

Racket includes additional support for macro development:
A @hyperlink["https://docs.racket-lang.org/macro-debugger/index.html"]{@italic{macro
debugger}} to make it easier for experienced programmers
to debug their macros and for novices to study their behavior,
and of macros. And the @hyperlink["https://docs.racket-lang.org/syntax/index.html"]{@italic{syntax/parse
library}} for writing macros and specifying syntax that
automatically validates macro uses and reports syntax
errors.

@local-table-of-contents[]

@;------------------------------------------------------------------------
@include-section["pattern-macros.scrbl"]
@include-section["proc-macros.scrbl"]
@include-section["macro-module.scrbl"]


