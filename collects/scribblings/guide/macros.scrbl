#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "macros" #:style 'toc]{Macros}

A @deftech{macro} is a syntactic form with an associated
@deftech{transformer} that @deftech{expands} the original form
into existing forms. To put it another way, a macro is an
extension to the Scheme compiler. Most of the syntactic forms of
@schememodname[scheme/base] and @schememodname[scheme] are
actually macros that expand into a small set of core constructs.

Like many languages, Scheme provides pattern-based macros that
make simple transformations easy to implement and reliable to
use. Scheme also supports arbitrary macro transformers that are
implemented in Scheme---or in a macro-extended variant of Scheme.

@local-table-of-contents[]

@;------------------------------------------------------------------------
@include-section["pattern-macros.scrbl"]
@include-section["proc-macros.scrbl"]

