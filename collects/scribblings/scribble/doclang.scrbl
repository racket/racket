#lang scribble/manual
@(require "utils.ss")

@title[#:tag "doclang"]{Document Language}

@defmodulelang[scribble/doclang]{The @schememodname[scribble/doclang]
language provides everything from @scheme[scheme/base], except that it
replaces the @scheme[#%module-begin] form.}

The @schememodname[scribble/doclang] @scheme[#%module-begin]
essentially packages the body of the module into a call to
@scheme[decode], binds the result to @scheme[doc], and exports
@scheme[doc].

Any module-level form other than an expression (e.g., a
@scheme[require] or @scheme[define]) remains at the top level, and
the @scheme[doc] binding is put at the end of the module. As usual, a
module-top-level @scheme[begin] slices into the module top level.
