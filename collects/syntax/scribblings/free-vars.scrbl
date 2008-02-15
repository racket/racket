#lang scribble/doc
@(require "common.ss"
          (for-label syntax/free-vars))

@title[#:tag "free-vars"]{Computing the Free Variables of an Expression}

@defmodule[syntax/free-vars]

@defproc[(free-vars [expr-stx syntax?]) (listof identifier?)]{

Returns a list of free @scheme[lambda]- and @scheme[let]-bound
identifiers in @scheme[expr-stx]. The expression must be fully
expanded (@secref[#:doc refman "fully-expanded"]).}
