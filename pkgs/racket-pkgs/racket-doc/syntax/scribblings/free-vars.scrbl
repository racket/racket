#lang scribble/doc
@(require "common.rkt" (for-label syntax/free-vars))

@title[#:tag "free-vars"]{Computing the Free Variables of an Expression}

@defmodule[syntax/free-vars]

@defproc[(free-vars [expr-stx syntax?] [insp inspector? _mod-decl-insp]) 
         (listof identifier?)]{

Returns a list of free @racket[lambda]- and @racket[let]-bound
identifiers in @racket[expr-stx] in the order in which each
identifier first appears within @racket[expr-stx]. The expression must be fully
expanded (see @secref[#:doc refman "fully-expanded"] and
@racket[expand]).

The inspector @racket[insp] is used to disarm @racket[expr-stx] and
sub-expressions before extracting idenrifiers. The default
@racket[insp] is the declaration-time inspector of the
@racketmodname[syntax/free-vars] module.}
