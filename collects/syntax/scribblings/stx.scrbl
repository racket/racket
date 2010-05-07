#lang scribble/doc
@(require "common.ss"
          (for-label syntax/stx))

@title[#:tag "stx"]{Deconstructing Syntax Objects}

@defmodule[syntax/stx]

@defproc[(stx-null? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is either the empty list or a
@tech[#:doc refman]{syntax object} representing the empty list (i.e.,
@scheme[syntax-e] on the @tech[#:doc refman]{syntax object} returns
the empty list).}

@defproc[(stx-pair? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is either a pair or a syntax object
representing a pair (see @techlink[#:doc refman]{syntax pair}).}

@defproc[(stx-list? [v any/c]) boolean?]{ 

Returns @scheme[#t] if @scheme[v] is a list, or if it is a sequence of
pairs leading to a syntax object such that @scheme[syntax->list] would
produce a list.}

@defproc[(stx->list [stx-list stx-list?]) list?]{

Produces a list by flatting out a trailing syntax object using
@scheme[syntax->list].}

@defproc[(stx-car [v stx-pair?]) any]{

Takes the car of a @techlink[#:doc refman]{syntax pair}.}

@defproc[(stx-cdr [v stx-pair?]) any]{

Takes the cdr of a @techlink[#:doc refman]{syntax pair}.}

@defproc[(module-or-top-identifier=? [a-id identifier?]
				     [b-id identifier?])
	 boolean?]{

Returns @scheme[#t] if @scheme[a-id] and @scheme[b-id] are
@scheme[free-identifier=?], or if @scheme[a-id] and @scheme[b-id] have
the same name (as extracted by @scheme[syntax-e]) and @scheme[a-id]
has no binding other than at the top level.

This procedure is useful in conjunction with @scheme[syntax-case*] to
match procedure names that are normally bound by Racket. For
example, the @scheme[include] macro uses this procedure to recognize
@scheme[build-path]; using @scheme[free-identifier=?]  would not work
well outside of @scheme[module], since the top-level
@scheme[build-path] is a distinct variable from the @schememodname[racket/base] export
(though it's bound to the same procedure, initially).}
