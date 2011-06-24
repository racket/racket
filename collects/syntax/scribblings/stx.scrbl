#lang scribble/doc
@(require "common.rkt" (for-label syntax/stx))

@title[#:tag "stx"]{Deconstructing Syntax Objects}

@defmodule[syntax/stx]

@defproc[(stx-null? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is either the empty list or a
@tech[#:doc refman]{syntax object} representing the empty list (i.e.,
@racket[syntax-e] on the @tech[#:doc refman]{syntax object} returns
the empty list).}

@defproc[(stx-pair? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is either a pair or a syntax object
representing a pair (see @techlink[#:doc refman]{syntax pair}).}

@defproc[(stx-list? [v any/c]) boolean?]{ 

Returns @racket[#t] if @racket[v] is a list, or if it is a sequence of
pairs leading to a syntax object such that @racket[syntax->list] would
produce a list.}

@defproc[(stx->list [stx-list stx-list?]) (or/c list? #f)]{

Produces a list by flatting out a trailing syntax object using
@racket[syntax->list].}

@defproc[(stx-car [v stx-pair?]) any]{

Takes the car of a @techlink[#:doc refman]{syntax pair}.}

@defproc[(stx-cdr [v stx-pair?]) any]{

Takes the cdr of a @techlink[#:doc refman]{syntax pair}.}

@defproc[(stx-map [proc procedure?]
                  [stxl stx-list?] ...)
         list?]{

Equivalent to @racket[(map proc (stx->list stxl) ...)].
}

@defproc[(module-or-top-identifier=? [a-id identifier?]
				     [b-id identifier?])
	 boolean?]{

Returns @racket[#t] if @racket[a-id] and @racket[b-id] are
@racket[free-identifier=?], or if @racket[a-id] and @racket[b-id] have
the same name (as extracted by @racket[syntax-e]) and @racket[a-id]
has no binding other than at the top level.

This procedure is useful in conjunction with @racket[syntax-case*] to
match procedure names that are normally bound by Racket. For
example, the @racket[include] macro uses this procedure to recognize
@racket[build-path]; using @racket[free-identifier=?]  would not work
well outside of @racket[module], since the top-level
@racket[build-path] is a distinct variable from the @racketmodname[racket/base] export
(though it's bound to the same procedure, initially).}
