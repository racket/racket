#lang scribble/doc
@(require "common.rkt" (for-label syntax/define))


@title[#:tag "define"]{Parsing @racket[define]-like Forms}

@defmodule[syntax/define]

@defproc[(normalize-definition [defn-stx syntax?]
                               [lambda-id-stx identifier?]

                               [check-context? boolean? #t]
                               [opt+kws? boolean? #f])
         (values identifier? syntax?)]{

Takes a definition form whose shape is like @racket[define] (though
possibly with a different name) and returns two values: the defined
identifier and the right-hand side expression.

To generate the right-hand side, this function may need to insert uses
of @racket[lambda]. The @racket[lambda-id-stx] argument provides a
suitable @racket[lambda] identifier.

If the definition is ill-formed, a syntax error is raised. If
@racket[check-context?] is true, then a syntax error is raised if
@racket[(syntax-local-context)] indicates that the current context is
an expression context. The default value of @racket[check-context?] is
@racket[#t].

If @racket[opt-kws?] is @racket[#t], then arguments of the form
@racket[[id expr]], @racket[keyword id], and @racket[keyword [id
expr]] are allowed, and they are preserved in the expansion.}
