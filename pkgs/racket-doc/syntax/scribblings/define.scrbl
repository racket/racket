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

If @racket[opt+kws?] is @racket[#t], then arguments of the form
@racket[[id expr]], @racket[keyword id], and @racket[keyword [id
expr]] are allowed, and they are preserved in the expansion.}

@defproc[(normalize-definition/mk-rhs [defn-stx syntax?]
                                      [lambda-id-stx identifier?]
                                      [check-context? boolean?]
                                      [opt+kws? boolean?]
                                      [err-no-body? boolean?])
         (values identifier? (-> syntax? syntax?) syntax?)]{

  The helper for @racket[normalize-definition] that produces three values:
  the defined identifier, a function that takes the syntax of the body
  and produces syntax that has the expected binding structure, and
  finally the right-hand side expression that @racket[normalize-definition]
  gives to the previous function.

  If @racket[err-no-body?] is true, then there must be a right-hand side
  expression or else it is a syntax error. The @racket[err-no-body?] argument
  is true for uses of @racket[normalize-definition].

  @history[#:added "6.1.1.8"]
}
