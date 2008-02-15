#lang scribble/doc
@(require "common.ss"
          (for-label syntax/define))


@title[#:tag "define"]{Parsing @scheme[define]-like Forms}

@defmodule[syntax/define]

@defproc[(normalize-definition [defn-stx syntax?]
                               [lambda-id-stx identifier?]
			       
                               [check-context? boolean? #t]
			       [opt+kws? boolean? #t])
	 (values identifier? syntax?)]{

Takes a definition form whose shape is like @scheme[define] (though
possibly with a different name) and returns two values: the defined
identifier and the right-hand side expression.

To generate the right-hand side, this function may need to insert uses
of @scheme[lambda]. The @scheme[lambda-id-stx] argument provides a
suitable @scheme[lambda] identifier.

If the definition is ill-formed, a syntax error is raised. If
@scheme[check-context?] is true, then a syntax error is raised if
@scheme[(syntax-local-context)] indicates that the current context is
an expression context. The default value of @scheme[check-context?] is
@scheme[#t].

If @scheme[opt-kws?] is @scheme[#t], then arguments of the form
@scheme[[id expr]], @scheme[keyword id], and @scheme[keyword [id
expr]] are allowed, and they are preserved in the expansion.}
