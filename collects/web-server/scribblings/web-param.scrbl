#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "lang/web-param.ss"]{Stateless Web Parameters}
@(require (for-label web-server/lang/web-param))

@defmodule[web-server/lang/web-param]{

It is not easy to use @scheme[parameterize] in the
Web Language. This module provides (roughly) the same
functionality in a way that is serializable. Like other serializable
things in the Web Language, they are sensitive to source code modification.

@defform[(make-web-parameter default)]{
 Expands to the definition of a web-parameter with
 @scheme[default] as the default value. A web-parameter is
 a procedure that, when called with zero arguments, returns @scheme[default]
 or the last value @scheme[web-parameterize]d in the dynamic context
 of the call.
}

@defproc[(web-parameter? [v any/c])
         boolean?]{
 Checks if @scheme[v] appears to be a web-parameter.
}

@defform[(web-parameterize ([web-parameter-expr value-expr] ...) expr ...)]{
 Runs @scheme[(begin expr ...)] such that the web-parameters that
 the @scheme[web-parameter-expr]s evaluate to are bound to the @scheme[value-expr]s.
 From the perspective of the @scheme[value-expr]s, this is like @scheme[let].
}
}
