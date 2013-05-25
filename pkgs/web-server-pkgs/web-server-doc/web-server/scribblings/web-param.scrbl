#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "lang/web-param"]{Stateless Web Parameters}
@(require (for-label web-server/lang/web-param))

@defmodule[web-server/lang/web-param]{

It is not easy to use @racket[parameterize] in the
Web Language. This module provides (roughly) the same
functionality in a way that is serializable. Like other serializable
things in the Web Language, they are sensitive to source code modification.

@defform[(make-web-parameter default)]{
 Expands to the definition of a web-parameter with
 @racket[default] as the default value. A web-parameter is
 a procedure that, when called with zero arguments, returns @racket[default]
 or the last value @racket[web-parameterize]d in the dynamic context
 of the call.
}

@defproc[(web-parameter? [v any/c])
         boolean?]{
 Checks if @racket[v] appears to be a web-parameter.
}

@defform[(web-parameterize ([web-parameter-expr value-expr] ...) expr ...)]{
 Runs @racket[(begin expr ...)] such that the web-parameters that
 the @racket[web-parameter-expr]s evaluate to are bound to the @racket[value-expr]s.
 From the perspective of the @racket[value-expr]s, this is like @racket[let].
}
}
