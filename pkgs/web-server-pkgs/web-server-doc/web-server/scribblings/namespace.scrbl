#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "namespace"]{Servlet Namespaces}
@(require (for-label web-server/configuration/namespace))

@defmodule[web-server/configuration/namespace]{

This module provides a function to help create the
@racket[make-servlet-namespace] procedure needed by the @racket[make] function
of @racketmodname[web-server/dispatchers/dispatch-servlets].

@defthing[make-servlet-namespace/c contract?]{
 Equivalent to 
 @racketblock[
(->* ()
     (#:additional-specs (listof module-path?))
     namespace?)
]
}

@defproc[(make-make-servlet-namespace (#:to-be-copied-module-specs to-be-copied-module-specs (listof module-path?)))
         make-servlet-namespace/c]{
This function creates a function that when called will construct a new
@racket[namespace] that has all the modules from
@racket[to-be-copied-module-specs] and @racket[additional-specs], as
well as @racket[racket] and @racket[mred], provided they are already
attached to the @racket[(current-namespace)] of the call-site.

Example:
@racketblock[
 (make-make-servlet-namespace
  #:to-be-copied-module-specs `((lib "database.rkt" "my-module")))
 ]
}

}

@section{Why this is useful}

A different namespace is needed for each servlet, so that if servlet A
and servlet B both use a stateful module C, they will be isolated from
one another. We see the @web-server as an operating system for servlets,
so we inherit the isolation requirement on operating systems.

However, there are some modules which must be shared. If they were not,
then structures cannot be passed from the @web-server to the servlets,
because Racket's structures are generative.

Since, on occasion, a user will actually wanted servlets A and B to
interact through module C.  A custom @racket[make-servlet-namespace] can
be created, through this procedure, that attaches module C to all
servlet namespaces. Through other means (see @secref["dispatchers"])
different sets of servlets can share different sets of modules.
