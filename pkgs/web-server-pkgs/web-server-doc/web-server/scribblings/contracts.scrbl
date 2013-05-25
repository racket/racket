#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "servlet-structs"]{Responses}
@(require (for-label web-server/servlet/servlet-structs
                     web-server/http
                     web-server/servlet))

@defmodule[web-server/servlet/servlet-structs]{

Servlets communicate to the Web Server by returning HTTP responses. In order to
accommodate lightweight programs (and backwards compatibility), the Web Server
provides an indirection from application-specific response formats and the internal
HTTP response format, @racket[response].

@deftogether[[
@defproc[(can-be-response? [x any/c])
          boolean?]
@defproc[(any->response [x any/c])
         (or/c false/c response?)]
@defproc[(set-any->response! [new-any->response (-> any/c (or/c false/c response?))])
         void]
]]{

   @racket[any->response] coerces any value into a response or returns @racket[#f] if coercion is not possible.
   @racket[any->response] guarantees that any @racket[response?] input must always be returned exactly (i.e. @racket[eq?].)
   The default always returns @racket[#f], signifying that no coercion is possible.
   
   @racket[can-be-response?] returns @racket[#t] if @racket[x] is a response or can be turned into a response by calling
   @racket[any->response].
   
   Users of @racket[any->response] should protect themselves by using @racket[can-be-response?] as a contract.
   If they do so, they can safely ignore the @racket[#f] return case of @racket[any->response].
   
   @racket[set-any->response!] replaces the global @racket[any->response] with the supplied argument. This
   function should return the same value for @racket[eq?] inputs to ensure that @racket[can-be-response?] is
   any accurate predicate. Similarly, this function should be cheap to call multiple times on the same input,
   since it will be used in contract checking as well as coercion before transmission. You may want to use a
   weak @racket[eq?]-based hash-table to cache the results for this purpose. (See @racket[make-weak-hasheq].)
   
}

}
