#lang scribble/doc
@(require "web-server.rkt")
@(require (for-label web-server/servlet
                     web-server/page
                     racket/promise
                     racket/list
                     xml))

@title[#:tag "page"]{Page: Short-hand for Common Patterns}

@defmodule[web-server/page]

The @web-server provides a simple utility library for building Web applications that consistent mostly of @racket[send/suspend/dispatch]-created pages and request handling.

Most Web applications rely heavily on @racket[send/suspend/dispatch] and typically use the pattern:
@racketblock[
 (send/suspend/dispatch
  (λ (my-embed/url)
    .... (my-embed/url other-page) ....))]

@defform[(page e ...)]{

The @racket[page] macro automates this by expanding @racket[(page e ...)] to a usage of @racket[send/suspend/dispatch] where the syntax parameter @racket[embed/url] is bound to the argument of @racket[send/suspend/dispatch]. 

}

@defidform[embed/url]{
When used inside @racket[page] syntactically, a rename transformer for the procedure embedding function; otherwise, a syntax error.}

A simple example:
@racketblock[
 (page
  (response/xexpr
   `(html 
     (body 
      (a ([href 
           ,(embed/url 
             (λ (req)
               "You clicked!"))])
         "Click me")))))]

Similarly, many Web applications make use almost exclusively of
functions that are arguments to @racket[embed/url] and immediately
invoke @racket[send/suspend/dispatch].

@deftogether[[@defform[(lambda/page formals e ...)]
              @defform[(define/page (id . formals) e ...)]]]{
  The @racket[lambda/page] and @racket[define/page] automate this by
  expanding to functions that accept a request as the first argument
  (followed by any arguments specified in @racket[formals]) and
  immediately wrap their body in @racket[page]. This functions also
  cooperate with @racket[get-binding] by binding the request to the
  @racket[current-request] parameter.
}

The binding interface of @racketmodname[web-server/http] is powerful,
but subtle to use conveniently due to its protection against hostile
clients.

@deftogether[[
@defparam[current-request req request?]
@defthing[binding-id/c contract?]
@defthing[binding-format/c contract?]
@defproc[(get-binding [id binding-id/c]
                      [req request? (current-request)]
                      [#:format format binding-format/c 'string])
         (or/c false/c string? bytes? binding?)]
@defproc[(get-bindings [id binding-id/c]
                       [req request? (current-request)]
                       [#:format format binding-format/c 'string])
         (listof (or/c string? bytes? binding?))]
]]{

  The @racket[get-binding](s) interface attempts to resolve this by
  providing a powerful interface with convenient defaults.

  @racket[get-binding] extracts the first binding of a form input from a
  request, while @racket[get-bindings] extracts them all.

  They accept a form identifier (@racket[id]) as either a byte string, a
  string, or a symbol. In each case, the user input is compared in a
  case-sensitive way with the form input.

  They accept an optional request argument (@racket[req]) that defaults
  to the value of the @racket[current-request] parameter used by
  @racket[lambda/page] and @racket[define/page].

  Finally, they accept an optional keyword argument (@racket[format])
  that specifies the desired return format. The default,
  @racket['string], produces a UTF-8 string (or @racket[#f] if the byte
  string cannot be converted to UTF-8.) The @racket['bytes] format
  always produces the raw byte string. The @racket['file] format
  produces the file upload content (or @racket[#f] if the form input was
  not an uploaded file.) The @racket['binding] format produces the
  binding object.

}
