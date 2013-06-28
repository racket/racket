#lang scribble/doc
@(require "common.rkt" scribble/eval
          (for-label net/cookie net/cookie-unit net/cookie-sig))

@(define cookie-eval (make-base-eval))
@interaction-eval[#:eval cookie-eval (require net/cookie)]


@title[#:tag "cookie"]{Cookie: HTTP Client Storage}

@defmodule[net/cookie]{The @racketmodname[net/cookie] library provides
utilities for using cookies as specified in RFC 2109 @cite["RFC2109"].}

@section[#:tag "cookie-procs"]{Functions}

@defproc[(cookie? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] represents a cookie, @racket[#f]
 otherwise.
}

@defproc[(valid-domain? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] represents a valid domain,
 @racket[#f] otherwise.
}

@defproc[(cookie-name? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a valid cookie name string,
 @racket[#f] otherwise.
}

@defproc[(cookie-value? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a valid cookie value string,
 @racket[#f] otherwise.
}

@defproc[(set-cookie [name cookie-name?] [value cookie-value?]) cookie?]{

Creates a new cookie, with default values for required fields.}

@defproc[(cookie:add-comment [cookie cookie?] [comment string?])
         cookie?]{

Modifies @racket[cookie] with a comment, and also returns
@racket[cookie].}

@defproc[(cookie:add-domain [cookie cookie?] [domain valid-domain?])
         cookie?]{

Modifies @racket[cookie] with a domain, and also returns
@racket[cookie]. The @racket[domain] must match a prefix of the
request URI.}

@defproc[(cookie:add-max-age [cookie cookie?] [seconds exact-nonnegative-integer?])
         cookie?]{

Modifies @racket[cookie] with a maximum age, and also returns
@racket[cookie]. The @racket[seconds] argument is number of seconds
that a client should retain the cookie.}

@defproc[(cookie:add-path [cookie cookie?] [path valid-path?])
         cookie?]{

Modifies @racket[cookie] with a path, and also returns
@racket[cookie].}

@defproc[(cookie:add-expires [cookie cookie?] [path string])
         cookie?]{

Modifies @racket[cookie] with an expiration, and also returns
@racket[cookie].}

@defproc[(cookie:secure [cookie cookie?] [secure boolean?])
         cookie?]{

Modifies @racket[cookie] with a security flag, and also returns
@racket[cookie].}

@defproc[(cookie:version [cookie cookie?] [version exact-nonnegative-integer?])
         cookie?]{

Modifies @racket[cookie] with a version, and also returns
@racket[cookie]. The default is the only known incarnation of HTTP
cookies: @racket[1].}

@defproc[(print-cookie [cookie cookie?]) string?]{

Prints @racket[cookie] to a string. Empty fields do not appear in the
output except when there is a required default.}


@defproc[(get-cookie [name cookie-name?] [cookies string?]) (listof cookie-value?)]{

Returns a list with all the values (strings) associated with @racket[name].

The method used to obtain the @racket["Cookie"] header depends on the
web server.  It may be an environment variable (CGI), or you may have
to read it from the input port (FastCGI), or maybe it comes in an
initial-request structure, etc.  The @racket[get-cookie] and
@racket[get-cookie/single] procedure can be used to extract fields
from a @racket["Cookie"] field value.}


@defproc[(get-cookie/single [name cookie-name?] [cookies string?]) (or/c cookie-value? false/c)]{

Like @racket[get-cookie], but returns the just first value string
associated to @racket[name], or #f if no association is found.}


@defstruct[(cookie-error exn:fail) ()]{

Raised for errors when handling cookies.}

@section[#:tag "cookie-examples"]{Examples}

@subsection{Creating a cookie}

@racketblock[
(let ([c (cookie:add-max-age
          (cookie:add-path
           (set-cookie "foo" "bar")
           "/servlets")
          3600)])
  (print-cookie c))
]

Produces

@racketblock[
@#,racketresultfont{"foo=bar; Max-Age=3600; Path=/servlets; Version=1"}
]

To use this output in a ``regular'' CGI, instead of the last line use:

@racketblock[
  (display (format "Set-Cookie: ~a" (print-cookie c)))
]

and to use with the PLT Web Server, use:

@racketblock[
  (make-response/full code message (current-seconds) mime
                      (list (make-header #"Set-Cookie" (string->bytes/utf-8 (print-cookie c))))
                      body)
]

@subsection{Parsing a cookie}

Imagine your Cookie header looks like this:

@interaction[
#:eval cookie-eval
(define cookies 
  "test2=2; test3=3; xfcTheme=theme6; xfcTheme=theme2")
]

Then, to get the values of the xfcTheme cookie, use

@interaction[
#:eval cookie-eval
(get-cookie "xfcTheme" cookies)
(get-cookie/single "xfcTheme" cookies)
]

If you try to get a cookie that simply is not there:

@interaction[
#:eval cookie-eval
(get-cookie/single "foo" cookies)
(get-cookie "foo" cookies)
]

Note that not having a cookie is normally not an error.  Most clients
won't have a cookie set then first arrive at your site.


@; ----------------------------------------

@section{Cookie Unit}

@margin-note{@racket[cookie@] and @racket[cookie^] are deprecated.
They exist for backward-compatibility and will likely be removed in
the future. New code should use the @racketmodname[net/cookie] module.}

@defmodule[net/cookie-unit]

@defthing[cookie@ unit?]{

Imports nothing, exports @racket[cookie^].}

@; ----------------------------------------

@section{Cookie Signature}

@defmodule[net/cookie-sig]

@defsignature[cookie^ ()]{}

Includes everything exported by the @racketmodname[net/cookie] module.


@close-eval[cookie-eval]
