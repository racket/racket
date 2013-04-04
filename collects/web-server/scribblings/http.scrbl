#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "http"]{HTTP: Hypertext Transfer Protocol}

@defmodule[web-server/http]

The @web-server implements many HTTP libraries that are provided by this module.

@; ------------------------------------------------------------
@section[#:tag "request-structs"]{Requests}
@(require (for-label web-server/http/request-structs
                     xml
                     racket/promise
                     racket/match))

@defmodule[web-server/http/request-structs]{

@defstruct[header ([field bytes?]
                   [value bytes?])]{
 Represents a header of @racket[field] to @racket[value].
}

@defproc[(headers-assq [id bytes?] [heads (listof header?)])
         (or/c false/c header?)]{
 Returns the header with a field equal to @racket[id] from @racket[heads] or @racket[#f].
}

@defproc[(headers-assq* [id bytes?] [heads (listof header?)])
         (or/c false/c header?)]{
 Returns the header with a field case-insensitively equal to @racket[id] from @racket[heads] or @racket[#f].

 You almost @bold{always} want to use this, rather than @racket[headers-assq] because Web browsers may send headers with arbitrary casing.
}

@defstruct[binding ([id bytes?])]{Represents a binding of @racket[id].}

@defstruct[(binding:form binding) ([value bytes?])]{
 Represents a form binding of @racket[id] to @racket[value].
}

@defstruct[(binding:file binding) ([filename bytes?]
                                   [headers (listof header?)]
                                   [content bytes?])]{
 Represents the uploading of the file @racket[filename] with the id @racket[id]
 and the content @racket[content], where @racket[headers] are the additional headers from
 the MIME envelope the file was in. (For example, the @racket[#"Content-Type"] header may
 be included by some browsers.)
}

@defproc[(bindings-assq [id bytes?]
                        [binds (listof binding?)])
         (or/c false/c binding?)]{
 Returns the binding with an id equal to @racket[id] from @racket[binds] or @racket[#f].
}

@defproc[(bindings-assq-all [id bytes?]
                            [binds (listof binding?)])
         (listof binding?)]{
Like @racket[bindings-assq], but returns a list of all bindings matching @racket[id].
}


@defstruct[request ([method bytes?]
                    [uri url?]
                    [headers/raw (listof header?)]
                    [bindings/raw-promise (promise/c (listof binding?))]
                    [post-data/raw (or/c false/c bytes?)]
                    [host-ip string?]
                    [host-port number?]
                    [client-ip string?])]{
 An HTTP @racket[method] request to @racket[uri] from @racket[client-ip]
 to the server at @racket[host-ip]:@racket[host-port] with @racket[headers/raw]
 headers, @racket[bindings/raw] GET and POST queries and @racket[post-data/raw]
 POST data.

 You are @bold{unlikely to need to construct} a request struct.
}

@defproc[(request-bindings/raw [r request?])
         (listof binding?)]{
 Forces @racket[(request-bindings/raw-promise r)].
}

Here is an example typical of what you will find in many applications:
@racketblock[
(define (get-number req)
  (match
    (bindings-assq
      #"number"
      (request-bindings/raw req))
    [(? binding:form? b)
     (string->number
      (bytes->string/utf-8
       (binding:form-value b)))]
    [_
     (get-number (request-number))]))
]

}

@; ------------------------------------------------------------
@section[#:tag "bindings"]{Bindings}
@(require (for-label web-server/http/bindings))

@defmodule[web-server/http/bindings]{

These functions, while convenient, could introduce subtle errors into your
application. Examples: that they are case-insensitive could introduce
an error; if the data submitted is not in UTF-8 format, then the conversion
to a string will fail; if an attacker submits a form field as if it were
a file, when it is not, then the @racket[request-bindings] will hold a
@racket[bytes?] object and your program will error; and, for file uploads
you lose the filename. @bold{Therefore, we recommend against their use, but
they are provided for compatibility with old code.}

@defproc[(request-bindings [req request?])
         (listof (or/c (cons/c symbol? string?)
                       (cons/c symbol? bytes?)))]{
 Translates the @racket[request-bindings/raw] of @racket[req] by
 interpreting @racket[bytes?] as @racket[string?]s, except in the case
 of @racket[binding:file] bindings, which are left as is. Ids are then
 translated into lowercase symbols.
}

@defproc[(request-headers [req request?])
         (listof (cons/c symbol? string?))]{
 Translates the @racket[request-headers/raw] of @racket[req] by
 interpreting @racket[bytes?] as @racket[string?]s. Ids are then
 translated into lowercase symbols.
}

@defproc[(extract-binding/single [id symbol?]
                                 [binds (listof (cons/c symbol? string?))])
         string?]{
 Returns the single binding associated with @racket[id] in the a-list @racket[binds]
 if there is exactly one binding. Otherwise raises @racket[exn:fail].
}

@defproc[(extract-bindings [id symbol?]
                           [binds (listof (cons/c symbol? string?))])
         (listof string?)]{
 Returns a list of all the bindings of @racket[id] in the a-list @racket[binds].
}

@defproc[(exists-binding? [id symbol?]
                          [binds (listof (cons/c symbol? string))])
         boolean?]{
 Returns @racket[#t] if @racket[binds] contains a binding for @racket[id].
 Otherwise, @racket[#f].
}

Here is an example typical of what you will find in many applications:
@racketblock[
(define (get-number req)
  (string->number
   (extract-binding/single
    'number
    (request-bindings req))))
]

}

@; ------------------------------------------------------------
@section[#:tag "response-structs"]{Responses}
@(require (for-label web-server/http/response-structs))

@defmodule[web-server/http/response-structs]{

@defstruct*[response
           ([code number?]
            [message bytes?]
            [seconds number?]
            [mime (or/c false/c bytes?)]
            [headers (listof header?)]
            [output (output-port? . -> . void)])]{

An HTTP response where @racket[output] produces the body by writing to
the output port. @racket[code] is the response code, @racket[message]
the message, @racket[seconds] the generation time, @racket[mime] the
MIME type of the file, and @racket[headers] are the headers.

If @racket[headers] does not include @litchar{Date},
@litchar{Last-Modified}, or @litchar{Server} headers, then the server
will automatically add them, where @litchar{Date} is based on
@racket[current-seconds], @litchar{Last-Modified} is based on
@racket[seconds], and @litchar{Server} is @litchar{Racket}.

If @racket[headers] does not include @litchar{Content-Type} and
@racket[mime] is not @racket[#f], then @racket[mime] is added as a
@litchar{Content-Type} header.

The server will always replace your @litchar{Connection} header if it
needs to ensure the connection will be closed. (Typically with an
HTTP/1.0 client.)

Examples:
 @racketblock[
  (response
   301 #"OK"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (λ (op) (write-bytes #"<html><body>Hello, World!</body></html>" op)))
  (response
   301 #"Moved Permanently"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (make-header #"Location"
                      #"http://racket-lang.org/download"))
   (λ (op) (write-bytes #"Moved" op)))
  (response
   304 #"Not Modified"
   (current-seconds) #f
   (list (make-header #"Location"
                      #"http://racket-lang.org/download"))
   void)
 ]
}

@defproc[(response/full [code number?] [message bytes?] [seconds number?] [mime (or/c false/c bytes?)]
                        [headers (listof header?)] [body (listof bytes?)])
         response?]{
 A constructor for responses where @racket[body] is the response body.

 Example:
 @racketblock[
  (response/full
   301 #"Moved Permanently"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (make-header #"Location"
                      #"http://racket-lang.org/download"))
   (list #"<html><body><p>"
         #"Please go to <a href=\""
         #"http://racket-lang.org/download"
         #"\">here</a> instead."
         #"</p></body></html>"))
 ]
}
                   
@defproc[(response/output [output (-> output-port? void?)]
                          [#:code code number? 200]
                          [#:message message bytes? #"Okay"]
                          [#:seconds seconds number? (current-seconds)]
                          [#:mime-type mime-type (or/c bytes? #f) TEXT/HTML-MIME-TYPE]
                          [#:headers headers (listof header?) '()]
                          [#:cookies cookies (listof cookie?) '()])
         response?]{
Equivalent to
@racketblock[(response code message seconds mime-type headers output)]
}

@defthing[TEXT/HTML-MIME-TYPE bytes?]{Equivalent to @racket[#"text/html; charset=utf-8"].}

@warning{If you include a Content-Length header in a response that is inaccurate, there @bold{will be an error} in
transmission that the server @bold{will not catch}.}

}

@; ------------------------------------------------------------
@section[#:tag "cookie"]{Placing Cookies}

@(require (for-label net/cookie
                     web-server/servlet
                     web-server/http/xexpr
                     web-server/http/redirect
                     web-server/http/request-structs
                     web-server/http/response-structs
                     web-server/http/cookie))

@defmodule[web-server/http/cookie]{
 This module provides functions to create cookies and responses that set them.

 @defproc[(make-cookie [name cookie-name?] [value cookie-value?]
                       [#:comment comment (or/c false/c string?) #f]
                       [#:domain domain (or/c false/c valid-domain?) #f]
                       [#:max-age max-age (or/c false/c exact-nonnegative-integer?) #f]
                       [#:path path (or/c false/c string?) #f]
                       [#:expires expires (or/c false/c string?) #f]
                       [#:secure? secure? (or/c false/c boolean?) #f])
          cookie?]{
  Constructs a cookie with the appropriate fields.
 }

 @defproc[(cookie->header [c cookie?]) header?]{
  Constructs a header that sets the cookie.
 }

 Examples:
 @racketblock[
  (define time-cookie
    (make-cookie "time" (number->string (current-seconds))))
  (define id-cookie
    (make-cookie "id" "joseph" #:secure? #t))

  (redirect-to
   "http://localhost/logged-in"
   see-other
   #:headers
   (map cookie->header
        (list time-cookie id-cookie)))

  (send/suspend
   (lambda (k-url)
     (response/xexpr
      #:cookies (list time-cookie id-cookie)
      `(html (head (title "Cookie Example"))
             (body (h1 "You're cookie'd!"))))))
 ]
}

@; ------------------------------------------------------------
@section[#:tag "id-cookie"]{Authenticated Cookies}

@(require (for-label web-server/http/id-cookie))
@defmodule[web-server/http/id-cookie]{

Cookies are useful for storing information of user's browsers and
particularly useful for storing identifying information for
authentication, sessions, etc. However, there are inherent
difficulties when using cookies as authenticators, because cookie data
is fully controlled by the user, and thus cannot be trusted.

This module provides functions for creating and verifying
authenticated cookies that are intrinsically timestamped. It is based
on the algorithm proposed by the
@link["http://cookies.lcs.mit.edu/"]{MIT Cookie Eaters}: if you store
the data @racket[_data] at thime @racket[_authored-seconds], then the
user will receive @litchar{digest&authored-seconds&data}, where
@racket[_digest] is an HMAC-SHA1 digest of @racket[_authored-seconds]
and @racket[_data], using an arbitrary secret key. When you receive a
cookie, it will reverify this digest and check that the cookie's
@racket[_authored-seconds] is not after a timeout period, and only
then return the cookie data to the program.

The interface represents the secret key as a byte string. The best way
to generate this is by using random bytes from something like OpenSSL
or
@tt{/dev/random}. @link["http://www.madboa.com/geek/openssl/#random-generate"]{This
FAQ} lists a few options. A convenient purely Racket-based option is
available (@racket[make-secret-salt/file]), but it will not have as
good entropy, if you care about that sort of thing.

 @defproc[(make-id-cookie
           [name cookie-name?]
           [secret-salt bytes?]
           [value cookie-value?]
           [#:path path (or/c false/c string?) #f])
          cookie?]{
  Generates an authenticated cookie named @racket[name] containing @racket[value], signed with @racket[secret-salt].
 }

 @defproc[(request-id-cookie
           [name cookie-name?]
           [secret-salt bytes?]
           [request request?]
           [#:timeout timeout +inf.0])
          (or/c false/c cookie-value?)]{
  Extracts the first authenticated cookie named @racket[name] that was previously signed with @racket[secret-salt] before @racket[timeout] from @racket[request]. If no valid cookie is available, returns @racket[#f].
 }

 @defproc[(logout-id-cookie
           [name cookie-name?]
           [#:path path (or/c false/c string?) #f])
          cookie?]{
  Generates a cookie named @racket[name] that is not validly authenticated.

  This will cause non-malicious browsers to overwrite a previously set
cookie. If you use authenticated cookies for login information, you
could send this to cause a "logout". However, malicious browsers do
not need to respect such an overwrite. Therefore, this is not an
effective way to implement timeouts or protect users on
public (i.e. possibly compromised) computers. The only way to securely
logout on the compromised computer is to have server-side state
keeping track of which cookies (sessions, etc.) are invalid. Depending
on your application, it may be better to track live sessions or dead
sessions, or never set cookies to begin with and just use
continuations, which you can revoke with @racket[send/finish].
 }

 @defproc[(make-secret-salt/file
            [secret-salt-path path-string?])
           bytes?]{

  Extracts the bytes from @racket[secret-salt-path]. If
@racket[secret-salt-path] does not exist, then it is created and
initialized with 128 random bytes.
 }
}

@; ------------------------------------------------------------
@section[#:tag "cookie-parse"]{Extracting Cookies}

@(require (for-label web-server/http/cookie-parse
                     web-server/http/xexpr
                     net/cookie
                     net/url
                     racket/list))
@defmodule[web-server/http/cookie-parse]{
 @defstruct[client-cookie
            ([name string?]
             [value string?]
             [domain (or/c false/c valid-domain?)]
             [path (or/c false/c string?)])]{

  While server cookies are represented with @racket[cookie?]s, cookies
  that come from the client are represented with a
  @racket[client-cookie] structure.
 }

 @defproc[(request-cookies [req request?])
          (listof client-cookie?)]{
  Extracts the cookies from @racket[req]'s headers.
 }

 Examples:
 @racketblock[
  (define (start req)
    (define cookies (request-cookies req))
    (define id-cookie
      (findf (lambda (c)
               (string=? "id" (client-cookie-name c)))
             cookies))
    (if id-cookie
        (hello (client-cookie-value id-cookie))
        (redirect-to
         (url->string (request-uri req))
         see-other
         #:headers
         (list
          (cookie->header (make-cookie "id" "joseph"))))))

   (define (hello who)
     (response/xexpr
      `(html (head (title "Hello!"))
             (body
              (h1 "Hello "
                  ,who)))))
 ]
}

@; ------------------------------------------------------------
@section[#:tag "redirect"]{Redirect}
@(require (for-label web-server/http/redirect
                     web-server/private/util))

@defmodule[web-server/http/redirect]{

@defproc[(redirect-to [uri non-empty-string/c]
                      [perm/temp redirection-status? temporarily]
                      [#:headers headers (listof header?) (list)])
         response?]{
 Generates an HTTP response that redirects the browser to @racket[uri],
 while including the @racket[headers] in the response.

 Example:
 @racket[(redirect-to "http://www.add-three-numbers.com" permanently)]
}

@defproc[(redirection-status? [v any/c])
         boolean?]{
 Determines if @racket[v] is one of the following values.
}

@defthing[permanently redirection-status?]{A @racket[redirection-status?] for permanent redirections.}

@defthing[temporarily redirection-status?]{A @racket[redirection-status?] for temporary redirections.}

@defthing[see-other redirection-status?]{A @racket[redirection-status?] for "see-other" redirections.}

}

@; ------------------------------------------------------------
@section[#:tag "basic-auth"]{Basic Authentication}
@(require (for-label web-server/http/response-structs
                     web-server/http/basic-auth))

@defmodule[web-server/http/basic-auth]{

An implementation of HTTP Basic Authentication.

@defproc[(make-basic-auth-header [realm string?])
         header?]{
 Returns a header that instructs the Web browser to request a username and password from the client using
 Basic authentication with @racket[realm] as the realm.
}

@defproc[(request->basic-credentials [req request?])
         (or/c false/c (cons/c bytes? bytes?))]{
 Returns a pair of the username and password from the authentication
 header in @racket[req] if they are present, or @racket[#f].
}

Example:
@racketmod[
web-server/insta

(define (start req)
  (match (request->basic-credentials req)
    [(cons user pass)
     (response/xexpr
      `(html (head (title "Basic Auth Test"))
             (body (h1 "User: " ,(bytes->string/utf-8 user))
                   (h1 "Pass: " ,(bytes->string/utf-8 pass)))))]
    [else
     (response
      401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE
      (list
       (make-basic-auth-header
        (format "Basic Auth Test: ~a" (gensym))))
      void)]))
]
}

@; ------------------------------------------------------------
@section[#:tag "digest-auth"]{Digest Authentication}
@(require (for-label web-server/http/digest-auth
                     web-server/http/xexpr
                     web-server/http/response-structs
                     racket/pretty))

@defmodule[web-server/http/digest-auth]{

An implementation of HTTP Digest Authentication.

@defproc[(make-digest-auth-header [realm string?] [private-key string?] [opaque string?])
         header?]{
 Returns a header that instructs the Web browser to request a username and password from the client
 using Digest authentication with @racket[realm] as the realm, @racket[private-key] as the server's
 contribution to the nonce, and @racket[opaque] as the opaque data passed through the client.
}

@defproc[(request->digest-credentials [req request?])
         (or/c false/c (listof (cons/c symbol? string?)))]{
 Returns the Digest credentials from @racket[req] (if they appear) as an association list.
}

@defthing[username*realm->password/c contract?]{
 Used to look up the password for a user is a realm.

 Equivalent to @racket[(string? string? . -> . string?)].
}

@defthing[username*realm->digest-HA1/c contract?]{
 Used to compute the user's secret hash.

 Equivalent to @racket[(string? string? . -> . bytes?)].
}

@defproc[(password->digest-HA1 [lookup-password username*realm->password/c])
         username*realm->digest-HA1/c]{
 Uses @racket[lookup-password] to find the password, then computes the
 secret hash of it.
}

@defproc[(make-check-digest-credentials [lookup-HA1 username*realm->digest-HA1/c])
         (string? (listof (cons/c symbol? string?)) . -> . boolean?)]{
 Constructs a function that checks whether particular Digest credentials
 (the second argument of the returned function) are correct given the
 HTTP method provided as the first argument and the secret hash computed
 by @racket[lookup-HA1].

 This is will result in an exception if the Digest credentials are
 missing portions.
}

Example:
@racketmod[
web-server/insta
(require racket/pretty)

(define private-key "private-key")
(define opaque "opaque")

(define (start req)
  (match (request->digest-credentials req)
    [#f
     (response
      401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE
      (list (make-digest-auth-header
             (format "Digest Auth Test: ~a" (gensym))
             private-key opaque))
      void)]
    [alist
     (define check
       (make-check-digest-credentials
        (password->digest-HA1 (lambda (username realm) "pass"))))
     (define pass?
       (check "GET" alist))
     (response/xexpr
      `(html (head (title "Digest Auth Test"))
             (body
              (h1 ,(if pass? "Pass!" "No Pass!"))
              (pre ,(pretty-format alist)))))]))
]
}

@; ------------------------------------------------------------
@section[#:tag "xexpr"]{X-expression Support}
@(require (for-label web-server/http/xexpr
                     xml))

@defmodule*/no-declare[(web-server/http/xexpr)]{}

@declare-exporting[web-server/http/xexpr web-server]

@defproc[(response/xexpr [xexpr xexpr/c]
                         [#:code code number? 200]
                         [#:message message bytes? #"Okay"]
                         [#:seconds seconds number? (current-seconds)]
                         [#:mime-type mime-type (or/c false/c bytes?) TEXT/HTML-MIME-TYPE]
                         [#:headers headers (listof header?) empty]
                         [#:cookies cookies (listof cookie?) empty]
                         [#:preamble preamble bytes? #""])
         response?]{
 Equivalent to
 @racketblock[
 (response/full
  code message seconds mime-type
  (append headers (map cookie->header cookies))
  (list preamble (string->bytes/utf-8 (xexpr->string xexpr))))
 ]

 This is a viable function to pass to @racket[set-any->response!].
 }
