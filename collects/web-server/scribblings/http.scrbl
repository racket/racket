#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "http"]{HTTP: Hypertext Transfer Protocol}

@defmodule[web-server/http]

The @web-server implements many HTTP RFCs that are provided by this module.

@; ------------------------------------------------------------
@section[#:tag "request-structs.ss"]{Requests}
@(require (for-label web-server/http/request-structs
                     xml
                     scheme/match))

@defmodule[web-server/http/request-structs]{

@defstruct[header ([field bytes?]
                   [value bytes?])]{
 Represents a header of @scheme[field] to @scheme[value].
}

@defproc[(headers-assq [id bytes?] [heads (listof header?)])
         (or/c false/c header?)]{
 Returns the header with a field equal to @scheme[id] from @scheme[heads] or @scheme[#f].
}

@defproc[(headers-assq* [id bytes?] [heads (listof header?)])
         (or/c false/c header?)]{
 Returns the header with a field case-insensitively equal to @scheme[id] from @scheme[heads] or @scheme[#f].
         
 You almost @bold{always} want to use this, rather than @scheme[headers-assq] because Web browsers may send headers with arbitrary casing.
}

@defstruct[binding ([id bytes?])]{Represents a binding of @scheme[id].}

@defstruct[(binding:form binding) ([value bytes?])]{
 Represents a form binding of @scheme[id] to @scheme[value].
}

@defstruct[(binding:file binding) ([filename bytes?]
                                   [headers (listof header?)]
                                   [content bytes?])]{
 Represents the uploading of the file @scheme[filename] with the id @scheme[id]
 and the content @scheme[content], where @scheme[headers] are the additional headers from
 the MIME envelope the file was in. (For example, the @scheme[#"Content-Type"] header may
 be included by some browsers.)
}

@defproc[(bindings-assq [id bytes?]
                        [binds (listof binding?)])
         (or/c false/c binding?)]{
 Returns the binding with an id equal to @scheme[id] from @scheme[binds] or @scheme[#f].
}

@defstruct[request ([method bytes?]
                    [uri url?]
                    [headers/raw (listof header?)]
                    [bindings/raw-promise (promise/c (listof binding?))]
                    [post-data/raw (or/c false/c bytes?)]
                    [host-ip string?]
                    [host-port number?]
                    [client-ip string?])]{
 An HTTP @scheme[method] request to @scheme[uri] from @scheme[client-ip]
 to the server at @scheme[host-ip]:@scheme[host-port] with @scheme[headers/raw]
 headers, @scheme[bindings/raw] GET and POST queries and @scheme[post-data/raw]
 POST data.
 
 You are @bold{unlikely to need to construct} a request struct.
}
                                         
@defproc[(request-bindings/raw [r request?])
         (listof binding?)]{
 Forces @scheme[(request-bindings/raw-promise r)].
}
                                         
Here is an example typical of what you will find in many applications:
@schemeblock[
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
@section[#:tag "bindings.ss"]{Bindings}
@(require (for-label web-server/http/bindings))

@defmodule[web-server/http/bindings]{

These functions, while convenient, could introduce subtle bugs into your
application. Examples: that they are case-insensitive could introduce
a bug; if the data submitted is not in UTF-8 format, then the conversion
to a string will fail; if an attacker submits a form field as if it were
a file, when it is not, then the @scheme[request-bindings] will hold a
@scheme[bytes?] object and your program will error; and, for file uploads
you lose the filename. @bold{Therefore, we recommend against their use, but
they are provided for compatibility with old code.}

@defproc[(request-bindings [req request?])
         (listof (or/c (cons/c symbol? string?)
                       (cons/c symbol? bytes?)))]{
 Translates the @scheme[request-bindings/raw] of @scheme[req] by
 interpreting @scheme[bytes?] as @scheme[string?]s, except in the case
 of @scheme[binding:file] bindings, which are left as is. Ids are then
 translated into lowercase symbols.
}

@defproc[(request-headers [req request?])
         (listof (cons/c symbol? string?))]{
 Translates the @scheme[request-headers/raw] of @scheme[req] by
 interpreting @scheme[bytes?] as @scheme[string?]s. Ids are then
 translated into lowercase symbols.
}

@defproc[(extract-binding/single [id symbol?]
                                 [binds (listof (cons/c symbol? string?))])
         string?]{
 Returns the single binding associated with @scheme[id] in the a-list @scheme[binds]
 if there is exactly one binding. Otherwise raises @scheme[exn:fail].
}

@defproc[(extract-bindings [id symbol?]
                           [binds (listof (cons/c symbol? string?))])
         (listof string?)]{
 Returns a list of all the bindings of @scheme[id] in the a-list @scheme[binds].
}

@defproc[(exists-binding? [id symbol?]
                          [binds (listof (cons/c symbol? string))])
         boolean?]{
 Returns @scheme[#t] if @scheme[binds] contains a binding for @scheme[id].
 Otherwise, @scheme[#f].
}
                  
Here is an example typical of what you will find in many applications:
@schemeblock[
(define (get-number req)
  (string->number
   (extract-binding/single
    'number
    (request-bindings req))))
]

}

@; ------------------------------------------------------------
@section[#:tag "response-structs.ss"]{Responses}
@(require (for-label web-server/http/response-structs))

@defmodule[web-server/http/response-structs]{

@defstruct[response/basic
           ([code number?]
            [message bytes?]
            [seconds number?]
            [mime bytes?]
            [headers (listof header?)])]{
 A basic HTTP response containing no body. @scheme[code] is the response code,
 @scheme[message] the message, @scheme[seconds] the generation time, @scheme[mime]
 the MIME type of the file, and @scheme[extras] are the extra headers, in addition
 to those produced by the server.
 
 Example:
 @schemeblock[
  (make-response/basic
   301 #"Moved Permanently"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (make-header #"Location"
                      #"http://www.plt-scheme.org/downloads")))
 ]
}

@defstruct[(response/full response/basic)
           ([body (listof bytes?)])]{
 As with @scheme[response/basic], except with @scheme[body] as the response
 body.

 Example:
 @schemeblock[
  (make-response/full
   301 #"Moved Permanently"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (make-header #"Location"
                      #"http://www.plt-scheme.org/downloads"))
   (list #"<html><body><p>"
         #"Please go to <a href=\""
         #"http://www.plt-scheme.org/downloads"
         #"\">here</a> instead."
         #"</p></body></html>"))
 ]
}

@defstruct[(response/incremental response/basic)
           ([generator ((() () #:rest (listof bytes?) . ->* . any) . -> . any)])]{
 As with @scheme[response/basic], except with @scheme[generator] as a function that is
 called to generate the response body, by being given an @scheme[output-response] function
 that outputs the content it is called with. If the @scheme[output-response] function is called
 with arguments of zero length (when concatenated), then the output port is flushed with
 @scheme[flush-output].
 
 Here is a short example:
 @schemeblock[
  (make-response/incremental
    200 #"OK" (current-seconds)
    #"application/octet-stream"
    (list (make-header #"Content-Disposition"
                       #"attachment; filename=\"file\""))
    (lambda (output-response)
      (output-response #"Some content")
      (output-response)
      (output-response #"Even" #"more" #"content!")
      (output-response #"Now we're done")))
 ]
}

@defthing[response/c contract?]{
 Equivalent to 
 @schemeblock[
 (or/c response/basic?
       (cons/c bytes? (listof (or/c string? bytes?)))
       xexpr/c)
 ]
}

@defproc[(make-xexpr-response [xexpr xexpr/c]
                              [#:code code number? 200]
                              [#:message message bytes? #"Okay"]
                              [#:seconds seconds number? (current-seconds)]
                              [#:mime-type mime-type bytes? TEXT/HTML-MIME-TYPE]
                              [#:headers headers (listof header?) empty])
         response/full?]{
 Equivalent to
 @schemeblock[
 (make-response/full 
  code message seconds mime-type headers
  (list (string->bytes/utf-8 (xexpr->string xexpr))))
 ]}
                         
@defproc[(normalize-response [close? boolean?] [response response/c])
         (or/c response/full? response/incremental?)]{
 Coerces @scheme[response] into a full response, filling in additional details where appropriate.
}

@defthing[TEXT/HTML-MIME-TYPE bytes?]{Equivalent to @scheme[#"text/html; charset=utf-8"].}

@warning{If you include a Content-Length header in a response that is inaccurate, there @bold{will be an error} in
transmission that the server @bold{will not catch}.}

}

@; ------------------------------------------------------------
@section[#:tag "cookie"]{Placing Cookies}

@(require (for-label net/cookie
                     web-server/servlet
                     web-server/http/redirect
                     web-server/http/request-structs
                     web-server/http/response-structs
                     web-server/http/cookie))

@defmodule[web-server/http/cookie]{
 This module provides functions to create cookies and responses that set them.
      
 @defproc[(make-cookie [name string?] [value string?]
                       [#:comment comment (or/c false/c string?) #f]
                       [#:domain domain (or/c false/c valid-domain?) #f]
                       [#:max-age max-age (or/c false/c exact-nonnegative-integer?) #f]
                       [#:path path (or/c false/c string?) #f]
                       [#:secure? secure? (or/c false/c boolean?) #f])
          cookie?]{
  Constructs a cookie with the appropriate fields.
 }
 
 @defproc[(cookie->header [c cookie?]) header?]{
  Constructs a header that sets the cookie.
 }             
                  
 @defproc[(xexpr-response/cookies [cookies (listof cookie?)]
                                  [xexpr xexpr/c])
          response/full?]{
  Constructs a response using @scheme[xexpr] that sets all the cookies in @scheme[cookies].
 }
                         
 Examples:
 @schemeblock[
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
      (xexpr-response/cookies
       (list time-cookie id-cookie)
       `(html (head (title "Cookie Example"))
              (body (h1 "You're cookie'd!"))))))
 ]
 
 @warning{When using cookies, make sure you follow the advice of the @link["http://cookies.lcs.mit.edu/"]{MIT Cookie Eaters},
          or you will be susceptible to dangerous attacks.} 
}                                  

@; ------------------------------------------------------------
@section[#:tag "cookie-parse"]{Extracting Cookies}

@(require (for-label web-server/http/cookie-parse
                     net/cookie
                     net/url
                     scheme/list))
@defmodule[web-server/http/cookie-parse]{
 @defstruct[client-cookie 
            ([name string?]
             [value string?]
             [domain (or/c false/c valid-domain?)]
             [path (or/c false/c string?)])]{
                              
  While server cookies are represented with @scheme[cookie?]s, cookies that come from the client are represented
  with a @scheme[client-cookie] structure.                              
 }
 
 @defproc[(request-cookies [req request?])
          (listof client-cookie?)]{
  Extracts the cookies from @scheme[req]'s headers.
 }

 Examples:
 @schemeblock[
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
     `(html (head (title "Hello!"))
            (body 
             (h1 "Hello " 
                 ,who))))
 ]
}

@; ------------------------------------------------------------
@section[#:tag "redirect.ss"]{Redirect}
@(require (for-label web-server/http/redirect
                     web-server/private/util))

@defmodule[web-server/http/redirect]{

@defproc[(redirect-to [uri non-empty-string/c]
                      [perm/temp redirection-status? temporarily]
                      [#:headers headers (listof header?) (list)])
         response/c]{
 Generates an HTTP response that redirects the browser to @scheme[uri],
 while including the @scheme[headers] in the response.
 
 Example:
 @scheme[(redirect-to "http://www.add-three-numbers.com" permanently)]
}

@defproc[(redirection-status? [v any/c])
         boolean?]{
 Determines if @scheme[v] is one of the following values.
}

@defthing[permanently redirection-status?]{A @scheme[redirection-status?] for permanent redirections.}

@defthing[temporarily redirection-status?]{A @scheme[redirection-status?] for temporary redirections.}

@defthing[see-other redirection-status?]{A @scheme[redirection-status?] for "see-other" redirections.}

}

@; ------------------------------------------------------------
@section[#:tag "basic-auth.ss"]{Basic Authentication}
@(require (for-label web-server/http/basic-auth))

@defmodule[web-server/http/basic-auth]{

An implementation of HTTP Basic Authentication.
   
@defproc[(make-basic-auth-header [realm string?])
         header?]{
 Returns a header that instructs the Web browser to request a username and password from the client using
 Basic authentication with @scheme[realm] as the realm.
}
                 
@defproc[(request->basic-credentials [req request?])
         (or/c false/c (cons/c bytes? bytes?))]{
 Returns a pair of the username and password from the authentication
 header in @scheme[req] if they are present, or @scheme[#f]. 
}
                                               
Example:
@schememod[
web-server/insta
 
(define (start req)
  (match (request->basic-credentials req)
    [(cons user pass)
     `(html (head (title "Basic Auth Test"))
            (body (h1 "User: " ,(bytes->string/utf-8 user))
                  (h1 "Pass: " ,(bytes->string/utf-8 pass))))]
    [else
     (make-response/basic
      401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE
      (list 
       (make-basic-auth-header 
        (format "Basic Auth Test: ~a" (gensym)))))])) 
]
}

@; ------------------------------------------------------------
@section[#:tag "digest-auth.ss"]{Digest Authentication}
@(require (for-label web-server/http/digest-auth
                     scheme/pretty))

@defmodule[web-server/http/digest-auth]{

An implementation of HTTP Digest Authentication.
   
@defproc[(make-digest-auth-header [realm string?] [private-key string?] [opaque string?])
         header?]{
 Returns a header that instructs the Web browser to request a username and password from the client
 using Digest authentication with @scheme[realm] as the realm, @scheme[private-key] as the server's
 contribution to the nonce, and @scheme[opaque] as the opaque data passed through the client.
}
                 
@defproc[(request->digest-credentials [req request?])
         (or/c false/c (listof (cons/c symbol? string?)))]{
 Returns the Digest credentials from @scheme[req] (if they appear) as an association list.
}         
                                                          
@defthing[username*realm->password/c contract?]{
 Used to look up the password for a user is a realm.                                                
                                                
 Equivalent to @scheme[(string? string? . -> . string?)].                                                
}

@defthing[username*realm->digest-HA1/c contract?]{
 Used to compute the user's secret hash.
      
 Equivalent to @scheme[(string? string? . -> . bytes?)].
} 
       
@defproc[(password->digest-HA1 [lookup-password username*realm->password/c])
         username*realm->digest-HA1/c]{
 Uses @scheme[lookup-password] to find the password, then computes the secret hash of it.
}      
                                       
@defproc[(make-check-digest-credentials [lookup-HA1 username*realm->digest-HA1/c])
         (string? (listof (cons/c symbol? string?)) . -> . boolean?)]{
 Constructs a function that checks whether particular Digest credentials (the second argument of the returned function)
 are correct given the HTTP method provided as the first argument and the secret hash computed by @scheme[lookup-HA1].
 
 This is will result in an exception if the Digest credentials are missing portions.
} 
                                                                     
Example:
@schememod[
web-server/insta
(require scheme/pretty)

(define private-key "private-key")
(define opaque "opaque")

(define (start req)
  (match (request->digest-credentials req)
    [#f
     (make-response/basic
      401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE
      (list (make-digest-auth-header
             (format "Digest Auth Test: ~a" (gensym))
             private-key opaque)))]
    [alist
     (define check
       (make-check-digest-credentials
        (password->digest-HA1 (lambda (username realm) "pass"))))
     (define pass?
       (check "GET" alist))
     `(html (head (title "Digest Auth Test"))
            (body 
             (h1 ,(if pass? "Pass!" "No Pass!"))
             (pre ,(pretty-format alist))))])) 
]
}
