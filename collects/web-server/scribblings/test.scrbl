#lang scribble/doc
@(require "web-server.rkt"
          (for-label web-server/http/request-structs
                     web-server/servlet/servlet-structs
                     xml
                     web-server/test
                     net/url
                     racket/promise
                     racket/match))

@title[#:tag "test"]{Testing Servlets}

@defmodule[web-server/test]

The @web-server provides a simple facility for writing tests for Web servlets.

The core functionality allows a request to be sent to the servlet and the response captured:

@defproc[(make-servlet-tester
          [servlet 
           (-> request?
               can-be-response?)])
         (->* ()
              ((or/c string? url? request? false/c)
               (listof binding?)
               #:raw? boolean?)
              (or/c bytes?
                    xexpr?))]{
 
This function accepts a servlet function and provides a function that accepts a request and returns the answer the servlet for that request. This interaction function has many possible calling patterns:
@itemize[
 @item{No arguments: a call to the root URL path with no bindings.}
 @item{At least one argument: this may be a string, URL, or a request data structure.}
 @item{Two arguments: the first argument must be a string or a URL, but the second argument can specify the request bindings.}
 @item{The optional @racket[#:raw?] keyword controls whether an X-expression or a byte string is returned as a result.}
]
}

This facility is designed to be used in concert with a technique of
extracting continuation URLs and relevant values;
@racketmodname[xml/path] is one way to do this. Here is an extended
example that tests an Add-Two-Numbers.com:
@(require (for-label xml/path
                     rackunit
                     racket/list
                     racket/promise))
@racketblock[
(define (test-add-two-numbers -s>)
  (define x (random 500))
  (define xs (string->bytes/utf-8 (number->string x)))
  (define y (random 500))
  (define ys (string->bytes/utf-8 (number->string y)))
  
  (define r0 (-s>))
  (define k0 (se-path* '(form #:action) r0))
  (define i0 (se-path* '(form input #:name) r0))
  (define r1 
    (-s> (format "~a?~a=~a" k0 i0 xs)
         (list (make-binding:form (string->bytes/utf-8 i0) xs))))
  (define k1 (se-path* '(form #:action) r1))
  (define i1 (se-path* '(form input #:name) r1))
  (define r2 
    (-s> (format "~a?~a=~a" k1 i1 ys)
         (list (make-binding:form (string->bytes/utf-8 i1) ys))))
  (define n (se-path* '(p) r2))
  (check-equal? n
                (format "The answer is ~a" (+ x y))))

(require
 (prefix-in ex:add1: web-server/default-web-root/htdocs/servlets/examples/add)
 (prefix-in ex:add2: web-server/default-web-root/htdocs/servlets/examples/add-v2))

(test-add-two-numbers 
 (make-servlet-tester ex:add1:start))
(test-add-two-numbers 
 (make-servlet-tester ex:add2:start))
]
