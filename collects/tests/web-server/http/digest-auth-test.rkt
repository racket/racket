#lang racket
(require rackunit
         web-server/http
         net/url)
(provide digest-auth-tests)

(define RFC-Example-bytes
  (string->bytes/utf-8 #<<END
Digest username="Mufasa",
                      realm="testrealm@host.com",
                      nonce="dcd98b7102dd2f0e8b11d0f600bfb0c093",
                      uri="/dir/index.html",
                      qop=auth,
                      nc=00000001,
                      cnonce="0a4f113b",
                      response="6629fae49393a05397450978507c4ef1",
                      opaque="5ccc069c403ebaf9f0171e9517f40e41"
END
                       ))
(define RFC-Example-alist
  '([username . "Mufasa"]
    [realm . "testrealm@host.com"]
    [nonce . "dcd98b7102dd2f0e8b11d0f600bfb0c093"]
    [uri . "/dir/index.html"]
    [qop . "auth"]
    [nc . "00000001"]
    [cnonce . "0a4f113b"]
    [response . "6629fae49393a05397450978507c4ef1"]
    [opaque . "5ccc069c403ebaf9f0171e9517f40e41"]))

(define Safari-Example-bytes
  #"Digest username=\"username\", realm=\"Digest Auth Test: g10971\", nonce=\"MTIzMzc2ODU3NCA4MjA2MTAyMDNhYzYyYTRiMTdmOTY4NzVjOWI1MzEwOQ==\", uri=\"/servlets/standalone.ss\", response=\"c3e45e8499e37bf0872930b35fcae291\", cnonce=\"94db38ffd6e360db658e0dbcbf5e43b9\", nc=00000001, qop=\"auth\"")
(define Safari-Example-alist
  '([username . "username"]
    [realm . "Digest Auth Test: g10971"]
    [nonce . "MTIzMzc2ODU3NCA4MjA2MTAyMDNhYzYyYTRiMTdmOTY4NzVjOWI1MzEwOQ=="]
    [uri . "/servlets/standalone.ss"]
    [response . "c3e45e8499e37bf0872930b35fcae291"]
    [cnonce . "94db38ffd6e360db658e0dbcbf5e43b9"]
    [nc . "00000001"]
    [qop . "auth"]))

(define (make-req hs)
  (make-request 
   #"GET" (string->url "http://test.com/foo")
   hs
   (delay empty) #f
   "host" 80 "client"))

(define (header->cons h)
  (cons (header-field h) (header-value h)))

(define digest-auth-tests
  (test-suite
   "Digest Authentication"
   
   (test-suite
    "make-digest-auth-header"
    
    (test-case "Field"
               (check-equal? (header-field (make-digest-auth-header "realm" "secret-key" "opaque"))
                             #"WWW-Authenticate"))
    
    (test-case "Value"
               (check-pred (lambda (v)
                             (regexp-match #rx"Digest realm=\"realm\", qop=\"auth\", nonce=\".+\" opaque=\"opaque\"" v))
                           (header-value (make-digest-auth-header "realm" "secret-key" "opaque")))))
   
   (test-suite
    "request->digest-credentials"
    (test-case "Error"
               (check-equal?
                (request->digest-credentials
                 (make-req (list (make-header #"Authorization" #"Basic bar=\"foo\""))))
                #f))
    
    (test-case "RFC Example"
               (check-equal? 
                (request->digest-credentials 
                 (make-req (list (make-header #"Authorization" RFC-Example-bytes))))
                RFC-Example-alist))
    
    (test-case "Safari Example"
               (check-equal? 
                (request->digest-credentials 
                 (make-req 
                  (list 
                   (make-header #"Authorization"
                                Safari-Example-bytes))))
                Safari-Example-alist)))
   
   (test-suite
    "password->digest-HA1"
    (test-case "Simple"
               (check-equal? ((password->digest-HA1 string-append) "username" "realm")
                             #"cdc6d76271d05ba4d7afeecfcb451c21"))
    (test-case "RFC Example"
               (check-equal? ((password->digest-HA1 (lambda (u r) "Circle Of Life")) "Mufasa" "testrealm@host.com")
                             #"939e7578ed9e3c518a452acee763bce9"))
    (test-case "Safari Example"
               (check-equal? ((password->digest-HA1 (lambda (u r) "password")) "username" "Digest Auth Test: g10971")
                             #"663c0814b20c2cdabe8baa309c6d7b82")))
   
   (test-suite
    "make-check-digest-credentials"
    (test-case "RFC Incorrect"
               (check-equal? ((make-check-digest-credentials (lambda (u r) #"939e7578ed9e3c518a452acee76321e9"))
                              "GET" RFC-Example-alist)
                             #f))
    
    (test-case "Error"
               (check-exn exn? 
                          (lambda ()
                            ((make-check-digest-credentials (lambda (u r) #"939e7578ed9e3c518a452acee76321e9"))
                             "GET" 
                             '([realm . "testrealm@host.com"]
                               [nonce . "dcd98b7102dd2f0e8b11d0f600bfb0c093"]
                               [uri . "/dir/index.html"]
                               [qop . "auth"]
                               [nc . "00000001"]
                               [cnonce . "0a4f113b"]
                               [response . "6629fae49393a05397450978507c4ef1"]
                               [opaque . "5ccc069c403ebaf9f0171e9517f40e41"])))))
    
    (test-case "RFC Example"
               (check-equal? ((make-check-digest-credentials (lambda (u r) #"939e7578ed9e3c518a452acee763bce9"))
                              "GET" RFC-Example-alist)
                             #t))
    
    (test-case "Safari Example"
               (check-equal? ((make-check-digest-credentials (lambda (u r) #"663c0814b20c2cdabe8baa309c6d7b82"))
                              "GET" Safari-Example-alist)
                             #t)))
   
   ))
