#lang racket/base

(require net/dns tests/eli-tester)

;; Run internal unit tests
(require (submod net/dns test))

;; Constants for testing. These can go out of sync
;; when server setups change, so keep them up-to-date.
(define *google-dns* "8.8.8.8")
(define *google-dns-2* "8.8.4.4")
(define *racket-url* "racket-lang.org")
(define *racket-host* "champlain.ccs.neu.edu")
(define *racket-ip* "129.10.115.116")
(define *racket-mx* #"aspmx.l.google.com")
(define *kame-url* "www.kame.net")
(define *kame-ip* "2001:200:dff:fff1:216:3eff:feb1:44d7")

(module+ main (tests))
(define (dns-test/nameserver nameserver)
  (test (dns-get-address nameserver *racket-url*) => *racket-ip*
        (dns-get-address nameserver *racket-host*) => *racket-ip*
        (dns-get-address nameserver *kame-url* #:ipv6? #t) => *kame-ip*
        (dns-get-name nameserver *racket-ip*) => *racket-host*
        (dns-get-mail-exchanger nameserver *racket-url*) => *racket-mx*))

(define (tests)
  (dns-test/nameserver *google-dns*)
  (dns-test/nameserver *google-dns-2*)

  (define ns (dns-find-nameserver))
  (when ns
    (dns-test/nameserver ns)))

