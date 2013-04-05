#lang racket/base

(require net/dns tests/eli-tester)

;; internal tests
(require (only-in rackunit require/expose) net/private/ip)
(require/expose net/dns (ip->in-addr.arpa ip->ip6.arpa))
(define (internal-tests)
  (test (ip->in-addr.arpa (ipv4 (bytes 8 8 8 8)))
        => "8.8.8.8.in-addr.arpa"
        (ip->in-addr.arpa (ipv4 (bytes 127 0 0 1)))
        => "1.0.0.127.in-addr.arpa"
        (ip->ip6.arpa (make-ip-address "4321:0:1:2:3:4:567:89ab"))
        => (string-append "b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0"
                          ".2.0.0.0.1.0.0.0.0.0.0.0.1.2.3.4.ip6.arpa")
        (ip->ip6.arpa (make-ip-address "2001:db8::567:89ab"))
        => (string-append "b.a.9.8.7.6.5.0.0.0.0.0.0.0.0.0"
                          ".0.0.0.0.0.0.0.0.8.b.d.0.1.0.0.2.ip6.arpa")))

;; Constants for testing. These can go out of sync
;; when server setups change, so keep them up-to-date.
(define *google-dns*   "8.8.8.8")
(define *google-dns-2* "8.8.4.4")
(define *racket-url*   "racket-lang.org")
(define *racket-host*  "champlain.ccs.neu.edu")
(define *racket-ip*    "129.10.115.116")
(define *racket-mx*    "aspmx.l.google.com")
(define *kame-url*     "www.kame.net")
(define *kame-ip*      "2001:200:dff:fff1:216:3eff:feb1:44d7")

(define (nameserver-tests nameserver)
  (test (dns-get-address nameserver *racket-url*) => *racket-ip*
        (dns-get-address nameserver *racket-host*) => *racket-ip*
        (dns-get-address nameserver *kame-url* #:ipv6? #t) => *kame-ip*
        (dns-get-name nameserver *racket-ip*) => *racket-host*
        (dns-get-mail-exchanger nameserver *racket-url*) => *racket-mx*))

(provide tests)
(module+ main (tests))
(define (tests)
  (test do (internal-tests)
           (nameserver-tests *google-dns*)
           (nameserver-tests *google-dns-2*)
           (let ([ns (dns-find-nameserver)]) (when ns (nameserver-tests ns)))))
