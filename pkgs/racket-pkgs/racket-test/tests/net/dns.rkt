#lang racket/base

(require net/dns tests/eli-tester)

;; internal tests

(define mx-response-with-cname
  '(;; random ID
    0 187
    ;; QR Opcode  AA TC RD RA Z     RCODE
    ;; 1  0 0 0 0 0  0  1  1  0 0 0 0 0 0 0
    129 128
    ;; QDCOUNT
    0 1
    ;; ANSCOUNT
    0 1
    ;; NSCOUNT
    0 1
    ;; ARCOUNT
    0 0
    ;; QNAME (stat.ethz.ch)
    4 115 116 97 116
    4 101 116 104 122
    2 99 104 0
    ;; QTYPE (MX)
    0 15
    ;; QCLASS (IN)
    0 1
    ;; NAME (pointer)
    ;; 1100000000001100
    192 12
    ;; TYPE: CNAME
    0 5
    ;; CLASS
    0 1
    ;; TTL
    0 0
    4 79
    ;; RDLENGTH
    0 19
    ;; RDATA
    11 109 97 103 101 108 108 97 110 45 48 54
    4 109 97 116 104
    192 17
    ;; NAME (pointer)
    192 17
    ;; TYPE: SOA
    0 6
    ;; CLASS
    0 1
    ;; TTL
    0 0 1 90
    ;; RDLENGTH & DATA
    0 44 8 100 117 109 109 121 45
    110 115 192 17 10 104 111 115 116 109
    97 115 116 101 114 192 17 119 168 176
    33 0 0 42 48 0 0 14 16 0 27 175 128 0 0 2 88))

(require (only-in rackunit require/expose) net/private/ip)
(require/expose net/dns (ip->in-addr.arpa ip->ip6.arpa
                         parse-reply parse-mx-response))

(define-values (_1 _2 mx-ans mx-nss _3 _4)
  (parse-reply '(0 187 129 128) mx-response-with-cname))

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
                          ".0.0.0.0.0.0.0.0.8.b.d.0.1.0.0.2.ip6.arpa")
        ;; FIXME: may need to change if SOA handling in net/dns
        ;;        changes (see FIXME there)
        (parse-mx-response mx-ans mx-nss mx-response-with-cname "stat.ethz.ch")
        => "stat.ethz.ch"))

;; Constants for testing. These can go out of sync
;; when server setups change, so keep them up-to-date.
(define *google-dns*   "8.8.8.8")
(define *google-dns-2* "8.8.4.4")
(define *racket-url*   "racket-lang.org")
(define *racket-host*  "winooski.ccs.neu.edu")
(define *racket-ip*    "129.10.115.117")
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

(module+ test (require (submod ".." main))) ; for raco test & drdr
