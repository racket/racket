#lang racket/base

(require net/dns rackunit)
(require net/private/rr-srv)

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
  (check-equal? (ip->in-addr.arpa (ipv4 (bytes 8 8 8 8)))
                "8.8.8.8.in-addr.arpa")
  (check-equal? (ip->in-addr.arpa (ipv4 (bytes 127 0 0 1)))
                "1.0.0.127.in-addr.arpa")
  (check-equal? (ip->ip6.arpa (make-ip-address "4321:0:1:2:3:4:567:89ab"))
                (string-append "b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0"
                               ".2.0.0.0.1.0.0.0.0.0.0.0.1.2.3.4.ip6.arpa"))
  (check-equal? (ip->ip6.arpa (make-ip-address "2001:db8::567:89ab"))
                (string-append "b.a.9.8.7.6.5.0.0.0.0.0.0.0.0.0"
                               ".0.0.0.0.0.0.0.0.8.b.d.0.1.0.0.2.ip6.arpa"))
        ;; FIXME: may need to change if SOA handling in net/dns
        ;;        changes (see FIXME there)
  (check-equal? (parse-mx-response mx-ans mx-nss mx-response-with-cname "stat.ethz.ch")
                "stat.ethz.ch"))

;; Constants for testing. These can go out of sync
;; when server setups change, so keep them up-to-date.
(define *google-dns*   "8.8.8.8")
(define *google-dns-2* "8.8.4.4")
(define *nwu-url*      "eecs.northwestern.edu")
(define *nwu-ips*       '("129.105.8.227" "129.105.5.15" "129.105.8.237"))
(define *racket-host*  "winooski.ccs.neu.edu")
(define *racket-ip*    "129.10.115.117")
(define *nwu-mx*       '("cuda.eecs.northwestern.edu" "barra.eecs.northwestern.edu"))
(define *kame-url*     "www.kame.net")
(define *kame-ip*      "2001:200:dff:fff1:216:3eff:feb1:44d7")
(define *xmpp-client*  (srv-rr 0 0 5222 "xmpp.racket-lang.org"))

(define (nameserver-tests nameserver)
  (check member (dns-get-address nameserver *nwu-url*) *nwu-ips*)
  (check-equal? (dns-get-address nameserver *racket-host*) *racket-ip*)
  (check-equal? (dns-get-address nameserver *kame-url* #:ipv6? #t) *kame-ip*)
  (check-equal? (dns-get-name nameserver *racket-ip*) *racket-host*)
  (check member (dns-get-mail-exchanger nameserver *nwu-url*) *nwu-mx*)
  (check-equal? (dns-get-srv nameserver "racket-lang.org" "xmpp-client") (list *xmpp-client*))
  (check-equal? (dns-get-srv nameserver "nonexistent-srv-record.racket-lang.org" "xmpp-client")
                '()))

(define (srv-tests)
  (define srv-xmpp-8020-srv-rr
    '((#"_xmpp-client._tcp.leastfixedpoint.com" srv in 62917
       (0 0
        0 0
        20 102
        4 120 109 112 112 13 101 105 103 104 116 121 45 116 119 101 110 116 121 3 111 114 103 0))))
  (define srv-xmpp-8020-reply
    '(0 185 129 128
      0 1
      0 1
      0 3
      0 0
      12 95 120 109 112 112 45 99 108 105 101 110 116 4 95 116 99 112 15 108
      101 97 115 116 102 105 120 101 100 112 111 105 110 116 3 99 111 109 0 0
      33 0 1 192 12 0 33 0 1 0 1 7 160 0 30 0 0 0 0 20 102 4 120 109 112 112
      13 101 105 103 104 116 121 45 116 119 101 110 116 121 3 111 114 103 0 192
      30 0 2 0 1 0 1 51 30 0 13 1 98 2 110 115 5 106 111 107 101 114 192 46 192
      30 0 2 0 1 0 1 51 30 0 4 1 99 192 111 192 30 0 2 0 1 0 1 51 30 0 4 1 97 192 111))
  (check-equal? (parse-srv-rr srv-xmpp-8020-srv-rr srv-xmpp-8020-reply)
                (list (srv-rr 0 0 5222 "xmpp.eighty-twenty.org"))))

(provide tests)
(module+ main (tests))
(define (tests)
  (internal-tests)
  (nameserver-tests *google-dns*)
  (nameserver-tests *google-dns-2*)
  (srv-tests)
  (let ([ns (dns-find-nameserver)]) (when ns (nameserver-tests ns))))

(module+ test (require (submod ".." main))) ; for raco test & drdr
