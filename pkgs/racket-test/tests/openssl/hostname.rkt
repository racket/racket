#lang racket
(require openssl/mzssl)

(define-values (r w) (ssl-connect "www.racket-lang.org" 443))
(define hostnames (ssl-peer-certificate-hostnames r))

(unless (and (list? hostnames)
             (andmap string? hostnames))
  (error "not a good hostnames result"))

(when (null? hostnames)
  (error "expected some hostnames"))
