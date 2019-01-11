#lang racket

(require openssl
         ;"../../../../racket/collects/openssl/mzssl.rkt"
         rackunit)

;; not sure this is ideal for pem, but copied from "test-protocols.rkt"
(define pem (build-path (collection-path "openssl") "test.pem")) 
(define MSG:C->S "Hello. This is Racket speaking.")
(define MSG:S->C "Yes, this is Racket too. Hello, Racket.")

(define (negotiate-ciphers #:server server-cipher-spec
                           #:client client-cipher-spec
                           #:use-server-pref? use-server-pref?)
  (define cust (make-custodian))
  (dynamic-wind
   void
   (λ () 
     (parameterize ([current-custodian cust])
       (define-values (r1 w2) (make-pipe 10))
       (define-values (r2 w1) (make-pipe 10))
       (define server-thread
         (thread
          (λ ()
            (define server-ctx (ssl-make-server-context 'tls))
            (ssl-load-certificate-chain! server-ctx pem)
            (ssl-load-private-key! server-ctx pem)
            (ssl-set-ciphers! server-ctx server-cipher-spec)
            (when use-server-pref?
              (ssl-server-context-use-server-cipher-preference! server-ctx))
            (define-values (r w)
              (ports->ssl-ports r2 w2
                                #:context server-ctx
                                #:mode 'accept 
                                #:close-original? #t
                                #:shutdown-on-close? #t))
            (check-equal? (read-line r) MSG:C->S "message from client")
            (fprintf w "~a\n" MSG:S->C)
            (close-output-port w))))
       (define client-ctx (ssl-make-client-context 'tls))
       (ssl-set-ciphers! client-ctx client-cipher-spec)
       (define-values (r w)
         (ports->ssl-ports r1 w1
                           #:context client-ctx
                           #:mode 'connect 
                           #:close-original? #t
                           #:shutdown-on-close? #t))
       (fprintf w "~a\n" MSG:C->S)
       (flush-output w)
       (define chosen-name (ssl-get-cipher-name r))
       (define chosen-version (ssl-get-cipher-version w))
       (check-equal? (read-line r) MSG:S->C "reply from server")
       (check-equal? (read-byte r) eof "server should have closed port")
       (list chosen-name chosen-version)))
   (λ () (custodian-shutdown-all cust))))
  


(define c1
  ;; aka TLS_RSA_WITH_AES_128_CBC_SHA
  "AES128-SHA")
(define c2
  ;; aka TLS_RSA_WITH_AES_256_CBC_SHA
  "AES256-SHA")
(define c3
  ;; aka TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA
  "ECDHE-RSA-AES128-SHA")
(define c4
  ;; aka TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA
  "ECDHE-RSA-AES256-SHA")

(define c2-c4 (string-append c2 ":" c4))
(define c4-c2 (string-append c4 ":" c2))

(negotiate-ciphers #:server c2-c4
                   #:client c4-c2
                   #:use-server-pref? #t)

(negotiate-ciphers #:server c4-c2
                   #:client c2-c4
                   #:use-server-pref? #t)

(negotiate-ciphers #:server c2-c4
                   #:client c4-c2
                   #:use-server-pref? #f)

(negotiate-ciphers #:server c4-c2
                   #:client c2-c4
                   #:use-server-pref? #f)


(define c1-c2 (string-append c1 ":" c2))
(define c2-c1 (string-append c2 ":" c1))
  
(negotiate-ciphers #:server c1-c2
                   #:client c2-c1
                   #:use-server-pref? #t)

(negotiate-ciphers #:server c2-c1
                   #:client c1-c2
                   #:use-server-pref? #t)

(negotiate-ciphers #:server c1-c2
                   #:client c2-c1
                   #:use-server-pref? #f)

(negotiate-ciphers #:server c2-c1
                   #:client c1-c2
                   #:use-server-pref? #f)

