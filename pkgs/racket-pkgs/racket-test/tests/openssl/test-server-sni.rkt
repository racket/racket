#lang racket
(require openssl
         rackunit
         racket/runtime-path)

(define (make-sctx pem)
  (define sctx (ssl-make-server-context 'tls))
  (ssl-load-default-verify-sources! sctx)
  (ssl-set-ciphers! sctx "DEFAULT:!aNULL:!eNULL:!LOW:!EXPORT:!SSLv2")
  (ssl-load-certificate-chain! sctx pem)
  (ssl-load-private-key! sctx pem)
  sctx)

(define-runtime-path test-pem '(lib "openssl/test.pem"))
(define-runtime-path test2-pem "sni-test2.pem")

(define lambda-sctx (make-sctx test-pem))
(define theultimate-sctx (make-sctx test2-pem))

(define (callback name)
  (cond [(equal? name "lambda") lambda-sctx]
        [(equal? name "theultimate") theultimate-sctx]
	[else #f]))
(ssl-set-server-name-identification-callback! lambda-sctx callback)
(ssl-set-server-name-identification-callback! theultimate-sctx callback)
(ssl-seal-context! lambda-sctx)
(ssl-seal-context! theultimate-sctx)
(define listener
  (ssl-listen 4433 5 #t #f lambda-sctx))
(void
 (thread
  (lambda ()
    (for ([x (in-naturals)])
      (ssl-accept listener)))))

(define (test-name name)
  (let*-values ([(in out) (tcp-connect "localhost" 4433)]
		[(ssl-in ssl-out)
                 (ports->ssl-ports in out
                                   #:encrypt 'tls
                                   #:hostname name)])
    ;; (printf "testing ~a: ~a~n" name (ssl-peer-certificate-hostnames ssl-in))
    (list (ssl-peer-certificate-hostnames ssl-in)
          (ssl-peer-check-hostname ssl-in name))))

(check-equal? (test-name "theultimate") '(("theultimate") #t))
(check-equal? (test-name "spacebadger") '(("lambda") #f))
(check-equal? (test-name "lambda")      '(("lambda") #t))
(check-equal? (test-name "spacebadger") '(("lambda") #f))
