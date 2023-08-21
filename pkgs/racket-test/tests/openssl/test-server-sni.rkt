#lang racket
(require openssl
         rackunit
         racket/runtime-path)

(define PORT 4438)

(define (make-sctx key-file cert-file)
  (define sctx (ssl-make-server-context 'auto))
  (ssl-load-default-verify-sources! sctx)
  (ssl-set-ciphers! sctx "DEFAULT:!aNULL:!eNULL:!LOW:!EXPORT:!SSLv2")
  (ssl-load-certificate-chain! sctx cert-file)
  (ssl-load-private-key! sctx key-file)
  sctx)

(define-runtime-path lambda-key "server_key.pem")
(define-runtime-path lambda-cert "server_lambda_crt.pem")
(define-runtime-path theultimate-key "server_ultimate_key.pem")
(define-runtime-path theultimate-cert "server_ultimate_crt.pem")

(define lambda-sctx (make-sctx lambda-key lambda-cert))
(define theultimate-sctx (make-sctx theultimate-key theultimate-cert))

(define (callback name)
  (cond [(equal? name "lambda") lambda-sctx]
        [(equal? name "theultimate") theultimate-sctx]
	[else #f]))
(ssl-set-server-name-identification-callback! lambda-sctx callback)
(ssl-set-server-name-identification-callback! theultimate-sctx callback)
(ssl-seal-context! lambda-sctx)
(ssl-seal-context! theultimate-sctx)
(define listener
  (ssl-listen PORT 5 #t #f lambda-sctx))
(void
 (thread
  (lambda ()
    (for ([x (in-naturals)])
      (ssl-accept listener)))))

(define (test-name name)
  (let*-values ([(in out) (tcp-connect "localhost" PORT)]
		[(ssl-in ssl-out)
                 (ports->ssl-ports in out
                                   #:encrypt 'auto
                                   #:hostname name)])
    ;; (printf "testing ~a: ~a~n" name (ssl-peer-certificate-hostnames ssl-in))
    (list (ssl-peer-certificate-hostnames ssl-in)
          (ssl-peer-check-hostname ssl-in name))))

(check-equal? (test-name "theultimate") '(("theultimate") #t))
(check-equal? (test-name "spacebadger") '(("lambda") #f))
(check-equal? (test-name "lambda")      '(("lambda") #t))
(check-equal? (test-name "spacebadger") '(("lambda") #f))
