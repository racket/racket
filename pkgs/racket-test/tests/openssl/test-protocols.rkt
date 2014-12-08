#lang racket
(require openssl
         rackunit
         racket/runtime-path)

;; Test protocol version compatibility
;; In particular, test 'auto doesn't include SSL 3.

(define PROTOCOLS '(auto sslv2 sslv3 tls tls11 tls12))

(define (compatible? client-p server-p)
  (or (eq? client-p server-p)
      (and (eq? client-p 'auto) (memq server-p '(tls tls11 tls12)))
      (and (eq? server-p 'auto) (memq client-p '(tls tls11 tls12)))))

(define pem (build-path (collection-path "openssl") "test.pem"))
(define MSG:C->S "Hello. This is Racket speaking.")
(define MSG:S->C "Yes, this is Racket too. Hello, Racket.")

;; check whether client-p can connect to server-p
;; raises error unless ( succeeds iff expect-ok? )
(define (test-connect client-p server-p expect-ok?)
  (parameterize ((current-custodian (make-custodian)))
    (define-values (r1 w2) (make-pipe 10))
    (define-values (r2 w1) (make-pipe 10))

    (define server-thread
      (thread
       (lambda ()
         (define server-ctx (ssl-make-server-context server-p))
         (ssl-load-certificate-chain! server-ctx pem)
         (ssl-load-private-key! server-ctx pem)
         (define-values (r w)
           (with-handlers ([values
                            (lambda (e)
                              (cond [expect-ok?
                                     (raise e)]
                                    [else
                                     (values #f #f)]))])
             (ports->ssl-ports r2 w2
                               #:context server-ctx
                               #:mode 'accept 
                               #:close-original? #t
                               #:shutdown-on-close? #t)))
         (when (or r w)
           (check-equal? (read-line r) MSG:C->S)
           (fprintf w "~a\n" MSG:S->C)
           (close-output-port w)
           (unless expect-ok?
             (error 'test-connect
                    "should not have worked (accept): ~s connecting to ~s"
                    client-p server-p))))))

    (define client-ctx (ssl-make-client-context client-p))
    (define-values (r w)
      (with-handlers ([values
                       (lambda (e)
                         (cond [expect-ok?
                                (raise e)]
                               [else
                                (values #f #f)]))])
        (ports->ssl-ports r1 w1
                          #:context client-ctx
                          #:mode 'connect 
                          #:close-original? #t
                          #:shutdown-on-close? #t)))
    (when (or r w)
      (fprintf w "~a\n" MSG:C->S)
      (flush-output w)
      (check-equal? (read-line r) MSG:S->C)
      (check-equal? (read-byte r) eof)
      (close-input-port r)
      (close-output-port w)
      (unless expect-ok?
        (custodian-shutdown-all (current-custodian))
        (error 'test-connect "should not have worked (connect): ~s connecting to ~s"
               client-p server-p)))
    (custodian-shutdown-all (current-custodian))
    (void)))

(for ([client-p PROTOCOLS]
      #:when (memq client-p (supported-client-protocols)))
  (for ([server-p PROTOCOLS]
        #:when (memq server-p (supported-server-protocols)))
    (define ok? (compatible? client-p server-p))
    (printf "** Testing ~s connecting to ~s (expect ~a)\n"
            client-p server-p (if ok? "ok" "fail"))
    (test-case (format "~s connecting to ~s (expect ~a)"
                       client-p server-p (if ok? "ok" "fail"))
      (test-connect client-p server-p ok?))))

(for ([client-p PROTOCOLS])
  (unless (memq client-p (supported-client-protocols))
    (printf "** Skipped unsupported client protocol ~s\n" client-p)))

(for ([server-p PROTOCOLS])
  (unless (memq server-p (supported-server-protocols))
    (printf "** Skipped unsupported server protocol ~s\n" server-p)))
