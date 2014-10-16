#lang racket
(require openssl
         rackunit)

;; Test DHE, ECDHE

(define pem (build-path (collection-path "openssl") "test.pem"))

(define (test-ephemeral server-enable! client-ciphers)
  (let-values ([(r1 w2) (make-pipe 10)]
               [(r2 w1) (make-pipe 10)])
    (define server-thread
      (thread (lambda ()
                (define server-ctx (ssl-make-server-context 'tls12))
                (ssl-load-certificate-chain! server-ctx pem)
                (ssl-load-private-key! server-ctx pem)
                (server-enable! server-ctx)
                (define-values (r w)
                  (ports->ssl-ports r2 w2
                                    #:context server-ctx
                                    #:mode 'accept 
                                    #:close-original? #t
                                    #:shutdown-on-close? #t))
                (check-equal? (read-bytes 5 r) #"abcde")
                (check-equal? (write-string "hello" w) 5)
                (close-output-port w))))
    (define client-ctx (ssl-make-client-context 'tls12))
    ;; Set client to only accept ephemeral ciphers
    (ssl-set-ciphers! client-ctx client-ciphers)
    (define-values (r w)
      (ports->ssl-ports r1 w1
                        #:context client-ctx
                        #:mode 'connect 
                        #:close-original? #t
                        #:shutdown-on-close? #t))
    (file-stream-buffer-mode w 'none)
    (check-equal? (write-bytes #"abcde" w) 5)
    (flush-output w)
    (check-equal? (read-string 5 r) "hello")
    (check-equal? (read-string 5 r) eof)
    (close-input-port r)
    (close-output-port w)
    (thread-wait server-thread)
    (void)))

(cond
 [(memq 'tls12 (supported-server-protocols))
  ;; Test DHE ciphers (note: cipher spec is "EDH", contrary to openssl ciphers docs)
  (test-ephemeral (lambda (server-ctx)
                    (ssl-server-context-enable-dhe! server-ctx ssl-dh4096-param-path))
                  "AES+EDH")

  ;; Test ECDHE ciphers
  (test-ephemeral (lambda (server-ctx)
                    (ssl-server-context-enable-ecdhe! server-ctx 'secp192k1))
                  "ECDHE-RSA-AES128-SHA256")

  ;; Sanity check for DHE: connection fails when enable! not called
  (check-exn
   #rx"connect failed"
   (lambda ()
     ;; for DrDr, suppress accept error printing
     (parameterize ((current-error-port (open-output-string)))
       (test-ephemeral void "AES+EDH"))))

  ;; Sanity check for ECDHE: connection fails when enable! not called
  (check-exn
   #rx"connect failed"
   (lambda ()
     ;; for DrDr, suppress accept error printing
     (parameterize ((current-error-port (open-output-string)))
       (test-ephemeral void "ECDHE-RSA-AES128-SHA256"))))]
 [else
  (printf "TLS 1.2 not supported; skipping tests\n")])

