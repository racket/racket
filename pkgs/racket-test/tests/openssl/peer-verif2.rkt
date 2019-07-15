#lang racket/base
(require openssl
         ffi/unsafe
         racket/tcp
         racket/runtime-path)

(define PORT 55001)

(define (check fmt got expect)
  (unless (equal? got expect)
    (error 'check fmt got)))

(define (check-fail thunk)
  (define s
    (with-handlers ([exn? (lambda (exn) (exn-message exn))])
      (thunk)
      "success"))
  (unless (regexp-match?  #rx"connect failed" s)
    (error 'test "failed: ~s" s)))

(define-runtime-path server-key "server_key.pem")
(define-runtime-path server-crt "server_crt.pem")
(define-runtime-path client-key "client_key.pem")
(define-runtime-path client-crt "client_crt.pem")
(define-runtime-path cacert     "cacert.pem")

(define server-hostname "server.example.com")
(define client-id
  #"/C=US/ST=Racketa/O=Testing Examples/OU=Testing/CN=client.example.com/emailAddress=client@example.com")

(define (call/custodian proc)
  (define cust (make-custodian))
  (parameterize ((current-custodian cust))
    (dynamic-wind void proc (lambda () (custodian-shutdown-all cust)))))

;; test-conn : ServerCtx ClientCtx -> (U #f Bytes)
(define (test-conn server-ctx client-ctx)
  (call/custodian
   (lambda ()
     (define chan (make-channel))
     (define listener (ssl-listen PORT 4 #t "localhost" server-ctx))
     (thread (lambda ()
               (ssl-try-verify! listener #t)
               (define-values (in out) (ssl-accept listener))
               (channel-put chan (and (ssl-peer-verified? in) (ssl-peer-subject-name in)))))
     ;; Use ports->ssl-ports instead of ssl-connect so we can supply a fake hostname.
     ;; (ssl-connect "localhost" PORT client-ctx)
     (define-values (in out) (tcp-connect "localhost" PORT))
     (if (symbol? client-ctx)
         (ports->ssl-ports in out #:mode 'connect #:encrypt client-ctx #:hostname server-hostname)
         (ports->ssl-ports in out #:mode 'connect #:context client-ctx #:hostname server-hostname))
     (channel-get chan))))

(define server-ctx1
  (ssl-make-server-context 'auto #:private-key `(pem ,server-key) #:certificate-chain server-crt))
(define server-ctx2
  (let ([ctx (ssl-make-server-context)])
    (ssl-load-certificate-chain! ctx server-crt)
    (ssl-load-private-key! ctx server-key #f #f)
    ctx))
;; Set roots for server-verifying-client
(parameterize ((ssl-default-verify-sources (list cacert)))
  (ssl-load-default-verify-sources! server-ctx1)
  (ssl-load-default-verify-sources! server-ctx2))

(define client-ctx/standard-trust
  (ssl-secure-client-context))
(define client-ctx/no-trust
  (parameterize ((ssl-default-verify-sources null))
    (ssl-secure-client-context)))

(define client-ctx/trust-ca1
  (parameterize ((ssl-default-verify-sources (list cacert)))
    (ssl-secure-client-context)))

(define client-ctx/trust-ca2
  (parameterize ((ssl-default-verify-sources (list cacert)))
    (ssl-make-client-context 'secure)))

(define client-ctx/auto/cred
  (let ([ctx (ssl-make-client-context 'auto)])
    (ssl-load-private-key! ctx client-key #f #f)
    (ssl-load-certificate-chain! ctx client-crt)
    ctx))

(define client-ctx/trust-ca/cred
  (parameterize ((ssl-default-verify-sources (list cacert)))
    (ssl-make-client-context 'secure
                             #:private-key `(pem ,client-key)
                             #:certificate-chain client-crt)))

(for ([server-ctx (list server-ctx1 server-ctx2)])

  ;; Test that the client fails to verify the server (server's CA not trusted).
  (for ([client-ctx (list client-ctx/standard-trust
                          client-ctx/no-trust)])
    (check-fail (lambda () (test-conn server-ctx client-ctx))))

  ;; Test that the client verifies the server, and the server does not
  ;; get a client identity (no key/cert loaded).
  (for ([client-ctx (list client-ctx/trust-ca1
                          client-ctx/trust-ca2)])
    (check "connection w/o client creds; got ~e"
           (test-conn server-ctx client-ctx)
           #f))

  ;; Test that the client verifies the server, and the server verify
  ;; the client and gets the right client identity.
  (for ([client-ctx (list client-ctx/auto/cred
                          client-ctx/trust-ca/cred)])
    (check "connection with client creds; got ~e"
           (test-conn server-ctx client-ctx)
           client-id))

  ;; Test that an implicit 'secure client context does verification.
  (parameterize ((ssl-default-verify-sources null))
    (check-fail (lambda () (test-conn server-ctx 'secure))))
  (parameterize ((ssl-default-verify-sources null))
    (check-fail (lambda () (test-conn server-ctx 'secure))))
  (parameterize ((ssl-default-verify-sources (list cacert)))
    (check "implicit, got ~e" (test-conn server-ctx 'secure) #f))
  (void))
