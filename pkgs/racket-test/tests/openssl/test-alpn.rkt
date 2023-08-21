#lang racket/base
(require openssl
         rackunit
         racket/tcp
         racket/runtime-path
         racket/system)

;; Tests for ALPN client support

(define-runtime-path server-key "server_key.pem")
(define-runtime-path server-crt "server_crt.pem")

;; server listens on localhost:PORT+counter
;; (need to change port, otherwise get "Address still in use")
(define PORT 4493)

;; Set up server
(define server-cust (make-custodian))
(define srvout (open-output-string))
(define-values (_srvout _srvin srvpid _srverr srvctl)
  (parameterize ((current-custodian server-cust)
                 (current-subprocess-custodian-mode 'kill))
    (apply values
           (process* "/usr/bin/openssl" "s_server"
                     "-accept" (number->string PORT)
                     "-cert" server-crt "-key" server-key
                     "-alpn" "rkt-proto,other"))))
(sleep 0.2) ;; wait for server to bind the port

(let ()
  ;; Check no protocol is selected if none is requested.
  (define-values (in out)
    (ssl-connect "localhost" PORT))
  (check-equal? (ssl-get-alpn-selected in) #f)
  (check-equal? (ssl-get-alpn-selected out) #f)
  (begin (close-input-port in) (close-output-port out)))

(let ()
  ;; Check supported protocol is selected.
  (define-values (in out)
    (ssl-connect "localhost" PORT #:alpn '(#"rkt-proto")))
  (check-equal? (ssl-get-alpn-selected in) #"rkt-proto")
  (check-equal? (ssl-get-alpn-selected out) #"rkt-proto")
  (begin (close-input-port in) (close-output-port out)))

(let ()
  ;; Check unsupported protocol is not selected.
  (define-values (in out)
    (ssl-connect "localhost" PORT #:alpn '(#"unsupported" #"rkt-proto")))
  (check-equal? (ssl-get-alpn-selected in) #"rkt-proto")
  (check-equal? (ssl-get-alpn-selected out) #"rkt-proto")
  (begin (close-input-port in) (close-output-port out)))

;; Don't test case when client requests only unsupported protocols,
;; because there are buggy versions of OpenSSL that accept the
;; connection without setting a protocol. (Spec says connection
;; should fail.)

(custodian-shutdown-all server-cust)

;; ----------------------------------------

(define sctx (ssl-make-server-context))
(ssl-load-private-key! sctx server-key)
(ssl-load-certificate-chain! sctx server-crt)
(ssl-set-server-alpn! sctx '(#"rkt-proto" #"other") #t)
(define listener (ssl-listen PORT 5 #f #f sctx))

(define server2-cust (make-custodian))
(define server2-chan (make-channel))
(define (server2)
  (define-values (in out) (ssl-accept listener))
  (channel-put server2-chan (ssl-get-alpn-selected in))
  (close-input-port in)
  (close-output-port out)
  (server2))
(parameterize ((current-custodian server2-cust))
  (void (thread server2)))
(sleep 0.2)

(let ()
  ;; Shared proto is selected.
  (define-values (in out)
    (ssl-connect "localhost" PORT #:alpn '(#"unsupported" #"rkt-proto")))
  (check-equal? (ssl-get-alpn-selected in) #"rkt-proto")
  (check-equal? (channel-get server2-chan) #"rkt-proto"))

(let ()
  ;; Server selects shared proto according to its priorities.
  (define-values (in out)
    (ssl-connect "localhost" PORT #:alpn '(#"other" #"rkt-proto")))
  (check-equal? (ssl-get-alpn-selected in) #"rkt-proto")
  (check-equal? (channel-get server2-chan) #"rkt-proto"))

(let ()
  ;; Client w/o ALPN can still connect.
  (define-values (in out) (ssl-connect "localhost" PORT))
  (check-equal? (ssl-get-alpn-selected in) #f)
  (check-equal? (channel-get server2-chan) #f))

(let ()
  ;; Client w/ no shared ALPN protocols can still connect.  It's not
  ;; clear whether this is allowed by RFC 7301, but this behavior is
  ;; popular and seems beneficial. See discussions here:
  ;; - https://github.com/openssl/openssl/pull/2570
  ;; - https://bugs.chromium.org/p/chromium/issues/detail?id=497770
  ;; - https://github.com/postmanlabs/httpbin/issues/497
  (define-values (in out)
    (ssl-connect "localhost" PORT #:alpn '(#"stuff" #"nonsense")))
  (check-equal? (ssl-get-alpn-selected in) #f)
  (check-equal? (channel-get server2-chan) #f))

(custodian-shutdown-all server2-cust)
