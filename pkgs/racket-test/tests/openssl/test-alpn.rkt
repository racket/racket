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
