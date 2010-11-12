#lang racket/unit
(require net/tcp-sig
         racket/async-channel
         mzlib/thread)
(require "web-server-structs.rkt"
         "connection-manager.rkt"
         "dispatch-server-sig.rkt")

;; ****************************************  
(import tcp^ (prefix config: dispatch-server-config^))
(export dispatch-server^) 

(define (async-channel-put* ac v)
  (when ac
    (async-channel-put ac v)))

;; serve: -> -> void
;; start the server and return a thunk to shut it down
(define (serve #:confirmation-channel [confirmation-channel #f])
  (define the-server-custodian (make-custodian))
  (parameterize ([current-custodian the-server-custodian]
                 [current-server-custodian the-server-custodian]
                 #;[current-thread-initial-stack-size 3])
    (start-connection-manager)
    (thread
     (lambda ()
       (run-server 1 ; This is the port argument, but because we specialize listen, it is ignored. 
                   handle-connection
                   #f
                   (lambda (exn)
                     ((error-display-handler) 
                      (format "Connection error: ~a" (exn-message exn))
                      exn))
                   (lambda (_ mw re)
                     (with-handlers ([exn? 
                                      (λ (x)
                                        (async-channel-put* confirmation-channel x)
                                        (raise x))])
                       (define listener (tcp-listen config:port config:max-waiting #t config:listen-ip))
                       (let-values ([(local-addr local-port end-addr end-port) (tcp-addresses listener #t)])
                         (async-channel-put* confirmation-channel local-port))
                       listener))
                   tcp-close
                   tcp-accept
                   tcp-accept/enable-break))))
  (lambda ()
    (custodian-shutdown-all the-server-custodian)))

;; serve-ports : input-port output-port -> void
;; returns immediately, spawning a thread to handle
;; the connection
;; NOTE: (GregP) should allow the user to pass in a connection-custodian
(define (serve-ports ip op)
  (define server-cust (make-custodian))
  (parameterize ([current-custodian server-cust]
                 [current-server-custodian server-cust])
    (define connection-cust (make-custodian))
    (start-connection-manager)
    (parameterize ([current-custodian connection-cust])
      (thread
       (lambda ()
         (handle-connection ip op
                            (lambda (ip)
                              (values "127.0.0.1"
                                      "127.0.0.1"))))))))

;; handle-connection : input-port output-port (input-port -> string string) -> void
;; returns immediately, spawning a thread to handle
(define (handle-connection ip op
                           #:port-addresses [port-addresses tcp-addresses])
  (define conn
    (new-connection config:initial-connection-timeout
                    ip op (current-custodian) #f))
  (let connection-loop ()
    #;(printf "C: ~a\n" (connection-id conn))
    (let-values ([(req close?) (config:read-request conn config:port port-addresses)])
      (set-connection-close?! conn close?)
      (config:dispatch conn req)
      (if (connection-close? conn)
          (kill-connection! conn)
          (connection-loop)))))
