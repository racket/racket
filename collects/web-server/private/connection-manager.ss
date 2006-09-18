;; this is a trivial implementation of the connection-manger interface that
;; uses timeouts instead of a queued-model.

;; the queued-model is also fully implemented but won't be used at this time.
(module connection-manager mzscheme
  (require "connection-structs.ss"
           "timer.ss"
           (lib "contract.ss"))
  (provide (all-from "connection-structs.ss"))

  (provide/contract
   [start-connection-manager (custodian? . -> . void)]
   [new-connection (number? input-port? output-port? custodian? boolean? . -> . connection?)]
   [kill-connection! (connection? . -> . void)]
   [adjust-connection-timeout! (connection? number? . -> . void)])
  
  ;; start-connection-manager: custodian -> void
  ;; calls the timer manager
  (define (start-connection-manager custodian)
    (start-timer-manager custodian))
  
  ;; new-connection: number i-port o-port custodian -> connection
  ;; ask the connection manager for a new connection
  (define (new-connection time-to-live i-port o-port cust close?)
    (letrec ([conn
              (make-connection
               (start-timer time-to-live
                            (lambda () (kill-connection! conn)))
               i-port o-port cust close?
               (make-semaphore 1))])
      conn))
  
  ;; kill-connection!: connection -> void
  ;; kill this connection
  (define (kill-connection! conn-demned)
    (cancel-timer! (connection-timer conn-demned))
    (with-handlers ([exn:fail:network? void])
      (close-output-port (connection-o-port conn-demned)))
    (with-handlers ([exn:fail:network? void])
      (close-input-port (connection-i-port conn-demned)))
    (set-connection-close?! conn-demned #t)
    (custodian-shutdown-all (connection-custodian conn-demned)))
  
  ;; adjust-connection-timeout!: connection number -> void
  ;; change the expiration time for this connection
  (define (adjust-connection-timeout! conn time)
    (reset-timer! (connection-timer conn) time)))