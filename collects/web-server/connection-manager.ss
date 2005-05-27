;; this is an trivial implementation of the connection-manger interface that
;; uses timeouts instead of a queued-model.

;; the queued-model is also fully implemented but won't be used at this time.
(module connection-manager mzscheme
  (require "timer.ss"
           (lib "contract.ss"))

  (define-struct connection (timer i-port o-port custodian close?)
    (make-inspector))

  (provide/contract
   [struct connection
           ([timer timer?]
            [i-port input-port?] [o-port output-port?] [custodian custodian?]
            [close? boolean?])]
   [start-connection-manager (custodian? . -> . void)]
   [new-connection (number? input-port? output-port? custodian? boolean? . -> . connection?)]
   [kill-connection! (connection? . -> . void)]
   [adjust-connection-timeout! (connection? number? . -> . void)])

  ;; start-connection-manager: custodian -> void
  ;; does nothing
  (define start-connection-manager void)

  ;; new-connection: number i-port o-port custodian -> connection
  ;; ask the connection manager for a new connection
  (define (new-connection time-to-live i-port o-port cust close?)
    (make-connection
     (start-timer time-to-live
                  (lambda () 
                    (close-output-port o-port)
                    (close-input-port i-port)
                    (custodian-shutdown-all cust)))
     i-port o-port cust close?))

  ;; kill-connection!: connection -> void
  ;; kill this connection
  (define (kill-connection! conn-demned)
    (close-output-port (connection-o-port conn-demned))
    (close-input-port (connection-i-port conn-demned))
    (custodian-shutdown-all (connection-custodian conn-demned)))

  ;; adjust-connection-timeout!: connection number -> void
  ;; change the expiration time for this connection
  (define (adjust-connection-timeout! conn time)
    (reset-timer (connection-timer conn) time))
  )
