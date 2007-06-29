(module connection-manager mzscheme
  (require (lib "contract.ss")
           "timer.ss")
  
  (define-struct connection (id timer i-port o-port custodian close?))
  
  (provide/contract
   [struct connection
           ([id integer?]
            [timer timer?]
            [i-port input-port?]
            [o-port output-port?]
            [custodian custodian?]
            [close? boolean?])]
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
  (define i (box 0))
  (define (new-connection time-to-live i-port o-port cust close?)
    (define conn
      (make-connection
       (begin0 (unbox i) (set-box! i (add1 (unbox i))))
       #f i-port o-port cust close?))
    (define conn-wb (make-weak-box conn))
    (set-connection-timer! 
     conn
     (start-timer time-to-live
                  (lambda () 
                    (cond
                      [(weak-box-value conn-wb)
                       => kill-connection!]))))
    conn)
  
  ;; kill-connection!: connection -> void
  ;; kill this connection
  (define (kill-connection! conn)
    #;(printf "K: ~a~n" (connection-id conn))
    ; XXX Don't need to do this when called from timer
    (with-handlers ([exn? void])
      (cancel-timer! (connection-timer conn)))
    (with-handlers ([exn:fail:network? void])
      (close-output-port (connection-o-port conn)))
    (with-handlers ([exn:fail:network? void])
      (close-input-port (connection-i-port conn)))
    (custodian-shutdown-all (connection-custodian conn)))
  
  ;; adjust-connection-timeout!: connection number -> void
  ;; change the expiration time for this connection
  (define (adjust-connection-timeout! conn time)
    (reset-timer! (connection-timer conn) time)))