(module dispatch-server-unit (lib "a-unit.ss")
  (require (lib "tcp-sig.ss" "net")
           (lib "thread.ss")
           (lib "contract.ss")
           (lib "kw.ss"))
  (require "web-server-structs.ss"
           "connection-manager.ss"
           "dispatch-server-sig.ss")
  
  ;; ****************************************  
  (import tcp^ (prefix config: dispatch-server-config^))
  (export dispatch-server^) 
  
  ;; serve: -> -> void
  ;; start the server and return a thunk to shut it down
  (define (serve)
    (define the-server-custodian (make-custodian))
    (start-connection-manager the-server-custodian)
    (parameterize ([current-custodian the-server-custodian]
                   [current-server-custodian the-server-custodian]
                   #;[current-thread-initial-stack-size 3])
      (thread
       (lambda ()
         (run-server config:port 
                     handle-connection
                     #f
                     (lambda (exn)
                       ((error-display-handler) 
                        (format "Connection error: ~a" (exn-message exn))
                        exn))
                     (lambda (p mw re)
                       (tcp-listen p config:max-waiting #t config:listen-ip))
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
    (start-connection-manager server-cust)
    (parameterize ([current-custodian server-cust]
                   [current-server-custodian server-cust])
      (define connection-cust (make-custodian))
      (parameterize ([current-custodian connection-cust])
        (thread
         (lambda ()
           (handle-connection ip op
                              (lambda (ip)
                                (values "127.0.0.1"
                                        "127.0.0.1"))))))))
  
  ;; handle-connection : input-port output-port (input-port -> string string) -> void
  ;; returns immediately, spawning a thread to handle
  (define/kw (handle-connection ip op
                                #:optional
                                [port-addresses tcp-addresses])
    (define conn
      (new-connection config:initial-connection-timeout
                      ip op (current-custodian) #f))
    (let connection-loop ()
      #;(printf "C: ~a~n" (connection-id conn))
      (let-values ([(req close?) (config:read-request conn config:port port-addresses)])
        (set-connection-close?! conn close?)
        (config:dispatch conn req)
        (if (connection-close? conn)
            (kill-connection! conn)
            (connection-loop))))))