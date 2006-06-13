(module web-server-unit mzscheme
  (require "sig.ss"
           "web-server-structs.ss"
           "connection-manager.ss"
           "configuration-structures.ss"
           "servlet.ss"
           "private/cache-table.ss"
           (rename "private/request.ss" 
                   the-read-request read-request))
  (require (prefix sequencer: "dispatchers/dispatch-sequencer.ss")
           (prefix passwords: "dispatchers/dispatch-passwords.ss")
           (prefix files: "dispatchers/dispatch-files.ss")
           (prefix servlets: "dispatchers/dispatch-servlets.ss")
           (prefix path-procedure: "dispatchers/dispatch-pathprocedure.ss")
           (prefix log: "dispatchers/dispatch-log.ss")
           (prefix host: "dispatchers/dispatch-host.ss"))
  (require (lib "tcp-sig.ss" "net")
           (lib "unitsig.ss")
           (lib "string.ss")
           (lib "list.ss")
           (lib "url.ss" "net"))
  
  (provide web-server@)  
  
  ;; ****************************************  
  (define dispatch-server@
    (unit/sig dispatch-server^
      (import net:tcp^ (config : dispatch-server-config^))
      
      ;; serve: -> -> void
      ;; start the server and return a thunk to shut it down
      (define (serve)
        (define the-server-custodian (make-custodian))
        (start-connection-manager the-server-custodian)
        (parameterize ([current-custodian the-server-custodian]
                       [current-server-custodian the-server-custodian]
                       [current-thread-initial-stack-size 3])
          (thread
           (lambda ()
             (start-listener))))
        (lambda ()
          (custodian-shutdown-all the-server-custodian)))
      
      ;; start-listener : -> void
      ;; loops around starting a listener if the current listener dies
      (define (start-listener)
        (define listener 
          (tcp-listen config:port config:max-waiting
                      #t config:listen-ip))
        (define get-ports
          (lambda () (tcp-accept listener)))
        (with-handlers ([void (lambda (e)
                                ; If the exception did not kill the listener
                                (with-handlers ([void void])
                                  (tcp-close listener))
                                ; Rethrow the error to this thread's error printer
                                (raise e))])
          (server-loop get-ports
                       tcp-addresses)))
      
      ;; server-loop: (-> input-port output-port) (input-port -> string string) -> void
      ;; start a thread to handle each incoming connection
      (define (server-loop get-ports port-addresses)
        (let loop ()
          (define connection-cust (make-custodian))
          (parameterize ([current-custodian connection-cust])
            (define-values (ip op) (get-ports))
            (serve-ports/inner ip op
                               port-addresses))
          (loop)))
      
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
            (serve-ports/inner ip op
                               (lambda (ip)
                                 (values "127.0.0.1"
                                         "127.0.0.1"))))))
      
      ;; serve-ports/inner : input-port output-port (input-port -> string string) -> void
      ;; returns immediately, spawning a thread to handle
      (define (serve-ports/inner ip op port-addresses)
        (thread
         (lambda ()
           (define conn
             (new-connection config:initial-connection-timeout
                             ip op (current-custodian) #f))
           (with-handlers ([exn:fail:network?
                            (lambda (e)
                              (kill-connection! conn)
                              (raise e))])
             (serve-connection conn port-addresses)))))
      
      ;; serve-connection: connection (input-port -> string string) -> void
      ;; respond to all requests on this connection
      (define (serve-connection conn port-addresses)
        (let connection-loop ()
          (define-values (req close?) (config:read-request conn config:port port-addresses))
          (unless close?
            (set-connection-close?! conn #f))
          (adjust-connection-timeout! conn config:initial-connection-timeout)
          (config:dispatch conn req)
          (when close?
            (set-connection-close?! conn #t))
          (cond
            [(connection-close? conn) (kill-connection! conn)]
            [else (connection-loop)])))))
  
  (define web-config@->dispatch-server-config@
    (unit/sig dispatch-server-config^
      (import (config : web-config^))
      (define read-request the-read-request)
      
      (define port config:port)
      (define listen-ip config:listen-ip)
      (define max-waiting config:max-waiting)
      (define initial-connection-timeout config:initial-connection-timeout)
      
      ;; dispatch : connection request -> void
      (define dispatch-cache (make-cache-table))
      (define dispatch 
        (host:make
         (lambda (host)
           (cache-table-lookup!
            dispatch-cache host
            (lambda ()
              (parameterize ([current-custodian (current-server-custodian)])
                (host-info->dispatcher
                 (config:virtual-hosts (symbol->string host)))))))))
      
      ;; host-info->dispatcher : host-info -> conn request -> void
      (define (host-info->dispatcher host-info)
        (sequencer:make
         (log:make #:log-format (host-log-format host-info)
                   #:log-path (host-log-path host-info))
         (passwords:make #:password-file (host-passwords host-info)
                         #:password-connection-timeout (timeouts-password (host-timeouts host-info))
                         #:authentication-responder (responders-authentication (host-responders host-info))
                         #:passwords-refresh-responder (responders-passwords-refreshed (host-responders host-info)))
         (path-procedure:make "/conf/collect-garbage"
                              (lambda ()
                                (collect-garbage)
                                ((responders-collect-garbage (host-responders host-info)))))
         (servlets:make config:instances config:scripts config:make-servlet-namespace
                        #:servlet-root (paths-servlet (host-paths host-info))
                        #:responders-servlets-refreshed (responders-servlets-refreshed (host-responders host-info))
                        #:responders-servlet-loading (responders-servlet-loading (host-responders host-info))
                        #:responders-servlet (responders-servlet (host-responders host-info))
                        #:responders-file-not-found (responders-file-not-found (host-responders host-info))
                        #:timeouts-servlet-connection (timeouts-servlet-connection (host-timeouts host-info))
                        #:timeouts-default-servlet (timeouts-default-servlet (host-timeouts host-info)))
         (files:make #:htdocs-path (paths-htdocs (host-paths host-info))
                     #:mime-types-path (paths-mime-types (host-paths host-info))
                     #:indices (host-indices host-info)
                     #:file-not-found-responder (responders-file-not-found (host-responders host-info)))))))
  
  (define web-server@
    (compound-unit/sig 
      (import (TCP : net:tcp^)
              (CONFIG : web-config^))
      (link (DISPATCH-CONFIG : dispatch-server-config^
                             (web-config@->dispatch-server-config@ CONFIG))
            (DISPATCH : dispatch-server^
                      (dispatch-server@ TCP DISPATCH-CONFIG)))
      (export (open (DISPATCH : web-server^))))))