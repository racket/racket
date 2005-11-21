(module web-server-unit mzscheme
  (require "sig.ss"
           "web-server-structs.ss"
           "connection-manager.ss"
           "configuration-structures.ss"
           "servlet.ss"
           "cache-table.ss"
           (rename "request-parsing.ss" 
                   the-read-request read-request))
  (require (prefix sequencer: "dispatch-sequencer.ss")
           (prefix passwords: "dispatch-passwords.ss")
           (prefix files: "dispatch-files.ss")
           (prefix servlets: "dispatch-servlets.ss")
           (prefix path-procedure: "dispatch-pathprocedure.ss")
           (prefix log: "dispatch-log.ss")
           (prefix host: "dispatch-host.ss"))
  (require (lib "tcp-sig.ss" "net")
           (lib "unitsig.ss")
           (lib "string.ss")
           (lib "url.ss" "net"))
  (provide web-server@)  
  
  ;; ****************************************  
  (define dispatch-server@
    (unit/sig dispatch-server^
      (import net:tcp^ (config : dispatch-server-config^))
      
      ;; serve: -> -> void
      ;; start the server and return a thunk to shut it down
      (define (serve)
        (let ([the-server-custodian (make-custodian)])
          (start-connection-manager the-server-custodian)
          (parameterize ([current-custodian the-server-custodian]
                         [current-server-custodian the-server-custodian])
            (thread
             (lambda ()
               (listener-loop))))
          (lambda ()
            (custodian-shutdown-all the-server-custodian))))
      
      ;; listener-loop : -> void
      ;; loops around starting a listener if the current listener dies
      (define (listener-loop)
        (let loop ()
          (thread-wait
           (let* ([listener (tcp-listen config:port config:max-waiting
                                        #t config:listen-ip)]
                  [get-ports
                   (lambda () (tcp-accept listener))])
             (thread
              (lambda ()
                (with-handlers ([void (lambda (e)
                                        ; If the exception did not kill the listener
                                        (with-handlers ([void void])
                                          (tcp-close listener))
                                        ; Rethrow the error to this thread's error printer
                                        (raise e))])
                  (server-loop get-ports
                               tcp-addresses))))))
          (loop)))
      
      ;; server-loop: (-> input-port output-port) (input-port -> string string) -> void
      ;; start a thread to handle each incoming connection
      (define (server-loop get-ports port-addresses)
        (let loop ()
          (let ([connection-cust (make-custodian)])
            (parameterize ([current-custodian connection-cust])
              (let-values ([(ip op) (get-ports)])
                (serve-ports/inner ip op
                                   port-addresses))))
          (loop)))
      
      ;; serve-ports : input-port output-port -> void
      ;; returns immediately, spawning a thread to handle
      ;; the connection
      ;; NOTE: (GregP) should allow the user to pass in a connection-custodian
      (define (serve-ports ip op)
        (let ([server-cust (make-custodian)])
          (start-connection-manager server-cust)
          (parameterize ([current-custodian server-cust]
                         [current-server-custodian server-cust])
            (let ([connection-cust (make-custodian)])
              (parameterize ([current-custodian connection-cust])
                (serve-ports/inner ip op
                                   (lambda (ip)
                                     (values "127.0.0.1"
                                             "127.0.0.1"))))))))
      
      ;; serve-ports/inner : input-port output-port (input-port -> string string) -> void
      ;; returns immediately, spawning a thread to handle
      (define (serve-ports/inner ip op port-addresses)
        (thread
         (lambda ()
           (let ([conn (new-connection config:initial-connection-timeout
                                       ip op (current-custodian) #f)])
             (with-handlers ([exn:fail:network?
                              (lambda (e)
                                (set-connection-close?! conn #t)
                                (kill-connection! conn)
                                (raise e))])
               (serve-connection conn port-addresses))))))
      
      ;; serve-connection: connection (input-port -> string string) -> void
      ;; respond to all requests on this connection
      (define (serve-connection conn port-addresses)
        (let connection-loop ()
          (let-values ([(req close?) (config:read-request conn config:port port-addresses)])
            (set-connection-close?! conn close?)
            (adjust-connection-timeout! conn config:initial-connection-timeout)
            (config:dispatch conn req)
            (cond
              [(connection-close? conn) (kill-connection! conn)]
              [else (connection-loop)]))))))
  
  (define web-config@->dispatch-server-config@
    (unit/sig dispatch-server-config^
      (import (config : web-config^))
      (define read-request the-read-request)
      
      (define port config:port)
      (define listen-ip config:listen-ip)
      (define max-waiting config:max-waiting)
      (define initial-connection-timeout config:initial-connection-timeout)
      
      ;; dispatch : connection request -> void
      (define dispatch 
        (let* ([cache (make-cache-table)])
          (host:gen-dispatcher
           (lambda (host)
             (cache-table-lookup!
              cache host
              (lambda ()
                (host-info->dispatcher
                 (config:virtual-hosts (symbol->string host)))))))))
      
      ;; host-info->dispatcher : host-info -> conn request -> void
      (define (host-info->dispatcher host-info)
        (sequencer:gen-dispatcher
         (log:gen-dispatcher (host-log-format host-info)
                             (host-log-path host-info))
         (passwords:gen-dispatcher (host-passwords host-info)
                                   (timeouts-password (host-timeouts host-info))
                                   (responders-authentication (host-responders host-info))
                                   (responders-passwords-refreshed (host-responders host-info)))
         (path-procedure:gen-dispatcher "/conf/collect-garbage"
                                        (lambda ()
                                          (collect-garbage)
                                          ((responders-collect-garbage (host-responders host-info)))))
         (servlets:gen-dispatcher config:instances config:scripts config:make-servlet-namespace
                                  (paths-servlet (host-paths host-info))
                                  (responders-servlets-refreshed (host-responders host-info))
                                  (responders-servlet-loading (host-responders host-info))
                                  (responders-servlet (host-responders host-info))
                                  (responders-file-not-found (host-responders host-info))
                                  (timeouts-servlet-connection (host-timeouts host-info))
                                  (timeouts-default-servlet (host-timeouts host-info)))
         (files:gen-dispatcher (paths-htdocs (host-paths host-info))
                               (host-indices host-info)
                               (responders-file-not-found (host-responders host-info)))))))
  
  (define web-server@
    (compound-unit/sig 
      (import (TCP : net:tcp^)
              (CONFIG : web-config^))
      (link (DISPATCH-CONFIG : dispatch-server-config^
                             (web-config@->dispatch-server-config@ CONFIG))
            (DISPATCH : dispatch-server^
                      (dispatch-server@ TCP DISPATCH-CONFIG)))
      (export (open (DISPATCH : web-server^))))))
