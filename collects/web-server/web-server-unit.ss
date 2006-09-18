(module web-server-unit mzscheme
  (require (lib "tcp-sig.ss" "net")
           (lib "contract.ss")
           (lib "unitsig.ss"))
  (require "sig.ss"
           "private/dispatch-server-unit.ss"
           "private/dispatch-server-sig.ss"
           "private/web-server-structs.ss"
           "private/configuration-structures.ss"
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
  (provide/contract
   ; XXX contract
   [web-server@ unit/sig?])  
    
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