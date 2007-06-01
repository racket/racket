(module web-server-unit mzscheme
  (require (lib "tcp-sig.ss" "net")
           (lib "unit.ss"))
  (require "web-server-sig.ss"
           "web-config-sig.ss"
           "private/dispatch-server-unit.ss"
           "private/dispatch-server-sig.ss"
           "private/web-server-structs.ss"
           "configuration/configuration-table-structs.ss"
           "private/cache-table.ss"
           (prefix http: "private/request.ss"))
  (require (prefix fsmap: "dispatchers/filesystem-map.ss")
           (prefix sequencer: "dispatchers/dispatch-sequencer.ss")
           (prefix passwords: "dispatchers/dispatch-passwords.ss")
           (prefix files: "dispatchers/dispatch-files.ss")
           (prefix servlets: "dispatchers/dispatch-servlets.ss")
           (prefix path-procedure: "dispatchers/dispatch-pathprocedure.ss")
           (prefix log: "dispatchers/dispatch-log.ss")
           (prefix host: "dispatchers/dispatch-host.ss")
           (prefix filter: "dispatchers/dispatch-filter.ss")
           (prefix const: "dispatchers/dispatch-const.ss"))
  
  (provide web-server@)
  
  (define-unit web-config@->dispatch-server-config@
    (import (prefix config: web-config^))
    (export dispatch-server-config^)
    (init-depend web-config^)
    (define read-request http:read-request)
    
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
       (let-values ([(update-password-cache! password-check)
                     (passwords:make #:password-file (host-passwords host-info)
                                     #:password-connection-timeout (timeouts-password (host-timeouts host-info))
                                     #:authentication-responder (responders-authentication (host-responders host-info)))])
         (sequencer:make
          password-check
          (path-procedure:make "/conf/refresh-passwords"
                               (lambda _
                                 (update-password-cache!)
                                 ((responders-passwords-refreshed (host-responders host-info)))))))
       (path-procedure:make "/conf/collect-garbage"
                            (lambda _
                              (collect-garbage)
                              ((responders-collect-garbage (host-responders host-info)))))
       (let-values ([(clear-cache! servlet-dispatch)
                     (servlets:make config:instances config:scripts config:make-servlet-namespace
                                    #:url->path
                                    (fsmap:make-url->path/optimism
                                     (fsmap:make-url->path (paths-servlet (host-paths host-info))))                                    
                                    #:responders-servlet-loading (responders-servlet-loading (host-responders host-info))
                                    #:responders-servlet (responders-servlet (host-responders host-info))
                                    #:timeouts-servlet-connection (timeouts-servlet-connection (host-timeouts host-info))
                                    #:timeouts-default-servlet (timeouts-default-servlet (host-timeouts host-info)))])
         (sequencer:make
          (path-procedure:make "/conf/refresh-servlets"
                               (lambda _
                                 (clear-cache!)
                                 ((responders-servlets-refreshed (host-responders host-info)))))
          (filter:make
           #rx"^/servlets"
           servlet-dispatch)))
       (files:make #:url->path (fsmap:make-url->path (paths-htdocs (host-paths host-info)))
                   #:mime-types-path (paths-mime-types (host-paths host-info))
                   #:indices (host-indices host-info))
       (const:make (responders-file-not-found (host-responders host-info))))))
  
  (define-compound-unit/infer web-server@
    (import tcp^ web-config^)
    (export web-server^)
    (link web-config@->dispatch-server-config@ dispatch-server@)))