#lang scheme/base
(require net/tcp-sig
         mzlib/unit)
(require "web-server-sig.ss"
         "web-config-sig.ss"
         "private/dispatch-server-unit.ss"
         "private/dispatch-server-sig.ss"
         "private/web-server-structs.ss"
         "private/mime-types.ss"
         "configuration/configuration-table-structs.ss"
         "private/cache-table.ss"
         (prefix-in http: web-server/http/request))
(require "dispatchers/dispatch.ss"
         web-server/servlet/setup
         (prefix-in fsmap: "dispatchers/filesystem-map.ss")
         (prefix-in sequencer: "dispatchers/dispatch-sequencer.ss")
         (prefix-in timeout: web-server/dispatchers/dispatch-timeout)
         (prefix-in passwords: "dispatchers/dispatch-passwords.ss")
         (prefix-in files: "dispatchers/dispatch-files.ss")
         (prefix-in servlets: "dispatchers/dispatch-servlets.ss")
         (prefix-in path-procedure: "dispatchers/dispatch-pathprocedure.ss")
         (prefix-in log: "dispatchers/dispatch-log.ss")
         (prefix-in host: "dispatchers/dispatch-host.ss")
         (prefix-in filter: "dispatchers/dispatch-filter.ss")
         (prefix-in lift: "dispatchers/dispatch-lift.ss"))

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
     (timeout:make initial-connection-timeout)
     (if (and (host-log-format host-info)
              (host-log-path host-info))
         (log:make #:format (log:log-format->format (host-log-format host-info))
                   #:log-path (host-log-path host-info))
         (lambda (conn req) (next-dispatcher)))
     (if (host-passwords host-info)
         (let-values ([(update-password-cache! password-check)
                       (passwords:password-file->authorized? (host-passwords host-info))])
           (sequencer:make
            (timeout:make (timeouts-password (host-timeouts host-info)))
            (passwords:make
             (passwords:make-basic-denied?/path
              password-check)
             #:authentication-responder (responders-authentication (host-responders host-info)))
            (path-procedure:make "/conf/refresh-passwords"
                                 (lambda _
                                   (update-password-cache!)
                                   ((responders-passwords-refreshed (host-responders host-info)))))))
         (lambda (conn req) (next-dispatcher)))
     (path-procedure:make "/conf/collect-garbage"
                          (lambda _
                            (collect-garbage)
                            ((responders-collect-garbage (host-responders host-info)))))     
     (let-values ([(clear-cache! url->servlet)
                   (servlets:make-cached-url->servlet
                    (fsmap:filter-url->path
                     #rx"\\.(ss|scm)$"
                     (fsmap:make-url->valid-path
                      (fsmap:make-url->path (paths-servlet (host-paths host-info)))))
                    (make-default-path->servlet
                     #:make-servlet-namespace config:make-servlet-namespace
                     #:timeouts-default-servlet (timeouts-default-servlet (host-timeouts host-info))))])
       (sequencer:make
        (path-procedure:make "/conf/refresh-servlets"
                             (lambda _
                               (clear-cache!)
                               ((responders-servlets-refreshed (host-responders host-info)))))
        (sequencer:make
         (timeout:make (timeouts-servlet-connection (host-timeouts host-info)))
         (servlets:make url->servlet
                        #:responders-servlet-loading (responders-servlet-loading (host-responders host-info))
                        #:responders-servlet (responders-servlet (host-responders host-info))))))
     (files:make #:url->path (fsmap:make-url->path (paths-htdocs (host-paths host-info)))
                 #:path->mime-type (make-path->mime-type (paths-mime-types (host-paths host-info)))
                 #:indices (host-indices host-info))
     (lift:make (responders-file-not-found (host-responders host-info))))))

(define-compound-unit/infer web-server@
  (import tcp^ web-config^)
  (export web-server^)
  (link web-config@->dispatch-server-config@ dispatch-server@))
