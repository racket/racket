(module run mzscheme
  (require (lib "unit.ss")
           (lib "tcp-sig.ss" "net"))
  (require (lib "dispatch-server-sig.ss" "web-server" "private")
           (lib "dispatch-server-unit.ss" "web-server" "private")
           (prefix http: (lib "request.ss" "web-server" "private"))
           (lib "configuration-structures.ss" "web-server" "private")
           (prefix files: (lib "dispatch-files.ss" "web-server" "dispatchers"))
           (prefix filter: (lib "dispatch-filter.ss" "web-server" "dispatchers"))
           (prefix sequencer: (lib "dispatch-sequencer.ss" "web-server" "dispatchers")))
  (require "hardcoded-configuration.ss"
           (prefix servlets2: "dispatch-servlets2.ss"))
  
  (define port 8080)
  (define listen-ip #f)
  (define max-waiting 40)
  (define initial-connection-timeout 60)
  (define host-info hardcoded-host)
  (define read-request http:read-request)
  (define dispatch
    (sequencer:make
     (filter:make
      #rx"\\.ss$"
      (servlets2:make #:htdocs-path (paths-htdocs (host-paths host-info))
                      #:timeouts-servlet-connection (timeouts-servlet-connection (host-timeouts host-info))
                      #:responders-servlet-loading (responders-servlet-loading (host-responders host-info))
                      #:responders-servlet (responders-servlet (host-responders host-info))
                      #:responders-file-not-found (responders-file-not-found (host-responders host-info))))
     (files:make #:htdocs-path (paths-htdocs (host-paths host-info))
                 #:mime-types-path (paths-mime-types (host-paths host-info))
                 #:indices (host-indices host-info)
                 #:file-not-found-responder (responders-file-not-found (host-responders host-info)))))
  
  (define-values/invoke-unit
    dispatch-server@
    (import tcp^ dispatch-server-config^)
    (export dispatch-server^))
  
  (define shutdown (serve))
  (semaphore-wait (make-semaphore 0)))