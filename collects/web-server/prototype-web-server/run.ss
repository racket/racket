(module run mzscheme
  (require (lib "web-server.ss" "web-server")
           (lib "configuration.ss" "web-server" "private")
           (prefix files: (lib "dispatch-files.ss" "web-server" "dispatchers"))
           (prefix filter: (lib "dispatch-filter.ss" "web-server" "dispatchers"))
           (prefix sequencer: (lib "dispatch-sequencer.ss" "web-server" "dispatchers"))
           (prefix servlets2: "dispatch-servlets2.ss"))
  
  (define server-root-path (build-path "~" "Development" "plt" "default-web-root"))
  (define default-host-path (build-path server-root-path "conf"))  
  (define htdocs-path (build-path server-root-path "htdocs"))
  (define file-not-found-file (build-path default-host-path "not-found.html"))
  (define servlet-error-file (build-path default-host-path "servlet-error.html"))
    
  (serve
   #:port 8080
   #:dispatch (sequencer:make
               (filter:make
                #rx"\\.ss"
                (servlets2:make #:htdocs-path htdocs-path
                                #:timeouts-servlet-connection 86400
                                #:responders-servlet-loading (gen-servlet-responder servlet-error-file)
                                #:responders-servlet (gen-servlet-responder servlet-error-file)
                                #:responders-file-not-found (gen-file-not-found-responder file-not-found-file)))
               (files:make #:htdocs-path htdocs-path
                           #:mime-types-path (build-path server-root-path "mime.types")
                           #:indices (list "index.html" "index.htm")
                           #:file-not-found-responder (gen-file-not-found-responder file-not-found-file))))
  
  (do-not-return))