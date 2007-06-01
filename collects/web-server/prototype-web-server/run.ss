(module run mzscheme
  (require (lib "web-server.ss" "web-server")
           (lib "responders.ss" "web-server" "configuration")
           (prefix fsmap: (lib "filesystem-map.ss" "web-server" "dispatchers"))
           (prefix files: (lib "dispatch-files.ss" "web-server" "dispatchers"))
           (prefix filter: (lib "dispatch-filter.ss" "web-server" "dispatchers"))
           (prefix const: (lib "dispatch-const.ss" "web-server" "dispatchers"))
           (prefix sequencer: (lib "dispatch-sequencer.ss" "web-server" "dispatchers"))
           (prefix servlets2: (lib "dispatch-servlets2.ss" "web-server" "prototype-web-server")))
  
  (define server-root-path (build-path "~" "Development" "plt" "default-web-root"))
  (define default-host-path (build-path server-root-path "conf"))  
  (define file-not-found-file (build-path default-host-path "not-found.html"))
  (define servlet-error-file (build-path default-host-path "servlet-error.html"))
  
  (define url->path
    (fsmap:make-url->path 
     (build-path server-root-path "htdocs")))
  
  (serve
   #:port 8080
   #:dispatch (sequencer:make
               (filter:make
                #rx"\\.ss"
                (servlets2:make #:url->path (fsmap:make-url->path/optimism url->path)
                                #:timeouts-servlet-connection 86400
                                #:responders-servlet-loading (gen-servlet-responder servlet-error-file)
                                #:responders-servlet (gen-servlet-responder servlet-error-file)))
               (files:make #:url->path url->path
                           #:mime-types-path (build-path server-root-path "mime.types")
                           #:indices (list "index.html" "index.htm"))
               (const:make (gen-file-not-found-responder file-not-found-file))))
  
  (do-not-return))