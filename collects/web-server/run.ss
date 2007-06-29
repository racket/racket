; This file is intended to be copied and/or modified and used as a template.
(module run mzscheme
  (require (lib "cmdline.ss")
           (lib "file.ss")
           (lib "web-server.ss" "web-server")
           (lib "responders.ss" "web-server" "configuration")
           (lib "mime-types.ss" "web-server" "private")
           (prefix fsmap: (lib "filesystem-map.ss" "web-server" "dispatchers"))
           (prefix timeout: (lib "dispatch-timeout.ss" "web-server" "dispatchers"))
           (prefix files: (lib "dispatch-files.ss" "web-server" "dispatchers"))
           (prefix filter: (lib "dispatch-filter.ss" "web-server" "dispatchers"))
           (prefix lift: (lib "dispatch-lift.ss" "web-server" "dispatchers"))
           (prefix sequencer: (lib "dispatch-sequencer.ss" "web-server" "dispatchers"))
           (prefix lang: (lib "dispatch-lang.ss" "web-server" "dispatchers"))
           (prefix stat: (lib "dispatch-stat.ss" "web-server" "dispatchers")))
  
  (define server-root-path (make-parameter (collection-path "web-server" "default-web-root")))
  (define port (make-parameter 8080))
  
  (parse-command-line 
   "run" (current-command-line-arguments)
   `((once-each
      [("-p" "--port") 
       ,(lambda (flag the-port) (port (string->number the-port)))
       (,(format "Specify a different port (default: ~a)" (number->string (port)))
        "number")]
      [("-r" "--root") 
       ,(lambda (flag path) (server-root-path (normalize-path (string->path path))))
       (,(format "Specify a different server root path (default: ~a)" (path->string (server-root-path)))
        "path")]))
   (lambda (flag-accum) (void))
   null)
  
  (define default-host-path (build-path (server-root-path) "conf"))  
  (define file-not-found-file (build-path default-host-path "not-found.html"))
  (define servlet-error-file (build-path default-host-path "servlet-error.html"))
  
  (define url->path
    (fsmap:make-url->path 
     (build-path (server-root-path) "htdocs")))
  
  (define gc-thread (stat:make-gc-thread 30))
  
  (serve #:port (port)
         #:dispatch
         (sequencer:make          
          (timeout:make (* 5 60))
          (stat:make)
          (filter:make
           #rx"\\.ss"
           (lang:make #:url->path (fsmap:make-url->valid-path url->path)
                      #:responders-servlet-loading (gen-servlet-responder servlet-error-file)
                      #:responders-servlet (gen-servlet-responder servlet-error-file)))
          (files:make #:url->path url->path
                      #:path->mime-type (make-path->mime-type (build-path (server-root-path) "mime.types"))
                      #:indices (list "index.html" "index.htm"))
          (lift:make (gen-file-not-found-responder file-not-found-file))))
  
  (do-not-return))