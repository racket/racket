(module run mzscheme
  (require (lib "unit.ss")
           (lib "tcp-sig.ss" "net"))
  (require (lib "dispatch-server-sig.ss" "web-server" "private")
           (lib "dispatch-server-unit.ss" "web-server" "private")
           (lib "response.ss" "web-server")
           (lib "util.ss" "web-server" "private")
           (prefix http: (lib "request.ss" "web-server" "private"))
           (prefix files: (lib "dispatch-files.ss" "web-server" "dispatchers"))
           (prefix filter: (lib "dispatch-filter.ss" "web-server" "dispatchers"))
           (prefix sequencer: (lib "dispatch-sequencer.ss" "web-server" "dispatchers"))
           (prefix servlets2: "dispatch-servlets2.ss"))
  
  ; error-response : nat str str [(cons sym str) ...] -> response
  (define (error-response code short text-file . extra-headers)
    (make-response/full code short
                        (current-seconds) TEXT/HTML-MIME-TYPE
                        extra-headers
                        (list (read-file text-file))))
  
  ; read-file : str -> str
  (define (read-file path)
    (call-with-input-file path
      (lambda (in) (read-string (file-size path) in))))
  
  (define port 8080)
  (define listen-ip #f)
  (define max-waiting 40)
  (define initial-connection-timeout 60)
  (define read-request http:read-request)
  
  (define server-root-path (build-path "~" "Development" "plt" "default-web-root"))
  (define default-host-path (build-path server-root-path "conf"))  
  (define htdocs-path (build-path server-root-path "htdocs"))
  (define file-not-found-file (build-path default-host-path "not-found.html"))
  (define servlet-error-file (build-path default-host-path "servlet-error.html"))
  
  (define responders-file-not-found
    (lambda (url)
      (error-response 404 "File not found" file-not-found-file)))
  (define responders-servlet
    (lambda (url exn)
      ((error-display-handler)
       (format "Servlet exception:\n~a\n" (exn-message exn))
       exn)
      (error-response 500 "Servlet error" servlet-error-file)))
  (define responders-servlet-loading
    (lambda (url exn)
      ((error-display-handler)
       (format "Servlet loading exception:\n~a\n" (exn-message exn))
       exn)
      (make-response/full 500 "Servlet didn't load"
                          (current-seconds)
                          #"text/plain" 
                          '() 
                          (list "Servlet didn't load.\n"
                                (exn->string exn)))))
  
  (define dispatch
    (sequencer:make
     (filter:make
      #rx"\\.ss"
      (servlets2:make #:htdocs-path htdocs-path
                      #:timeouts-servlet-connection 86400
                      #:responders-servlet-loading responders-servlet-loading
                      #:responders-servlet responders-servlet
                      #:responders-file-not-found responders-file-not-found))
     (files:make #:htdocs-path htdocs-path
                 #:mime-types-path (build-path server-root-path "mime.types")
                 #:indices (list "index.html" "index.htm")
                 #:file-not-found-responder responders-file-not-found)))
  
  (define-values/invoke-unit
    dispatch-server@
    (import tcp^ dispatch-server-config^)
    (export dispatch-server^))
  
  (define shutdown (serve))
  (semaphore-wait (make-semaphore 0)))