; Derived from plai/web/server, which was based on an older version of this
; Also derived from planet/untyped/instaservlet
#lang scheme
(require scheme/contract
         scheme/list
         scheme/serialize
         scheme/runtime-path)
(require web-server/managers/lru
         web-server/managers/manager
         web-server/configuration/namespace
         web-server/http
         web-server/stuffers
         web-server/configuration/responders
         web-server/private/mime-types
         web-server/servlet/setup
         web-server/servlet-dispatch
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in fsmap: web-server/dispatchers/filesystem-map)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in servlets: web-server/dispatchers/dispatch-servlets)
         (prefix-in log: web-server/dispatchers/dispatch-log))

(define (quit-server sema)
  (lift:make
   (lambda (request)
     (thread (lambda () (sleep 2) (semaphore-post sema)))
     `(html (head (title "Server Stopped")
              (link ([rel "stylesheet"] [href "/error.css"])))
            (body (div ([class "section"])
                    (div ([class "title"]) "Server Stopped")
                    (p "Return to DrScheme.")))))))

(define-runtime-path default-web-root
  (list 'lib
        "web-server/default-web-root"))

(provide/contract
 [serve/servlet (((request? . -> . response/c))
                 (#:command-line? boolean?
                  #:launch-browser? boolean?
                  #:quit? boolean?
                  #:banner? boolean?
                  #:listen-ip (or/c false/c string?)
                  #:port number?
                  #:ssl? boolean?
                  #:ssl-cert (or/c false/c path-string?)
                  #:ssl-key (or/c false/c path-string?)
                  #:manager manager?
                  #:servlet-namespace (listof module-path?)
                  #:server-root-path path-string?
                  #:stateless? boolean?
                  #:stuffer (stuffer/c serializable? bytes?)
                  #:extra-files-paths (listof path-string?)
                  #:servlets-root path-string?
                  #:servlet-current-directory path-string?
                  #:file-not-found-responder (request? . -> . response/c)
                  #:mime-types-path path-string?
                  #:servlet-path string?
                  #:servlet-regexp regexp?
                  #:log-file (or/c false/c path-string?)
                  #:log-format log:log-format/c)
                 . ->* .
                 void)])

;; utility for conveniently chaining dispatchers
(define (dispatcher-sequence . dispatchers)
  (let loop ([ds dispatchers] [r '()])
    (cond [(null? ds) (apply sequencer:make (reverse r))]
          [(not (car ds))   (loop (cdr ds) r)]
          [(list? (car ds)) (loop (append (car ds) (cdr ds)) r)]
          [else (loop (cdr ds) (cons (car ds) r))])))

(define (serve/servlet
         start
         #:command-line?
         [command-line? #f]
         #:launch-browser?
         [launch-browser? (not command-line?)]
         #:quit?
         [quit? (not command-line?)]
         #:banner?
         [banner? (not command-line?)]

         #:listen-ip
         [listen-ip "127.0.0.1"]
         #:port
         [the-port 8000]         

         #:manager
         [manager
          (make-threshold-LRU-manager
           (lambda (request)
             `(html (head (title "Page Has Expired."))
                    (body (p "Sorry, this page has expired. Please go back."))))
           (* 128 1024 1024))]

         #:servlet-path
         [servlet-path "/servlets/standalone.ss"]
         #:servlet-regexp
         [servlet-regexp (regexp (format "^~a$" (regexp-quote servlet-path)))]
         #:stateless?
         [stateless? #f]
         #:stuffer
         [stuffer default-stuffer]

         #:servlet-namespace
         [servlet-namespace empty]
         #:server-root-path
         [server-root-path default-web-root]
         #:extra-files-paths
         [extra-files-paths (list (build-path server-root-path "htdocs"))]
         #:servlets-root
         [servlets-root (build-path server-root-path "htdocs")]
         #:servlet-current-directory
         [servlet-current-directory servlets-root]
         #:file-not-found-responder
         [file-not-found-responder
          (gen-file-not-found-responder
           (build-path server-root-path "conf" "not-found.html"))]
         #:mime-types-path
         [mime-types-path (let ([p (build-path server-root-path "mime.types")])
                            (if (file-exists? p)
                              p
                              (build-path default-web-root "mime.types")))]
         
         #:ssl?
         [ssl? #f]
         #:ssl-cert
         [ssl-cert (and ssl? (build-path server-root-path "server-cert.pem"))]
         #:ssl-key
         [ssl-key (and ssl? (build-path server-root-path "private-key.pem"))]

         #:log-file
         [log-file #f]
         #:log-format
         [log-format 'apache-default])
  (define (dispatcher sema)
    (dispatcher-sequence
     (and log-file (log:make #:format (log:log-format->format log-format)
                             #:log-path log-file))
     (and quit? (filter:make #rx"^/quit$" (quit-server sema)))
     (dispatch/servlet 
      start
      #:regexp servlet-regexp
      #:namespace servlet-namespace
      #:stateless? stateless?
      #:stuffer stuffer
      #:current-directory servlet-current-directory
      #:manager manager)
     (let-values ([(clear-cache! url->servlet)
                   (servlets:make-cached-url->servlet
                    (fsmap:filter-url->path
                     #rx"\\.(ss|scm)$"
                     (fsmap:make-url->valid-path
                      (fsmap:make-url->path servlets-root)))
                    (make-default-path->servlet
                     #:make-servlet-namespace 
                     (make-make-servlet-namespace #:to-be-copied-module-specs servlet-namespace)))])
       (servlets:make url->servlet))
     (map (lambda (extra-files-path)
            (files:make
             #:url->path (fsmap:make-url->path extra-files-path)
             #:path->mime-type (make-path->mime-type mime-types-path)
             #:indices (list "index.html" "index.htm")))
          extra-files-paths)
     (files:make
      #:url->path (fsmap:make-url->path (build-path server-root-path "htdocs"))
      #:path->mime-type (make-path->mime-type mime-types-path)
      #:indices (list "index.html" "index.htm"))
     (lift:make file-not-found-responder)))
  (serve/launch/wait
   dispatcher   
   #:launch-path (if launch-browser? servlet-path #f) 
   #:banner? banner?   
   #:listen-ip listen-ip
   #:port the-port
   #:ssl-cert ssl-cert
   #:ssl-key ssl-key))
