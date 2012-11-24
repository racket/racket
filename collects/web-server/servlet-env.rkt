; Derived from plai/web/server, which was based on an older version of this
; Also derived from planet/untyped/instaservlet
#lang racket/base
(require racket/contract
         racket/list
         racket/serialize
         racket/runtime-path
         (for-syntax racket/base))
(require net/url
         web-server/managers/lru
         web-server/managers/manager
         web-server/configuration/namespace
         web-server/http
         web-server/stuffers
         web-server/configuration/responders
         web-server/private/mime-types
         web-server/servlet/setup
         web-server/servlet/servlet-structs
         web-server/servlet-dispatch
         unstable/contract
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
     (semaphore-post sema)
     (response/xexpr
      `(html (head (title "Server Stopped")
                   (link ([rel "stylesheet"] [href "/error.css"])))
             (body (div ([class "section"])
                        (div ([class "title"]) "Server Stopped")
                        (p "Return to DrRacket."))))))))

(define-runtime-path default-web-root "default-web-root")

(provide/contract
 [serve/servlet (((request? . -> . can-be-response?))
                 (#:connection-close? boolean?
                  #:command-line? boolean?
                  #:launch-browser? boolean?
                  #:quit? boolean?
                  #:banner? boolean?
                  #:listen-ip (or/c false/c string?)
                  #:port tcp-listen-port?
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
                  #:file-not-found-responder (request? . -> . can-be-response?)
                  #:servlet-loading-responder (url? any/c . -> . can-be-response?)
                  #:servlet-responder (url? any/c . -> . can-be-response?)
                  #:mime-types-path path-string?
                  #:servlet-path string?
                  #:servlet-regexp regexp?
                  #:log-file (or/c false/c path-string?)
                  #:log-format (or/c log:log-format/c log:format-req/c))
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
         #:connection-close?
         [connection-close? #f]
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
             (response/xexpr
              `(html (head (title "Page Has Expired."))
                     (body (p "Sorry, this page has expired. Please go back.")))))
           (* 128 1024 1024))]

         #:servlet-path
         [servlet-path "/servlets/standalone.rkt"]
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
         #:servlet-loading-responder
         [responders-servlet-loading servlet-loading-responder]
         #:servlet-responder 
         [responders-servlet servlet-error-responder]

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
     (and log-file (log:make #:format 
                             (if (symbol? log-format)
                                 (log:log-format->format log-format)
                                 log-format)
                             #:log-path log-file))
     (and quit? (filter:make #rx"^/quit$" (quit-server sema)))
     (dispatch/servlet 
      start
      #:regexp servlet-regexp
      #:stateless? stateless?
      #:stuffer stuffer
      #:current-directory servlet-current-directory
      #:manager manager
      #:responders-servlet-loading
      responders-servlet-loading
      #:responders-servlet 
      responders-servlet)
     (let-values ([(clear-cache! url->servlet)
                   (servlets:make-cached-url->servlet
                    (fsmap:filter-url->path
                     #rx"\\.(ss|scm|rkt|rktd)$"
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
     (lift:make (compose any->response file-not-found-responder))))
  (serve/launch/wait
   dispatcher  
   #:connection-close? connection-close?
   #:launch-path (if launch-browser? servlet-path #f) 
   #:banner? banner?   
   #:listen-ip listen-ip
   #:port the-port
   #:ssl-cert ssl-cert
   #:ssl-key ssl-key))
