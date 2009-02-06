; Derived from plai/web/server, which was based on an older version of this
; Also derived from planet/untyped/instaservlet
#lang scheme
(require (prefix-in net: net/sendurl)
         scheme/contract
         scheme/list
         scheme/unit
         scheme/serialize
         net/tcp-unit
         net/tcp-sig
         scheme/runtime-path
         net/ssl-tcp-unit)
(require web-server/web-server
         web-server/managers/lru
         web-server/managers/manager
         web-server/configuration/namespace
         web-server/http
         web-server/stuffers
         web-server/configuration/responders
         web-server/private/mime-types
         web-server/servlet/setup
         web-server/dispatchers/dispatch
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in fsmap: web-server/dispatchers/filesystem-map)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in servlets: web-server/dispatchers/dispatch-servlets)
         (prefix-in log: web-server/dispatchers/dispatch-log))

(define send-url (make-parameter net:send-url))

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
 [dispatch/servlet (((request? . -> . response/c))
                    (#:regexp regexp?
                     #:current-directory path-string?
                     #:namespace (listof module-path?)
                     #:stateless? boolean?
                     #:stuffer (stuffer/c serializable? bytes?)
                     #:manager manager?)
                    . ->* .
                    dispatcher/c)]
 [serve/launch/wait (((semaphore? . -> . dispatcher/c))
                     (#:launch-path (or/c false/c string?)
                      #:banner? boolean?
                      #:listen-ip (or/c false/c string?)
                      #:port number?
                      #:ssl-keys (or/c false/c (cons/c path-string? path-string?)))
                     . ->* .
                     void)]
 [serve/servlet (((request? . -> . response/c))
                 (#:command-line? boolean?
                  #:launch-browser? boolean?
                  #:quit? boolean?
                  #:banner? boolean?
                  #:listen-ip (or/c false/c string?)
                  #:port number?
                  #:ssl? boolean?
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

(define (dispatch/servlet 
         start
         #:regexp
         [servlet-regexp #rx""]
         #:current-directory 
         [servlet-current-directory (current-directory)]
         #:namespace 
         [servlet-namespace empty]                  
         #:stateless? 
         [stateless? #f]
         #:stuffer
         [stuffer default-stuffer]
         #:manager
         [manager
          (make-threshold-LRU-manager
           (lambda (request)
             `(html (head (title "Page Has Expired."))
                    (body (p "Sorry, this page has expired. Please go back."))))
           (* 64 1024 1024))])
  (define servlet-box (box #f))
  (define make-servlet-namespace
    (make-make-servlet-namespace #:to-be-copied-module-specs servlet-namespace))
  (filter:make
   servlet-regexp
   (servlets:make
    (lambda (url)
      (or (unbox servlet-box)
          (let ([servlet
                 (parameterize ([current-custodian (make-custodian)]
                                [current-namespace
                                 (make-servlet-namespace
                                  #:additional-specs
                                  default-module-specs)])
                   (if stateless?
                       (make-stateless.servlet servlet-current-directory stuffer start)
                       (make-v2.servlet servlet-current-directory manager start)))])
            (set-box! servlet-box servlet)
            servlet))))))

(define (serve/launch/wait
         dispatcher
         
         #:launch-path
         [launch-path #f]          
         #:banner?
         [banner? #t]
         
         #:listen-ip
         [listen-ip "127.0.0.1"]
         #:port
         [port 8000]
         #:ssl-keys
         [ssl-keys #f])
  (define ssl? (pair? ssl-keys))
  (define server-url
    (string-append (if ssl? "https" "http")
                   "://localhost"
                   (if (and (not ssl?) (= port 80))
                     "" (format ":~a" port))))
  (define sema (make-semaphore 0))
  (define shutdown-server
    (serve #:dispatch (dispatcher sema)
           #:listen-ip listen-ip
           #:port port
           #:tcp@ (if ssl?
                    (let ()
                      (define-unit-binding ssl-tcp@
                        (make-ssl-tcp@
                         (car ssl-keys) (cdr ssl-keys)
                         #f #f #f #f #f)
                        (import) (export tcp^))
                      ssl-tcp@)
                    tcp@)))
  (when launch-path
    ((send-url) (string-append server-url launch-path) #t))
  (when banner?
    (printf "Your Web application is running at ~a.\n" 
            (if launch-path 
                (string-append server-url launch-path)
                server-url))
    (printf "Click 'Stop' at any time to terminate the Web Server.\n"))
  (let ([bye (lambda ()
               (when banner? (printf "\nWeb Server stopped.\n"))
               (shutdown-server))])
    (with-handlers ([exn:break? (lambda (exn) (bye))])
      (semaphore-wait/enable-break sema)
      ;; We can get here if a /quit url is visited
      (bye))))

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
         #:ssl?
         [ssl? #f]

         #:manager
         [manager
          (make-threshold-LRU-manager
           (lambda (request)
             `(html (head (title "Page Has Expired."))
                    (body (p "Sorry, this page has expired. Please go back."))))
           (* 64 1024 1024))]

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
   #:ssl-keys
   (if ssl?
       (cons (build-path server-root-path "server-cert.pem")
             (build-path server-root-path "private-key.pem"))
       #f)))
