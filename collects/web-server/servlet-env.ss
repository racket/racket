; Derived from plai/web/server, which was based on an older version of this
; Also derived from planet/untyped/instaservlet
#lang scheme/base
(require (prefix-in net: net/sendurl)
         scheme/contract
         scheme/list)
(require web-server/web-server
         web-server/managers/lru
         web-server/managers/manager
         web-server/private/servlet
         web-server/configuration/namespace
         web-server/private/cache-table 
         web-server/private/request-structs
         web-server/private/response-structs
         web-server/private/util
         web-server/configuration/responders
         web-server/dispatchers/dispatch
         web-server/private/mime-types
         web-server/configuration/configuration-table
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in fsmap: web-server/dispatchers/filesystem-map)         
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)         
         (prefix-in servlets: web-server/dispatchers/dispatch-servlets))

(define send-url (make-parameter net:send-url))

(define (quit-server sema)
  (lift:make
   (lambda (request)
     (thread (lambda () (sleep 2) (semaphore-post sema)))
     `(html
       (head
        (title "Server Stopped")
        (link ([rel "stylesheet"] [href "/error.css"])))
       (body
        (div ([class "section"])
             (div ([class "title"]) "Server Stopped")
             (p "Return to DrScheme.")))))))

(provide/contract
 [serve/servlet (((request? . -> . response?))
                 (#:launch-browser? boolean?
                  #:quit? boolean?
                  #:listen-ip string?
                  #:port number?
                  #:manager manager?
                  #:servlet-namespace (listof module-path?)
                  #:server-root-path path?
                  #:extra-files-path path?
                  #:servlets-root path?
                  #:file-not-found-path path?
                  #:mime-types-path path?
                  #:servlet-path path?)
                 . ->* .
                 void)])
(define (serve/servlet new-servlet 
                       #:launch-browser?
                       [launch-browser? #t]
                       #:quit?
                       [quit? #t]
                       
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
                         (* 64 1024 1024))]
                       
                       #:servlet-namespace
                       [servlet-namespace empty]
                       #:server-root-path 
                       [server-root-path (directory-part default-configuration-table-path)]
                       #:extra-files-path 
                       [extra-files-path (build-path server-root-path "htdocs")]
                       #:servlets-root
                       [servlets-root (build-path server-root-path ".")]
                       #:file-not-found-path
                       [file-not-found-path (build-path server-root-path "conf" "not-found.html")]
                       #:mime-types-path
                       [mime-types-path (build-path server-root-path "mime.types")]
                       #:servlet-path
                       [servlet-path "servlets/standalone.ss"])
  (let*-values
      ([(standalone-url)
        (format "http://localhost:~a/~a" the-port servlet-path)]
       [(make-servlet-namespace) (make-make-servlet-namespace
                                  #:to-be-copied-module-specs servlet-namespace)]
       [(the-scripts) (make-cache-table)]
       [(sema) (make-semaphore 0)]
       [(dispatcher)
        (sequencer:make
         (if quit?
             (filter:make 
              #rx"^/quit$"
              (quit-server sema))
             (lambda _ (next-dispatcher)))
         (filter:make
          #rx"\\.ss"
          (let-values ([(clear-cache! servlet-dispatch)
                        (servlets:make (box the-scripts)
                                       #:make-servlet-namespace make-servlet-namespace
                                       #:url->path
                                       (lambda _
                                         (values (build-path servlets-root servlet-path)
                                                 empty)))])
            servlet-dispatch))
         (files:make 
          #:url->path (fsmap:make-url->path
                       extra-files-path)
          #:path->mime-type (make-path->mime-type mime-types-path)
          #:indices (list "index.html" "index.htm"))         
         (files:make 
          #:url->path (fsmap:make-url->path 
                       (build-path server-root-path "htdocs"))
          #:path->mime-type (make-path->mime-type (build-path server-root-path "mime.types"))
          #:indices (list "index.html" "index.htm"))
         (lift:make (gen-file-not-found-responder file-not-found-path)))]
       [(shutdown-server)
        (serve #:dispatch dispatcher
               #:listen-ip listen-ip
               #:port the-port)])
    (cache-table-lookup! the-scripts
                         (string->symbol
                          (path->string
                           (build-path servlets-root servlet-path)))
                         (lambda ()
                           (make-servlet (make-custodian)
                                         (make-servlet-namespace)
                                         manager
                                         new-servlet)))
    (when launch-browser?
      ((send-url) standalone-url #t))
    (printf "Your Web application is running at ~a.~n" standalone-url)
    (printf "Click 'Stop' at any time to terminate the Web Server.~n")
    (with-handlers
        ([exn:break?
          (lambda (exn)
            (printf "~nWeb Server stopped.~n")
            (shutdown-server))])
      (semaphore-wait/enable-break sema))
    ; We shouldn't get here, because nothing posts to the semaphore. But just in case...
    (shutdown-server)))