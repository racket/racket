#lang scheme

(define default-context-length (error-print-context-length))

;; This code has parts that are copied from `serve/servlet' in
;; "web-server/servlet-env.ss", and parts from `serve/launch/wait' in
;; "web-server/servlet-dispatch.ss"

(require web-server/web-server
         web-server/servlet-dispatch
         web-server/managers/lru
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in fsmap: web-server/dispatchers/filesystem-map)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in log: web-server/dispatchers/dispatch-log)
         net/tcp-sig
         net/ssl-tcp-unit)

(define ((send-error msg) req)
  `(html (head (meta [(http-equiv "refresh") (content "3;URL=/")])
               (title ,msg))
         (body ,msg "; " (a ([href "/"]) "restarting") " in 3 seconds.")))

(provide run-servlet)
(define (run-servlet dispatcher port server-dir
                     #:log-file [log-file #f])
  (serve
   #:port port #:listen-ip #f
   #:tcp@ (let ()
            (define-unit-binding ssl-tcp@
              (make-ssl-tcp@ (build-path server-dir "server-cert.pem")
                             (build-path server-dir "private-key.pem")
                             #f #f #f #f #f)
              (import) (export tcp^))
            ssl-tcp@)
   #:dispatch
   (sequencer:make
    (and log-file (log:make #:format (log:log-format->format 'apache-default)
                            #:log-path log-file))
    (dispatch/servlet
     dispatcher
     #:regexp #rx""
     #:namespace '(handin-server/private/md5
                   handin-server/private/logger
                   handin-server/private/config
                   handin-server/private/hooker
                   handin-server/private/reloadable)
     #:current-directory server-dir
     #:manager (make-threshold-LRU-manager
                (send-error "Your session has expired") (* 12 1024 1024)))
    (files:make
     #:url->path (fsmap:make-url->path (build-path server-dir "htdocs")))
    (lift:make (send-error "File not found")))))
