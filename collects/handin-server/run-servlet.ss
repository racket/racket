#lang scheme

(define default-context-length (error-print-context-length))

;; This code has parts that are copied from `serve/servlet' in
;; "web-server/servlet-env.ss", and parts from `serve/launch/wait' in
;; "web-server/servlet-dispatch.ss"

(require web-server/web-server
         web-server/servlet-dispatch
         web-server/managers/lru
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in log: web-server/dispatchers/dispatch-log)
         web-server/http/request-structs
         net/url
         net/tcp-sig
         net/ssl-tcp-unit)

(provide run-servlet)
(define (run-servlet dispatcher port server-dir
                     #:namespace [namespace '()]
                     #:log-file [log-file #f])
  (define ((send-error msg to) req)
    (let ([to (to)])
      `(html (head (meta ([http-equiv "refresh"]
                          [content ,(format "3;URL=~a" to)]))
                   (title ,msg))
             (body ,msg "; " (a ([href ,to]) "restarting") " in 3 seconds."))))
  (define tcp@
    (let ()
      (define-unit-binding ssl-tcp@
        (make-ssl-tcp@ (build-path server-dir "server-cert.pem")
                       (build-path server-dir "private-key.pem")
                       #f #f #f #f #f)
        (import) (export tcp^))
      ssl-tcp@))
  (serve
   #:port port #:listen-ip #f
   #:tcp@ tcp@
   #:dispatch
   (sequencer:make
    (and log-file (log:make #:format (log:log-format->format 'apache-default)
                            #:log-path log-file))
    (let ([init-path (make-parameter "/")])
      (dispatch/servlet
       (lambda (req)
         (init-path (url->string (request-uri req)))
         (dispatcher req))
       #:regexp #rx""
       #:namespace namespace
       #:current-directory server-dir
       #:manager (make-threshold-LRU-manager
                  (send-error "Your session has expired" init-path)
                  (* 12 1024 1024))))
    ;; This can be used to serve html content too; doesn't make sense now,
    ;; since the servlet will be used for all requests, and it never calls
    ;; (next-dispatcher).  (See "servlet-env.ss" for the needed `require's.)
    ;; (files:make
    ;;  #:url->path (fsmap:make-url->path (build-path server-dir "htdocs")))
    ;; (lift:make (send-error "File not found" (lambda () "/")))
    )))
