#lang racket/base

(define default-context-length (error-print-context-length))

;; This code has parts that are copied from `serve/servlet' in
;; "web-server/servlet-env.rkt", and parts from `serve/launch/wait' in
;; "web-server/servlet-dispatch.rkt"

(require racket/unit
         racket/async-channel
         web-server/web-server
         web-server/servlet-dispatch
         web-server/managers/lru
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in log: web-server/dispatchers/dispatch-log)
         web-server/http/request-structs
         net/url
         openssl
         net/tcp-sig)

;; maps ports to the underlying ssl ports (could be done with `prop:*-port')
(define port->ssl (make-weak-hasheq))

;; wrap a port so that the semaphore is posted to when it's closed
(define (wrap-with-closing-sema outp sema)
  (make-output-port
   (object-name outp) outp
   (lambda (buf start end nonblock? breakable?)
     ((if nonblock?
        write-bytes-avail*
        (if breakable?
          write-bytes-avail/enable-break
          write-bytes-avail))
      buf outp start end))
   (lambda ()
     (close-output-port outp)
     (semaphore-post sema))))

;; creates a tcp unit that grabs connections from the async-channel, each
;; connection is passed as a list holding an input port, an output port, and a
;; semaphore that should be posted to when the connection is done.
(define (make-tcp@ ach)
  (unit (import) (export tcp^)
    (define (tcp-accept . _)
      (define-values [inp outp sema] (apply values (async-channel-get ach)))
      (define outp* (wrap-with-closing-sema outp sema))
      (hash-set! port->ssl outp* outp)
      (values inp outp*))
    (define tcp-accept/enable-break tcp-accept)
    (define (tcp-accept-ready? l)
      ;; FIXME: need to peek into the channel
      #t)
    (define (tcp-abandon-port p)
      (ssl-abandon-port (hash-ref port->ssl p p)))
    (define (tcp-addresses p [port-numbers? #f])
      (if (void? p) ;; from listen!
          (if port-numbers?
              (values "localhost" "0.0.0.0" 1 1)
              (values "localhost" "0.0.0.0"))
          (ssl-addresses (hash-ref port->ssl p p) port-numbers?)))
    ;; prevent the server from actually listening:
    (define tcp-close void)
    (define tcp-listen void)
    (define tcp-listener? void?)
    (define tcp-connect void)
    (define tcp-connect/enable-break void)))

(provide run-servlet)
(define (run-servlet dispatcher
                     #:log-file [log-file #f])
  ;; a channel for incoming requests
  (define ach (make-async-channel))
  ;; wrap the dispatcher so we can post on the waiting semaphore
  (define (wrap-sequence . ds)
    (let* ([ds (filter values ds)] ;; drop #f when log-file is #f
           [d (apply sequencer:make ds)])
      (lambda (conn req)
        (d conn req)
        ;; (cond [(hash-ref port->sema (connection-o-port conn) #f)
        ;;        => semaphore-post])
        )))
  ;; error handler that redirects back to where the interaction started
  (define ((send-error msg to) req)
    (let ([to (to)])
      `(html (head (meta ([http-equiv "refresh"]
                          [content ,(format "3;URL=~a" to)]))
                   (title ,msg))
             (body ,msg "; " (a ([href ,to]) "restarting") " in 3 seconds."))))
  (define shut
    (serve
     #:tcp@ (make-tcp@ ach)
     #:dispatch
     (wrap-sequence
      (and log-file (log:make #:format (log:log-format->format 'apache-default)
                              #:log-path log-file))
      (let ([init-path (make-parameter "/")])
        (dispatch/servlet
         (lambda (req)
           (init-path (url->string (request-uri req)))
           (dispatcher req))
         #:regexp #rx""
         #:manager (make-threshold-LRU-manager
                    (send-error "Your session has expired" init-path)
                    (* 12 1024 1024))))
      ;; This can be used to serve html content too; doesn't make sense now,
      ;; since the servlet will be used for all requests, and it never calls
      ;; (next-dispatcher).  (See "servlet-env.rkt" for the needed `require's.)
      ;; (files:make
      ;;  #:url->path (fsmap:make-url->path (build-path server-dir "htdocs")))
      ;; (lift:make (send-error "File not found" (lambda () "/")))
      )))
  (lambda (msg . args)
    (case msg
      [(shutdown) (shut)]
      [(connect) (async-channel-put ach args)])))
