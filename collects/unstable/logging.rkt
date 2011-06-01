#lang racket/base

(require racket/contract)

;; Known limitations:
;; - If another thread is logging while t is running, these messages will be
;;   intercepted as well, even if they don't come from proc.
;; - In the following example:
;;     (with-logging-to-port port level
;;       (lambda () (log-warning "ok") 3))
;;     (log-warning "not ok")
;;   If the logging on the last line is executed before the thread listening
;;   to the logs sees the stop message, "not ok" will also be sent to port.
(define (with-intercepted-logging interceptor proc #:level [level 'debug])
  (let* ([logger    (make-logger #f (current-logger))]
         [receiver  (make-log-receiver logger level)]
         [stop-chan (make-channel)]
         [t (thread (lambda ()
                      (define (clear-events)
                        (let ([l (sync/timeout 0 receiver)])
                          (when l ; still something to read
                            (interceptor l) ; interceptor get the whole vector
                            (clear-events))))
                      (let loop ()
                        (let ([l (sync receiver stop-chan)])
                          (cond [(eq? l 'stop)
                                 ;; we received all the events we were supposed
                                 ;; to get, read them all (w/o waiting), then
                                 ;; stop
                                 (clear-events)]
                                [else ; keep going
                                 (interceptor l)
                                 (loop)])))))])
    (begin0
        (parameterize ([current-logger logger])
          (proc))
      (channel-put stop-chan 'stop) ; stop the receiver thread
      (thread-wait t))))

(define (with-logging-to-port port proc #:level [level 'debug])
  (with-intercepted-logging
   (lambda (l) (displayln (vector-ref l 1) ; actual message
                          port))
   proc #:level level))

(define level/c (or/c 'fatal 'error 'warning 'info 'debug))

(provide/contract [with-intercepted-logging
                   (->* ((-> (vector/c level/c string? any/c) any)
                         (-> any))
                        (#:level level/c)
                        any)]
                  [with-logging-to-port
                   (->* (output-port? (-> any))
                        (#:level level/c)
                        any)])
