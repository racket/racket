#lang racket/base

(require racket/contract)

;; Known limitations:
;; - If another thread is logging while t is running, these messages will be
;;   sent to the port as well, even if they don't come from proc.
;; - In the following example:
;;     (with-logging-to-port port level
;;       (lambda () (log-warning "ok") 3))
;;     (log-warning "not ok")
;;   If the logging on the last line is executed before the thread listening
;;   to the logs sees the stop message, "not ok" will also be sent to port.
(define (with-logging-to-port port level proc)
  (let* ([logger    (make-logger #f (current-logger))]
         [receiver  (make-log-receiver logger level)]
         [stop-chan (make-channel)]
         [t (thread (lambda ()
                      (define (output-event l)
                        (displayln (vector-ref l 1) ; actual message
                                   port))
                      (define (clear-events)
                        (let ([l (sync/timeout 0 receiver)])
                          (when l ; still something to read
                            (output-event l)
                            (clear-events))))
                      (let loop ()
                        (let ([l (sync receiver stop-chan)])
                          (cond [(eq? l 'stop)
                                 ;; we received all the events we were supposed
                                 ;; to get, read them all (w/o waiting), then
                                 ;; stop
                                 (clear-events)]
                                [else ; keep going
                                 (output-event l)
                                 (loop)])))))])
    (begin0
        (parameterize ([current-logger logger])
          (proc))
      (channel-put stop-chan 'stop) ; stop the receiver thread
      (thread-wait t))))

(provide/contract [with-logging-to-port
                   (-> output-port?
                       (or/c 'fatal 'error 'warning 'info 'debug)
                       (-> any)
                       any)])
