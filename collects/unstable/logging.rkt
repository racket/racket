#lang racket/base

(require racket/contract)

(define (with-intercepted-logging interceptor proc #:level [level 'debug])
  (let* ([orig-logger (current-logger)]
         ;; the new logger is unrelated to the original, to avoid getting
         ;; messages sent to it that didn't originate from proc
         [logger      (make-logger)]
         [receiver    (make-log-receiver logger level)]
         [stop-chan   (make-channel)]
         [t (thread (lambda ()
                      (define (intercept l)
                        ;; we want to send l to the original logger, so that
                        ;; the rest of the system can see it too.
                        (log-message orig-logger
                                     (vector-ref l 0)  ; level
                                     (vector-ref l 1)  ; message
                                     (vector-ref l 2)) ; data
                        (interceptor l))
                      (define (clear-events)
                        (let ([l (sync/timeout 0 receiver)])
                          (when l ; still something to read
                            (intercept l) ; interceptor get the whole vector
                            (clear-events))))
                      (let loop ()
                        (let ([l (sync receiver stop-chan)])
                          (cond [(eq? l 'stop)
                                 ;; we received all the events we were supposed
                                 ;; to get, read them all (w/o waiting), then
                                 ;; stop
                                 (clear-events)]
                                [else ; keep going
                                 (intercept l)
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
