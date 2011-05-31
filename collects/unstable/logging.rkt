#lang racket/base

(require racket/contract)

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
