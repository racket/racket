#lang racket

(require racket/date)

(provide with-logging-to)

(define (with-logging-to filename thunk)
  (call-with-output-file filename
    (λ (out)
      (thd-with-log thunk out))
    #:exists 'append))

(define (thd-with-log thunk log-port)
  (define benchmark-logger
    (make-logger #f (current-logger)))
  (define bmark-log-recv
    (make-log-receiver benchmark-logger 'info))
  (define handler (log-handler bmark-log-recv log-port))
  (parameterize ([current-logger benchmark-logger])
    (define body-thd
      (thread thunk))
    (let loop ()
      (sync
       (handle-evt body-thd
                   (λ (_)
                     (log-info "WAIT_FOR")
                     (loop)))
       (handle-evt bmark-log-recv 
                   (λ (log-evt) 
                     (cond
                       [(regexp-match? #rx"WAIT_FOR" (vector-ref log-evt 1))
                        (void)]
                       [else
                        (handler log-evt)
                        (loop)])))))))

(define (log-handler recv log-port)
  (λ (log-evt)
    (define msg (vector-ref log-evt 1))
    (unless
        (regexp-match? #rx"cm-accomplice" msg)
      (displayln (timestamp) log-port)
      (displayln (vector-ref log-evt 1) log-port))))

(define (timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (date->string (current-date) #t)))
