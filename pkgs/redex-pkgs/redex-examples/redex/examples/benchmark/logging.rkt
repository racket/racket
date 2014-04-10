#lang racket

(require racket/date)

(provide with-logging-to)

(define (with-logging-to filename thunk)
  (define benchmark-logger
    (make-logger #f (current-logger)))
  (define bmark-log-recv
    (make-log-receiver benchmark-logger 'info))
  (define handler (log-handler bmark-log-recv filename))
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

(define (log-handler recv filename)
  (λ (log-evt)
    (define msg (vector-ref log-evt 1))
    (unless (regexp-match? #rx"cm-accomplice" msg)
      (call-with-output-file filename
        (λ (log-port)
          (displayln (string-append (timestamp) " " msg)
                     log-port))
        #:exists 'append))))

(define (timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (date->string (current-date) #t)))
