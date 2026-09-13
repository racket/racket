#lang racket/base
(require "config.rkt"
         "print.rkt")

(provide call-in-pkg-timeout-sandbox)

(define (call-in-pkg-timeout-sandbox thunk
                                     #:make-exn [make-exn exn:fail])
  (define c (make-custodian))
  (define timeout (get-network-timeout))
  (define fail-reason #f)
  (define th
    (parameterize ([current-custodian c])
      (thread (lambda ()
                (with-handlers ([(lambda (x) #t)
                                 (lambda (exn)
                                   (set! fail-reason (list exn))
                                   (custodian-shutdown-all c))])
                  (thunk)))
              #:keep 'results)))
  (define timeout?
    (not (sync/timeout timeout th)))
  (custodian-shutdown-all c)
  (thread-wait th (lambda ()
                    (cond
                      [fail-reason
                       (raise (car fail-reason))]
                      [timeout?
                       (log-pkg-debug "failed due to timeout")
                       (raise
                        (make-exn (format "timeout after ~a seconds" timeout)
                                  (current-continuation-marks)))]
                      [else (error "thread terminated")]))))
