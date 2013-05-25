#lang racket/base
(require racket/format)
(provide (all-defined-out))

(define-logger macro-stepper)

(define (log-macro-stepper-time task msecs)
  (log-macro-stepper-debug
   (format "time for ~a: ~ams" task (~r msecs #:precision 0))))

(define-syntax-rule (with-log-time task body ...)
  (let ([time1 (current-inexact-milliseconds)])
    (begin0 (begin body ...)
      (let ([time2 (current-inexact-milliseconds)])
        (log-macro-stepper-time task (- time2 time1))))))

(define-syntax-rule (splicing-with-log-time task body ...)
  (begin (define time1 (current-inexact-milliseconds))
         body ...
         (define time2 (current-inexact-milliseconds))
         (define-values ()
           (begin0 (values)
             (log-macro-stepper-time task (- time2 time1))))))
