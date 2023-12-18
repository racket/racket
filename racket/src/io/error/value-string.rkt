#lang racket/base

(provide error-value->string
         register-error-value->string!)

(define saved-error-value->string #f)

(define (error-value->string v)
  (if saved-error-value->string
      (saved-error-value->string v)
      ((error-value->string-handler) v (error-print-width))))

(define (register-error-value->string! proc)
  (unless saved-error-value->string
    (set! saved-error-value->string proc)))
