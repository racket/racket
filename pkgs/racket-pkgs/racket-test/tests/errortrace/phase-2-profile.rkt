#lang racket/base

(provide phase-2-profile-tests)

(define (phase-2-profile-tests)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (dynamic-require 'errortrace #f)
    ((dynamic-require 'errortrace 'profiling-enabled) #t)
    (eval
     '(module m racket/base
        (require (for-syntax racket/base))
        (begin-for-syntax
         (require (for-syntax racket/base))
         (define-syntax (a stx)
           (syntax-case stx () 
             [(_) #'123])))))))

(phase-2-profile-tests)
