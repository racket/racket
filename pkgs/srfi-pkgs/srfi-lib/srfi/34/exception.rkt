#lang racket/base
(require (for-syntax racket/base
                     syntax/stx))

;; SRFI 34 for PLT Scheme
;; Zhu Chongkai, April 2005
;; <mrmathematica@yahoo.com>

(provide with-exception-handler
         guard
         raise)
  
(define-syntax with-exception-handler
  (syntax-rules ()
    ((_ handler thunk)
     (with-handlers (((lambda (exn) #t) handler)) (thunk)))))

(define-syntax (guard stx)
  (syntax-case stx ()
    ((_ (var clause ... (els de ...)) e1 e2 ...)
     (and (identifier? #'els)
          (module-or-top-identifier=? #'els #'else))
     (syntax/loc stx
       (with-handlers (((lambda (exn) #t)
                        (lambda (var) (cond clause ...
                                            (else de ...)))))
         e1 e2 ...)))
    ((_ (var clause ...) e1 e2 ...)
     (syntax/loc stx
       (with-handlers (((lambda (exn) #t)
                        (lambda (var) (cond clause ...
                                            (else (raise var))))))
         e1 e2 ...)))))
