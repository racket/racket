#lang scheme

(define-syntax (try stx)
  (syntax-case stx ()
    [(_ e) #'(#%expression e)]
    [(_ e0 e ...)
     (syntax/loc stx
       (with-handlers* ([exn:fail? (lambda (x) (try e ...))])
         (#%expression e0)))]))

(provide try)
