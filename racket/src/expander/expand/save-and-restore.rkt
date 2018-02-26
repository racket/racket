#lang racket/base
(require (for-syntax racket/base))

(provide with-save-and-restore)

(define-syntax (with-save-and-restore stx)
  (syntax-case stx ()
    [(_ ([id init-val] ...) body0 body ...)
     (with-syntax ([(old-id ...) (generate-temporaries #'(id ...))]
                   [(new-id ...) (generate-temporaries #'(id ...))])
       #'(let ([old-id id] ...
               [new-id init-val] ...)
           (dynamic-wind
            (lambda () (set! id new-id) ...)
            (lambda () body0 body ...)
            (lambda () (set! id old-id) ...))))]))

           
