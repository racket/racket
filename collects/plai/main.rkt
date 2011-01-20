#lang racket
(require plai/datatype
         plai/test-harness)

(provide (all-from-out plai/datatype)
         (except-out (all-from-out racket) error (for-syntax error) #%module-begin provide)
         (except-out (all-from-out plai/test-harness) plai-error)
         (rename-out [plai-error error] 
                     [plai-module-begin #%module-begin])
         (rename-out [plai-provide provide]))

(define-syntax (plai-provide stx)
  (raise-syntax-error #f "The PLAI language provides all defined names" stx))

(define-syntax (plai-module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     #`(#%module-begin
        (provide #,(datum->syntax stx '(all-defined-out)))
        body ...)]))
