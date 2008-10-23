#lang scheme

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ term ...)
     (syntax/loc stx
       (#%module-begin
        (define result (compute '(term ...)))
        (provide result)))]))

(define compute
  (match-lambda
    [(? rational? const) const]
    [(list add ...) (compute-additive add)]))

(define compute-additive
  (match-lambda
    [(list pre ... '+ post ...)
     (+ (compute-additive pre)
        (compute-additive post))]
    [(list pre ... '- post ...)
     (- (compute-additive pre)
        (compute-additive post))]
    [(list mul ...) (compute-multiplicative mul)]))

(define compute-multiplicative
  (match-lambda
    [(list pre ... '* post ...)
     (* (compute-multiplicative pre)
        (compute-multiplicative post))]
    [(list pre ... '/ post ...)
     (/ (compute-multiplicative pre)
        (compute-multiplicative post))]
    [(list term) (compute term)]))

(provide (rename-out [module-begin #%module-begin]))
