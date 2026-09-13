#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         ffi2
         rackunit)

(define-ffi2-type (offset_double_t delta) double_t
  #:c->racket (lambda (v) (+ v delta))
  #:racket->c (lambda (v) (- v delta)))

(define-ffi2-type-syntax num_t
  (lambda (stx)
    (if (identifier? stx)
        #'int64_t
        (syntax-parse stx
          [(_ #:double) #'double_t]
          [(_ #:int) #'int64_t]
          [(_ #:offset delta) #'(offset_double_t delta)]
          [(_ #:wrong) "oops"]))))

(define ip (ffi2-malloc (num_t #:int)))
(ffi2-set! ip (num_t #:int) 0)
(check-equal? (ffi2-ref ip num_t) 0)
(check-equal? (ffi2-ref ip (num_t #:double)) 0.0)
(check-equal? (ffi2-ref ip (num_t #:offset 4.0)) 4.0)
(check-exn (lambda (exn)
             (and (exn:fail:syntax? exn)
                  (regexp-match? #rx"invalid expansion of type syntax" (exn-message exn))))
           (lambda ()
             (parameterize ([current-namespace (variable-reference->namespace
                                                (#%variable-reference))])
               (eval '(ffi2-ref ip (num_t #:wrong))))))
