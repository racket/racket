#lang racket/base

(provide (rename-out [-drr-undefined drr-undefined])
         drr-named-undefined
         drr-undefined?)

(struct drr-undefined (name) 
  #:methods gen:custom-write
  [(define (write-proc s p mode)
     (cond
       [(drr-undefined-name s)
        (display "#<drr's undefined: " p)
        (display (drr-undefined-name s) p)
        (display ">" p)]
       [else
        (display "#<drr's undefined>" p)]))])
(define -drr-undefined (drr-undefined #f))
(define (drr-named-undefined name)
  (unless (symbol? name)
    (raise-argument-error 'named-undefined "string?" name))
  (drr-undefined name))
(define-values (struct:undef make-undef undef? undef-ref undef-set!)
  (make-struct-type 'undefined #f 0 0))
