#lang racket/base

(provide set!-transformer?
         prop:set!-transformer
         make-set!-transformer
         set!-transformer-procedure)

(define-values (prop:set!-transformer set!-transformer? set!-transformer-value)
  (make-struct-type-property 'set!-transformer
                             (lambda (v info)
                               (unless (or (and (procedure? v)
                                                (or (procedure-arity-includes? v 1)
                                                    (procedure-arity-includes? v 2)))
                                           (exact-nonnegative-integer? v))
                                 (raise-argument-error
                                  'guard-for-prop:set!-transformer
                                  (string-append "(or/c (procedure-arity-includes? proc 1)\n"
                                                 "      (procedure-arity-includes? proc 2)\n"
                                                 "      exact-nonnegative-integer?)")
                                  v))
                               (when (exact-nonnegative-integer? v)
                                 (unless (v . <= . (list-ref info 1))
                                   (raise-arguments-error 'guard-for-prop:set!-transformer
                                                          "field index >= initialized-field count for structure type"
                                                          "field index" v
                                                          "initialized-field count" (list-ref info 1)))
                                 (unless (member v (list-ref info 5))
                                   (raise-arguments-error 'guard-for-prop:set!-transformer
                                                          "field index not declared immutable"
                                                          "field index" v)))
                               (define ref (list-ref info 3))
                               (cond
                                [(integer? v) (lambda (t)
                                                (define p (ref t v))
                                                (if (and (procedure? p)
                                                         (procedure-arity-includes? p 1))
                                                    p
                                                    (lambda (s) (error "bad syntax:" s))))]
                                [else (lambda (t) v)]))))

(define make-set!-transformer
  (let ()
    (struct set!-transformer (proc)
      #:property prop:set!-transformer 0)
    (lambda (proc)
      (unless (and (procedure? proc)
                   (procedure-arity-includes? proc 1))
        (raise-argument-error 'make-set!-transformer
                              "(procedure-arity-includes/c 1)"
                              proc))
      (set!-transformer proc))))

(define (set!-transformer-procedure t)
  (define v ((set!-transformer-value t) t))
  (if (procedure-arity-includes? v 1)
      v
      (lambda (s) (v t s))))
