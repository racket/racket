#lang racket/base
(require "../syntax/syntax.rkt")

(provide rename-transformer?
         prop:rename-transformer
         make-rename-transformer
         rename-transformer-target)

(define-values (prop:rename-transformer rename-transformer? rename-transformer-value)
  (make-struct-type-property 'rename-transformer
                             (lambda (v info)
                               (unless (or (exact-nonnegative-integer? v)
                                           (identifier? v)
                                           (and (procedure? v)
                                                (procedure-arity-includes? v 1)))
                                 (raise-argument-error
                                  'guard-for-prop:rename-transformer
                                  (string-append "(or/c exact-nonnegative-integer?\n"
                                                 "      identifier?\n"
                                                 "      (procedure-arity-includes? proc 1))")
                                  v))
                               (when (exact-nonnegative-integer? v)
                                 (unless (v . <= . (list-ref info 1))
                                   (raise-arguments-error 'guard-for-prop:rename-transformer
                                                          "field index >= initialized-field count for structure type"
                                                          "field index" v
                                                          "initialized-field count" (list-ref info 1)))
                                 (unless (member v (list-ref info 5))
                                   (raise-arguments-error 'guard-for-prop:rename-transformer
                                                          "field index not declared immutable"
                                                          "field index" v)))
                               (define ref (list-ref info 3))
                               (cond
                                [(identifier? v) (lambda (t) v)]
                                [(integer? v)
                                 (lambda (t)
                                   (define val (ref t v))
                                   (if (identifier? val)
                                       val
                                       (datum->syntax #f '?)))]
                                [else (lambda (t)
                                        (define id (call-with-continuation-prompt
                                                    (lambda ()
                                                      (v t))))
                                        (unless (identifier? id)
                                          (raise-arguments-error 'prop:rename-transformer
                                                                 "contract violation for given value; expected an identifier"
                                                                 "given" id))
                                        id)]))))

(struct id-rename-transformer (id)
  #:property prop:rename-transformer 0
  #:reflection-name 'rename-transformer)

(define (make-rename-transformer id)
  (unless (identifier? id)
    (raise-argument-error 'make-rename-transformer "identifier?" id))
  (id-rename-transformer id))

(define (rename-transformer-target t)
  ((rename-transformer-value t) t))
