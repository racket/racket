#lang racket/base
(require "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "misc.rkt")
(provide (rename-out [struct-type-property/c* struct-type-property/c]))

(define (get-stpc-proj stpc)
  (let ([get-val-proj
         (contract-projection
          (struct-type-property/c-value-contract stpc))])
    (lambda (blame)
      (let ([val-proj (get-val-proj (blame-swap blame))])
        (lambda (x)
          (unless (struct-type-property? x)
            (raise-blame-error blame x
                               '(expected "struct-type-property" given: "~e")
                               x))
          (let-values ([(nprop _pred _acc)
                        (make-struct-type-property
                         (wrap-name x)
                         (lambda (val _info)
                           (val-proj val))
                         (list (cons x values)))])
            nprop))))))

(define (wrap-name x)
  (string->symbol (format "wrapped-~a" (object-name x))))

(struct struct-type-property/c (value-contract)
  #:property prop:contract
             (build-contract-property
              #:name (lambda (c)
                       (build-compound-type-name
                        'struct-type-property/c
                        (struct-type-property/c-value-contract c)))
              #:first-order (lambda (c) struct-type-property?)
              #:projection get-stpc-proj))

(define struct-type-property/c*
  (let ([struct-type-property/c
         (lambda (value-contract)
           (struct-type-property/c
            (coerce-contract 'struct-type-property/c value-contract)))])
    struct-type-property/c))
