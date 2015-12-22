#lang racket/base
(require "guts.rkt"
         "blame.rkt"
         "prop.rkt")
(provide (rename-out [struct-type-property/c* struct-type-property/c]))

(define (get-stpc-late-neg-proj stpc)
  (define get-late-neg-proj
    (get/build-late-neg-projection
     (struct-type-property/c-value-contract stpc)))
  (λ (input-blame)
    (define blame (blame-add-context input-blame "the struct property value of" #:swap? #t))
    (define late-neg-proj (get-late-neg-proj blame))
    (λ (x neg-party)
      (unless (struct-type-property? x)
        (raise-blame-error input-blame x #:neg-party
                           '(expected "struct-type-property" given: "~e")
                           x))
      (define-values (nprop _pred _acc)
        (make-struct-type-property
         (wrap-name x)
         (lambda (val _info)
           (late-neg-proj val neg-party))
         (list (cons x values))))
      nprop)))

(define (wrap-name x)
  (string->symbol (format "wrapped-~a" (object-name x))))

(struct struct-type-property/c (value-contract)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
             (build-contract-property
              #:name (lambda (c)
                       (build-compound-type-name
                        'struct-type-property/c
                        (struct-type-property/c-value-contract c)))
              #:first-order (lambda (c) struct-type-property?)
              #:late-neg-projection get-stpc-late-neg-proj))

(define struct-type-property/c*
  (let ([struct-type-property/c
         (lambda (value-contract)
           (struct-type-property/c
            (coerce-contract 'struct-type-property/c value-contract)))])
    struct-type-property/c))
