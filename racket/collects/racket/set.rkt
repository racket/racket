#lang racket/base

(require racket/contract
         racket/private/set
         racket/private/set-types)

(provide (except-out (all-from-out racket/private/set)
                     primitive-set/c)
         (all-from-out racket/private/set-types)
         set/c)

(define (set/c elem/c
               #:cmp [cmp 'dont-care]
               #:kind [kind 'dont-care])
  (define cmp/c
    (case cmp
      [(dont-care) any/c]
      [(equal) set-equal?]
      [(eqv) set-eqv?]
      [(eq) set-eq?]
      [else (raise-arguments-error 'set/c
                                  "invalid #:cmp argument"
                                  "#:cmp argument" cmp)]))
  (define kind/c
    (case kind
      [(dont-care) any/c]
      [(mutable-or-weak) (or/c set-weak? set-mutable?)]
      [(mutable) set-mutable?]
      [(weak) set-weak?]
      [(immutable) set-immutable?]
      [else (raise-arguments-error 'set/c
                                   "invalid #:kind argument"
                                   "#:kind argument" kind)]))
  (case cmp
    [(eqv eq)
     (unless (flat-contract? elem/c)
       (raise-arguments-error
        'set/c
        "element contract must be a flat contract for eqv? and eq?-based sets"
        "element contract" (contract-name elem/c)
        "#:cmp option" cmp))]
    [else
     (unless (contract? elem/c)
       (raise-argument-error 'set/c "contract?" elem/c))])
  (define c
    (and/c (primitive-set/c elem/c)
           cmp/c
           kind/c))
  (define name
    `(set/c ,(contract-name elem/c)
            ,@(if (eq? cmp 'dont-care)
                  `[]
                  `[#:cmp (quote #,cmp)])
            ,@(if (eq? kind 'dont-care)
                  `[]
                  `[#:kind (quote #,kind)])))
  (rename-contract c name))

(define (rename-contract c name)
  (define make
    (cond
      [(flat-contract? c) make-flat-contract]
      [(chaperone-contract? c) make-chaperone-contract]
      [else make-contract]))
  (make
    #:name name
    #:first-order (contract-first-order c)
    #:projection
    (lambda (b)
      ((contract-projection c)
       (blame-add-context b #f)))))
