#lang typed/racket/base

(require racket/match racket/list)

(provide check-type-declarations)

(define-type type-declaration #t)
(define-type type-environment #t)

(define-type dag (HashTable Symbol (U Symbol #f)))

(: check-type-declarations ((Listof type-declaration) type-environment -> Void))
(define (check-type-declarations types env)
  (: compute-reference-dag ((Listof type-declaration) dag -> dag))
  (define (compute-reference-dag types dag)
      (compute-reference-dag types dag))

  (: cycle-exists? (Symbol dag -> Boolean))
  (define (cycle-exists? symbol a-dag)
      (cycle-exists? symbol a-dag))

  (: reference-dag dag)
  (: cycle Boolean)
  (define reference-dag (compute-reference-dag types (make-immutable-hash empty)))
  (define cycle (cycle-exists? 'name reference-dag))
  (void))
