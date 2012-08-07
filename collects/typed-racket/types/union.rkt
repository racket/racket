#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep)
         (prefix-in c: (contract-req))
         (types subtype)
         racket/match
         racket/list)


(provide/cond-contract
 [Un (() #:rest (c:listof Type/c) . c:->* . Type/c)])

;; List[Type] -> Type
;; Argument types should not overlap or be union types
(define (make-union* types)
  (match types
    [(list t) t]
    [_ (make-Union types)]))

;; a is a Type (not a union type)
;; b is a List[Type] (non overlapping, non Union-types)
;; The output is a non overlapping list of non Union types.
(define (merge a b)
  (define b* (make-union* b))
  (cond
    [(subtype a b*) b]
    [(subtype b* a) (list a)]
    [else (cons a b)]))

;; Type -> List[Type]
(define (flat t)
  (match t
    [(Union: es) es]
    [_ (list t)]))

(define empty-union (make-Union null))

;; Union constructor
;; Normalizes representation by sorting types.
;; Type * -> Type
;; The input types can overlap and be union types
(define Un
  (case-lambda 
    [() empty-union]
    [(t) t]
    [args 
     (define ts (foldr merge '()
                       (remove-dups (sort (append-map flat args) type<?))))
     (make-union* ts)]))
