#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (prefix-in c: (contract-req))
         (types subtype base-abbrev resolve current-seen)
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
;; The overlapping constraint is lifted if we are in the midst of subtyping. This is because during
;; subtyping calls to subtype are expensive.
(define (merge a b)
  (define b* (make-union* b))
  (match* (a b)
    ;; If a union element is a Name application, then it should not
    ;; be checked for subtyping since that can cause infinite
    ;; loops if this is called during type instantiation.
    [((App: (? Name? rator) rands stx) _)
     ;; However, we should check if it's a well-formed application
     ;; so that bad applications are rejected early.
     (resolve-app-check-error rator rands stx)
     (cons a b)]
    [(_ _) #:when (currently-subtyping?) (cons a b)]
    [((? (λ _ (subtype a b*))) _) b]
    [((? (λ _ (subtype b* a))) _) (list a)]
    [(_ _) (cons a (filter-not (λ (b-elem) (subtype b-elem a)) b))]))

;; Type -> List[Type]
(define (flat t)
  (match t
    [(Union: es) es]
    [_ (list t)]))

;; Union constructor
;; Normalizes representation by sorting types.
;; Type * -> Type
;; The input types can overlap and be union types
(define Un
  (case-lambda 
    [() -Bottom]
    [(t) t]
    [args 
     (define ts (foldr merge '()
                       (remove-dups (sort (append-map flat args) type<?))))
     (make-union* ts)]))
