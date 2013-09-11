#lang racket/base

;;TODO use contract-req
(require "rep-utils.rkt" "free-variance.rkt" racket/contract/base)

(provide Filter/c FilterSet/c name-ref/c hash-name filter-equal?)

(define (Filter/c-predicate? e)
  (and (Filter? e) (not (NoFilter? e)) (not (FilterSet? e))))
(define Filter/c (flat-named-contract 'Filter Filter/c-predicate?))

(define FilterSet/c
  (flat-named-contract
   'FilterSet
   (λ (e) (or (FilterSet? e) (NoFilter? e)))))

;; A Name-Ref is any value that represents an object.
;; As an identifier, it represents a free variable in the environment
;; As a list, it represents a De Bruijn indexed bound variable
(define name-ref/c (or/c identifier? (list/c integer? integer?)))
(define (hash-name v) (if (identifier? v) (hash-id v) (list v)))

(def-filter Bot () [#:fold-rhs #:base])
(def-filter Top () [#:fold-rhs #:base])

;; TODO: t should only be a Type/c, but that leads to circular dependencies
(def-filter TypeFilter ([t Type?] [p (listof PathElem?)] [v name-ref/c])
  [#:intern (list (Rep-seq t) (map Rep-seq p) (hash-name v))]
  [#:frees (λ (f) (combine-frees (map f (cons t p))))]
  [#:fold-rhs (*TypeFilter (type-rec-id t) (map pathelem-rec-id p) v)])

;; TODO: t should only be a Type/c, but that leads to circular dependencies
(def-filter NotTypeFilter ([t Type?] [p (listof PathElem?)] [v name-ref/c])
  [#:intern (list (Rep-seq t) (map Rep-seq p) (hash-name v))]
  [#:frees (λ (f) (combine-frees (map f (cons t p))))]
  [#:fold-rhs (*NotTypeFilter (type-rec-id t) (map pathelem-rec-id p) v)])

;; implication
(def-filter ImpFilter ([a Filter/c] [c Filter/c]))

(def-filter AndFilter ([fs (non-empty-listof Filter/c)])
  [#:fold-rhs (*AndFilter (map filter-rec-id fs))]
  [#:frees (λ (f) (combine-frees (map f fs)))])

(def-filter OrFilter ([fs (non-empty-listof Filter/c)])
  [#:fold-rhs (*OrFilter (map filter-rec-id fs))]
  [#:frees (λ (f) (combine-frees (map f fs)))])

(def-filter FilterSet ([thn Filter/c] [els Filter/c])
  [#:fold-rhs (*FilterSet (filter-rec-id thn) (filter-rec-id els))])

;; represents no info about the filters of this expression
;; should only be used for parsing type annotations and expected types
(def-filter NoFilter () [#:fold-rhs #:base])

(define (filter-equal? a b) (= (Rep-seq a) (Rep-seq b)))
