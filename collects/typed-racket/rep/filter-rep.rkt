#lang racket/base

(require "rep-utils.rkt" "free-variance.rkt" racket/contract/base)

(define (Filter/c-predicate? e)
  (and (Filter? e) (not (NoFilter? e)) (not (FilterSet? e))))
(define Filter/c (flat-named-contract 'Filter Filter/c-predicate?))

(define FilterSet/c
  (flat-named-contract
   'FilterSet
   (λ (e) (or (FilterSet? e) (NoFilter? e)))))

(provide Filter/c FilterSet/c name-ref/c hash-name)

(define name-ref/c (or/c identifier? integer?))
(define (hash-name v) (if (identifier? v) (hash-id v) (list v)))

(def-filter Bot () [#:fold-rhs #:base])
(def-filter Top () [#:fold-rhs #:base])

(def-filter TypeFilter ([t Type/c] [p (listof PathElem?)] [v name-ref/c])
  [#:intern (list (Rep-seq t) (map Rep-seq p) (hash-name v))]
  [#:frees (λ (f) (combine-frees (map f (cons t p))))]
  [#:fold-rhs (*TypeFilter (type-rec-id t) (map pathelem-rec-id p) v)])

(def-filter NotTypeFilter ([t Type/c] [p (listof PathElem?)] [v name-ref/c])
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

(def-filter FilterSet (thn els)
  [#:contract (->i ([t any/c]
                    [e any/c])
                   (#:syntax [stx #f])
                   #:pre (t e)
                   (and (cond [(Bot? t) #t]
                              [(Bot? e) (Top? t)]
                              [else (Filter/c-predicate? t)])
                        (cond [(Bot? e) #t]
                              [(Bot? t) (Top? e)]
                              [else (Filter/c-predicate? e)]))
                   [result FilterSet?])]
  [#:fold-rhs (*FilterSet (filter-rec-id thn) (filter-rec-id els))])

;; represents no info about the filters of this expression
;; should only be used for parsing type annotations and expected types
(def-filter NoFilter () [#:fold-rhs #:base])

(define (filter-equal? a b) (= (Rep-seq a) (Rep-seq b)))
(provide filter-equal?)
