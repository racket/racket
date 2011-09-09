#lang racket/base

(require "rep-utils.rkt" "free-variance.rkt" "filter-rep.rkt" "../utils/utils.rkt" (contract-req))
(provide object-equal?)

(def-pathelem CarPE () [#:fold-rhs #:base])
(def-pathelem CdrPE () [#:fold-rhs #:base])
(def-pathelem SyntaxPE () [#:fold-rhs #:base])
;; t is always a Name (can't put that into the contract b/c of circularity)
(def-pathelem StructPE ([t Type?] [idx natural-number/c])
  [#:frees (free-vars* t) (free-idxs* t)]
  [#:fold-rhs (*StructPE (type-rec-id t) idx)])

(def-object Empty () [#:fold-rhs #:base])

(def-object Path ([p (listof PathElem?)] [v name-ref/c])
  [#:intern (list (map Rep-seq p) (hash-name v))]
  [#:frees (combine-frees (map free-vars* p)) (combine-frees (map free-idxs* p))]
  [#:fold-rhs (*Path (map pathelem-rec-id p) v)])

;; represents no info about the object of this expression
;; should only be used for parsing type annotations and expected types
(def-object NoObject () [#:fold-rhs #:base])

(define (object-equal? o1 o2) (= (Rep-seq o1) (Rep-seq o2)))

#|
(dlo LEmpty () [#:fold-rhs #:base])

(dlo LPath ([p (listof PathElem?)] [idx index/c])
  [#:frees (combine-frees (map free-vars* p)) (combine-frees (map free-idxs* p))]
  [#:fold-rhs (*LPath (map pathelem-rec-id p) idx)])
|#
