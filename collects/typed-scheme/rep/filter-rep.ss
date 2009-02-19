#lang scheme/base

(require scheme/match scheme/contract)
(require "rep-utils.ss" "free-variance.ss")

(df Bot () [#:fold-rhs #:base])

(df TypeFilter ([t Type?] [p (listof PathElem?)] [v identifier?])
  [#:intern (list t p (hash-id v))]
  [#:frees (combine-frees (map free-vars* (cons t p)))
	   (combine-frees (map free-idxs* (cons t p)))]
  [#:fold-rhs (*TypeFilter (type-rec-id t) (map pathelem-rec-id p) v)])

(df NotTypeFilter ([t Type?] [p (listof PathElem?)] [v identifier?])
  [#:intern (list t p (hash-id v))]
  [#:frees (combine-frees (map free-vars* (cons t p)))
	   (combine-frees (map free-idxs* (cons t p)))]
  [#:fold-rhs (*NotTypeFilter (type-rec-id t) (map pathelem-rec-id p) v)])

(df FilterSet ([thn (listof (and/c Filter? (not/c FilterSet?)))]
	       [els (listof (and/c Filter? (not/c FilterSet?)))])
     [#:frees (combine-frees (map free-vars* (append thn els)))
	      (combine-frees (map free-idxs* (append thn els)))]
     [#:fold-rhs (*FilterSet (map filter-rec-id thn) (map filter-rec-id els))])

(define index/c (or/c natural-number/c keyword?))

(dlf LBot () [#:fold-rhs #:base])

(dlf LTypeFilter ([t Type?] [p (listof PathElem?)] [idx index/c])
  [#:frees (lambda (frees*) (combine-frees (map (compose make-invariant frees*) (cons t p))))]
  [#:fold-rhs (*LTypeFilter (type-rec-id t) (map pathelem-rec-id p) idx)])

(dlf LNotTypeFilter ([t Type?] [p (listof PathElem?)] [idx index/c])
  [#:frees (lambda (frees*) (combine-frees (map (compose make-invariant frees*) (cons t p))))]
  [#:fold-rhs (*LNotTypeFilter (type-rec-id t) (map pathelem-rec-id p) idx)])

(dlf LFilterSet ([thn (listof (and/c LatentFilter? (not/c LFilterSet?)))]
		 [els (listof (and/c LatentFilter? (not/c LFilterSet?)))])
     [#:frees (combine-frees (map free-vars* (append thn els)))
	      (combine-frees (map free-idxs* (append thn els)))]
     [#:fold-rhs (*LFilterSet (map latentfilter-rec-id thn) (map latentfilter-rec-id els))])
