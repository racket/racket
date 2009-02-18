#lang scheme/base

(require mzlib/plt-match)
(require mzlib/etc)
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


(dlf LBot () [#:fold-rhs #:base])

(dlf LTypeFilter ([t Type?] [p (listof PathElem?)])
  [#:frees (combine-frees (map free-vars* (cons t p)))
	   (combine-frees (map free-idxs* (cons t p)))]
  [#:fold-rhs (*LTypeFilter (type-rec-id t) (map pathelem-rec-id p))])

(dlf LNotTypeFilter ([t Type?] [p (listof PathElem?)])
  [#:frees (combine-frees (map free-vars* (cons t p)))
	   (combine-frees (map free-idxs* (cons t p)))]
  [#:fold-rhs (*LNotTypeFilter (type-rec-id t) (map pathelem-rec-id p))])
