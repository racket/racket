#lang scheme/base

(require scheme/match scheme/contract "rep-utils.ss" "free-variance.ss")

(dpe CarPE () [#:frees #f] [#:fold-rhs #:base])
(dpe CdrPE () [#:frees #f] [#:fold-rhs #:base])
(dpe StructPE ([t Type?] [idx natural-number/c])     
    [#:frees (free-vars* t) (free-idxs* t)]
    [#:fold-rhs (*StructPE (type-rec-id t) idx)])

(do Bot () [#:frees #f] [#:fold-rhs #:base])

(do Path ([p (listof PathElem?)] [v identifier?])
  [#:intern (list p (hash-id v))]
  [#:frees (combine-frees (map free-vars* p)) (combine-frees (map free-idxs* p))]
  [#:fold-rhs (*Path (map pathelem-rec-id t) v)])

(dlo LBot () [#:frees #f] [#:fold-rhs #:base])

(dlo LPath ([p (listof PathElem?)] [idx natural-number/c])
  [#:frees (combine-frees (map free-vars* p)) (combine-frees (map free-idxs* p))]
  [#:fold-rhs (*LPath (map pathelem-rec-id t) idx)])