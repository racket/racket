#lang racket/base
(require racket/contract/base
         "private/geometry.rkt")

(provide/contract
 [struct point ([x real?] [y real?])]
 [struct line-string ([points (listof point?)])]
 [struct polygon ([exterior linear-ring?]
                  [interiors (listof linear-ring?)])]
 [struct multi-point ([elements (listof point?)])]
 [struct multi-line-string ([elements (listof line-string?)])]
 [struct multi-polygon ([elements (listof polygon?)])]
 [struct geometry-collection ([elements (listof geometry2d?)])]

 [line? (-> any/c boolean?)]
 [linear-ring? (-> any/c boolean?)]
 [geometry2d? (-> any/c boolean?)]

 [geometry->wkb
  (->* (geometry2d?)
       (#:big-endian? any/c)
       bytes?)]
 [wkb->geometry
  (->* (bytes?)
       (exact-nonnegative-integer?
        exact-nonnegative-integer?)
       geometry2d?)])
