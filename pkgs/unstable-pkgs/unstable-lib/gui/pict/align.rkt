#lang racket/base
(require pict
         racket/contract/base)

(define (pin-over/align scene x y halign valign pict)
  (let ([localrefx (* (pict-width pict) (align->frac halign))]
        [localrefy (* (pict-height pict) (align->frac valign))])
    (pin-over scene (- x localrefx) (- y localrefy) pict)))

(define (align->frac align)
  (case align
    ((t l)   0)
    ((c)   1/2)
    ((b r)   1)))

(define (align->h align)
  (case align
    ((lt lc lb) 'l)
    ((ct cc cb) 'c)
    ((rt rc rb) 'r)))

(define (align->v align)
  (case align
    ((lt ct rt) 't)
    ((lc cc rc) 'c)
    ((lb cb rb) 'b)))

(define (halign->vcompose halign)
  (case halign
    ((l) vl-append)
    ((c) vc-append)
    ((r) vr-append)))

(define (valign->hcompose align)
  (case align
    ((t) ht-append)
    ((c) hc-append)
    ((b) hb-append)))

(define align/c
  (or/c 'lt 'ct 'rt
        'lc 'cc 'rc
        'lb 'cb 'rb))
(define halign/c
  (or/c 'l 'c 'r))
(define valign/c
  (or/c 't 'c 'b))

(provide
 (contract-out
  ;; xxx more specific
  [halign->vcompose (-> halign/c procedure?)]
  [valign->hcompose (-> valign/c procedure?)]
  [pin-over/align (-> pict? real? real? halign/c valign/c pict? pict?)]
  [align->frac (-> (or/c halign/c valign/c) real?)]
  [align/c contract?]
  [halign/c contract?]
  [align->h (-> align/c halign/c)]
  [valign/c contract?]
  [align->v (-> align/c valign/c)]))
