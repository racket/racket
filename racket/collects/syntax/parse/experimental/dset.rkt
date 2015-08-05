#lang racket/base

;; A dset is an `equal?`-based set, but it preserves order based on
;; the history of additions, so that if items are added in a
;; deterministic order, they come back out in a deterministic order.

(provide dset
         dset-empty?
         dset->list
         dset-add
         dset-union
         dset-subtract
         dset-filter)

(define dset
  (case-lambda
    [() (hash)]
    [(e) (hash e 0)]))

(define (dset-empty? ds)
  (zero? (hash-count ds)))

(define (dset->list ds)
  (map cdr
       (sort (for/list ([(k v) (in-hash ds)])
               (cons v k))
             <
             #:key car)))

(define (dset-add ds e)
  (if (hash-ref ds e #f)
      ds
      (hash-set ds e (hash-count ds))))

(define (dset-union ds1 ds2)
  (cond
   [((hash-count ds1) . > . (hash-count ds2))
    (dset-union ds2 ds1)]
   [else
    (for/fold ([ds2 ds2]) ([e (dset->list ds1)])
      (dset-add ds2 e))]))

(define (dset-subtract ds1 ds2)
  ;; ! takes O(size(ds2)) time !
  (for/fold ([r (dset)]) ([e (in-list (dset->list ds1))])
    (if (hash-ref ds2 e #f)
        r
        (dset-add r e))))

(define (dset-filter ds pred)
  (for/fold ([r (dset)]) ([e (in-list (dset->list ds))])
    (if (pred e)
        (dset-add r e)
        r)))
