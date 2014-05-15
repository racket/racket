#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (types abbrev union utils)
         racket/list racket/match)

(provide/cond-contract
  [var-promote (-> Type/c (listof symbol?) Type/c)]
  [var-demote (-> Type/c (listof symbol?) Type/c)])

(define (V-in? V . ts)
  (for/or ([e (in-list (append* (map fv ts)))])
    (memq e V)))

;; get-filters : SomeValues -> FilterSet
;; extract filters out of the range of a function type
(define (get-filters rng)
  (match rng
    [(AnyValues:) null]
    [(Values: (list (Result: _ lf _) ...)) lf]
    [(ValuesDots: (list (Result: _ lf _) ...) _ _) lf]))

(define (var-promote T V)
  (define (vp t) (var-promote t V))
  (define (inv t) (if (V-in? V t) Univ t))
  (type-case (#:Type vp #:Filter (sub-f vp)) T
             [#:F name (if (memq name V) Univ T)]
             [#:Vector t (make-Vector (inv t))]
             [#:Box t (make-Box (inv t))]
             [#:Channel t (make-Channel (inv t))]
             [#:ThreadCell t (make-ThreadCell (inv t))]
             [#:Hashtable k v (make-Hashtable (inv k) (inv v))]
             [#:Param in out
                      (make-Param (var-demote in V)
                                  (vp out))]
             [#:arr dom rng rest drest kws
                    (cond
                      [(apply V-in? V (get-filters rng))
                       (make-top-arr)]
                      [(and drest (memq (cdr drest) V))
                       (make-arr (for/list ([d (in-list dom)]) (var-demote d V))
                                 (vp rng)
                                 (var-demote (car drest) V)
                                 #f
                                 (for/list ([k (in-list kws)]) (var-demote k V)))]
                      [else
                       (make-arr (for/list ([d (in-list dom)]) (var-demote d V))
                                 (vp rng)
                                 (and rest (var-demote rest V))
                                 (and drest
                                      (cons (var-demote (car drest) V)
                                            (cdr drest)))
                                 (for/list ([k (in-list kws)]) (var-demote k V)))])]))

(define (var-demote T V)
  (define (vd t) (var-demote t V))
  (define (inv t) (if (V-in? V t) (Un) t))
  (type-case (#:Type vd #:Filter (sub-f vd)) T
             [#:F name (if (memq name V) (Un) T)]
             [#:Vector t (make-Vector (inv t))]
             [#:Box t (make-Box (inv t))]
             [#:Channel t (make-Channel (inv t))]
             [#:ThreadCell t (make-ThreadCell (inv t))]
             [#:Hashtable k v (make-Hashtable (inv k) (inv v))]
             [#:Param in out
                      (make-Param (var-promote in V)
                                  (vd out))]
             [#:arr dom rng rest drest kws
                    (cond
                      [(apply V-in? V (get-filters rng))
                       (make-top-arr)]
                      [(and drest (memq (cdr drest) V))
                       (make-arr (for/list ([d (in-list dom)]) (var-promote d V))
                                 (vd rng)
                                 (var-promote (car drest) V)
                                 #f
                                 (for/list ([k (in-list kws)]) (var-promote k V)))]
                      [else
                       (make-arr (for/list ([d (in-list dom)]) (var-promote d V))
                                 (vd rng)
                                 (and rest (var-promote rest V))
                                 (and drest
                                      (cons (var-promote (car drest) V)
                                            (cdr drest)))
                                 (for/list ([k (in-list kws)]) (var-promote k V)))])]))
