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

(define-values (var-promote var-demote)
  (let ()
    (define (var-change V T change)
      (define (co t) (var-change V t change))
      (define (contra t) (var-change V t (not change)))
      (define (inv t)
        (if (V-in? V t)
            (if change Univ -Bottom)
            t))
      (type-case (#:Type co #:Filter (sub-f co)) T
             [#:F name (if (memq name V) (if change Univ -Bottom) T)]
             [#:Vector t (make-Vector (inv t))]
             [#:Box t (make-Box (inv t))]
             [#:Channel t (make-Channel (inv t))]
             [#:ThreadCell t (make-ThreadCell (inv t))]
             [#:Hashtable k v (make-Hashtable (inv k) (inv v))]
             [#:Param in out (make-Param (contra in) (co out))]
             [#:arr dom rng rest drest kws
                    (cond
                      [(apply V-in? V (get-filters rng))
                       (make-top-arr)]
                      [(and drest (memq (cdr drest) V))
                       (make-arr (map contra dom)
                                 (co rng)
                                 (contra (car drest))
                                 #f
                                 (map contra kws))]
                      [else
                       (make-arr (map contra dom)
                                 (co rng)
                                 (and rest (contra rest))
                                 (and drest (cons (contra (car drest)) (cdr drest)))
                                 (map contra kws))])]))
    (values
      (lambda (T V) (var-change V T #t))
      (lambda (T V) (var-change V T #f)))))
