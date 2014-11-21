#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (types abbrev utils structural)
         (prefix-in c: (contract-req))
         racket/performance-hint
         racket/list racket/match)

(provide/cond-contract
  [var-promote (c:-> Type/c (c:listof symbol?) Type/c)]
  [var-demote (c:-> Type/c (c:listof symbol?) Type/c)])

(define (V-in? V . ts)
  (for/or ([e (in-list (append* (map fv ts)))])
    (memq e V)))

;; get-filters : SomeValues -> FilterSet
;; extract filters out of the range of a function type
(define (get-filters rng)
  (match rng
    [(AnyValues: f) (list (-FS f f))]
    [(Values: (list (Result: _ lf _) ...)) lf]
    [(ValuesDots: (list (Result: _ lf _) ...) _ _) lf]))


(begin-encourage-inline
  (define (var-change V T change)
    (define (structural-recur t sym)
      (case sym
        [(co) (var-change V t change)]
        [(contra) (var-change V t (not change))]
        [(inv)
         (if (V-in? V t)
             (if change Univ -Bottom)
             t)]))
    (define (co t) (structural-recur t 'co))
    (define (contra t) (structural-recur t 'contra))

    ;; arr? -> (or/c #f arr?)
    ;; Returns the changed arr or #f if there is no arr above it
    (define (arr-change arr)
      (match arr
        [(arr: dom rng rest drest kws)
         (cond
           [(apply V-in? V (get-filters rng))
            #f]
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

    (match T
      [(F: name) (if (memq name V) (if change Univ -Bottom) T)]
      [(Function: arrs)
       (make-Function (filter-map arr-change arrs))]
      [(? structural?) (structural-map T structural-recur)]
      [(? Filter?) ((sub-f co) T)]
      [(? Object?) ((sub-o co) T)]
      [(? Type?) ((sub-t co) T)]))
  (define (var-promote T V)
    (var-change V T #t))
  (define (var-demote T V)
    (var-change V T #f)))
