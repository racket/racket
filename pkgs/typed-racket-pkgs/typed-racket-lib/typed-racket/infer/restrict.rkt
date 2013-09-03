#lang racket/unit

(require "../utils/utils.rkt")
(require (rep type-rep)
         (types union subtype remove-intersect resolve)
         "signatures.rkt"
         racket/match)

(import infer^)
(export restrict^)

;; we don't use union map directly, since that might produce too many elements
  (define (union-map f l)
    (match l
      [(Union: es)
       (let ([l (map f es)])
         (apply Un l))]))

;; NEW IMPL
;; restrict t1 to be a subtype of t2
;; if `f' is 'new, use t2 when giving up, otherwise use t1
(define (restrict* t1 t2 [f 'new])
  (cond
    [(subtype t1 t2) t1] ;; already a subtype
    [(match t2 
       [(Poly: vars t)
        (and (infer vars null (list t1) (list t) #f) t1)]
       [_ #f])]
    [(Union? t1) (union-map (lambda (e) (restrict* e t2 f)) t1)]
    [(Union? t2) (union-map (lambda (e) (restrict* t1 e f)) t2)]
    [(needs-resolving? t1) (restrict* (resolve-once t1) t2 f)]
    [(needs-resolving? t2) (restrict* t1 (resolve-once t2) f)]
    [(subtype t2 t1) t2] ;; we don't actually want this - want something that's a part of t1
    [(not (overlap t1 t2)) (Un)] ;; there's no overlap, so the restriction is empty
    [else (if (eq? f 'new) t2 t1)])) ;; t2 and t1 have a complex relationship, so we punt

(define restrict restrict*)
