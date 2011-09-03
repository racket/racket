#lang scheme/unit

(require "../utils/utils.rkt")
(require (rep type-rep)
	 (types utils union subtype remove-intersect resolve substitute)
         "signatures.rkt"
         racket/match mzlib/trace)

(import infer^)
(export restrict^)


;; NEW IMPL
;; restrict t1 to be a subtype of t2
(define (restrict* t1 t2)
  ;; we don't use union map directly, since that might produce too many elements
  (define (union-map f l)
    (match l
      [(Union: es)
       (let ([l (map f es)])
         (apply Un l))]))
  (cond
    [(subtype t1 t2) t1] ;; already a subtype
    [(match t2
       [(Poly: vars t)
        (let ([subst (infer vars null (list t1) (list t) t1)])
          (and subst (restrict* t1 (subst-all subst t1))))]
       [_ #f])]
    [(Union? t1) (union-map (lambda (e) (restrict* e t2)) t1)]
    [(needs-resolving? t1) (restrict* (resolve-once t1) t2)]
    [(needs-resolving? t2) (restrict* t1 (resolve-once t2))]
    [(subtype t2 t1) t2] ;; we don't actually want this - want something that's a part of t1
    [(not (overlap t1 t2)) (Un)] ;; there's no overlap, so the restriction is empty
    [else t2] ;; t2 and t1 have a complex relationship, so we punt
    ))
(define restrict restrict*)
;(trace restrict*)
