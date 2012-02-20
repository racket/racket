#lang scheme/unit

(require "../utils/utils.rkt")
(require (rep type-rep)
	 (types utils union subtype remove-intersect resolve substitute abbrev)
         "signatures.rkt"
         racket/match mzlib/trace)

(import infer^)
(export restrict^)


;; NEW IMPL
;; restrict t1 to be a subtype of t2
;; if `f' is 'new, use t2 when giving up, otherwise use t1
(define (restrict* t1 t2 [f 'new])
  ;; we don't use union map directly, since that might produce too many elements
  (define (union-map f l)
    (match l
      [(Union: es)
       (let ([l (map f es)])
         (apply Un l))]))
  (match* (t1 t2) 
    [(_ (? (lambda _ (subtype t1 t2)))) t1] ;; already a subtype
    [(_ (Poly: vars t))
     (let ([subst (infer vars null (list t1) (list t) t1)])
       (and subst (restrict* t1 (subst-all subst t1) f)))]
    [((Union: _) _) (union-map (lambda (e) (restrict* e t2 f)) t1)]
    [(_ (Union: _)) (union-map (lambda (e) (restrict* t1 e f)) t2)]
    [((? needs-resolving?) _) (restrict* (resolve-once t1) t2 f)]
    [(_ (? needs-resolving?)) (restrict* t1 (resolve-once t2) f)]
    [(_ _)
     (cond [(subtype t2 t1) t2] ;; we don't actually want this - want something that's a part of t1
           [(not (overlap t1 t2)) (Un)] ;; there's no overlap, so the restriction is empty
           [else (if (eq? f 'new) t2 t1)])])) ;; t2 and t1 have a complex relationship, so we punt
    
(define restrict restrict*)
