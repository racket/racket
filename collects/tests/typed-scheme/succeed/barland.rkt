#lang typed-scheme
(define-type-alias top Any)
(define-type-alias set (top -> top))

(define: (autos [elt : top]) : top (memq elt '(vw saab bmw audi)))

(define: (element-of? [elt : top] [s : set]) : top (s elt))

;; CHANGE - use exact-integer?
(define: (evens [elt : top]) : top (and (exact-integer? elt) (even? elt)))

(define-typed-struct pr ([fst : top] [snd : top]))

#;(define: (length=2? [any : top]) : boolean
(and (pair? any)
     (pair? (cdr any))
     (empty? (cdr (cdr any)))))

(define: (cartesian-product [A : set] [B : set]) : set
  (lambda: ([elt : top])
	   (and (pr? elt)
		(element-of? (pr-fst elt) A)
		(element-of? (pr-snd elt) B))))



(define: evenEuroCars : set (cartesian-product evens autos))
#;(display (element-of? (make-pr 4 'bmw) evenEuroCars)) ; = #t
#;(display (element-of? (make-pr 'bmw 4) evenEuroCars)) ; = #f

