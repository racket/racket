#;
(exn-pred 2)
#lang typed/scheme

(define-struct: parent ((x : Integer)))
(define-struct: (child1 parent) ((y : Integer)))
(define-struct: (child2 parent) ((y : Float)))

(: f (parent -> Integer))
(define (f x)
  (cond [(child1? x) (+ "a" "b")] ; rhs was considered dead code
        [(child2? x) 2]
        [else (error "eh?")]))

(f (make-child1 1 2))
(f (make-child2 1 2.0))
