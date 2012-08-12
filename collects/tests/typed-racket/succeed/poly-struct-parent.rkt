#lang typed/racket

(define-struct: (A) Box ([value : A]) #:transparent)
(define-struct: (A B) (Child-Box Box) ([other-value : B]) #:transparent)

(: v1 Symbol)
(define v1 (Box-value (Child-Box 'sym "str")))
(: v2 String)
(define v2 (Child-Box-other-value (Child-Box 'sym "str")))


(struct: (A) Box2 ([proc : (-> A)]))
(struct: (A) Strict-Box2 Box2 ())
(struct: (A) Mutable-Box2 Strict-Box2 ([value : (Vector A)]))

(: box-mutable2 (All (A) ((Box2 A) -> (Mutable-Box2 A))))
(define (box-mutable2 b)
  (define v ((Box2-proc b)))
  (define vs (vector v))
  (Mutable-Box2 (λ () (vector-ref vs 0)) vs))

(: box-strict2 (All (A) ((Box2 A) -> (Box2 A))))
(define (box-strict2 b)
  (cond
    [(Strict-Box2? b)  b]
    [else (box-mutable2 b)]))

(box-mutable2 (Box2 (λ () 0)))
(box-strict2 (box-mutable2 (Box2 (λ () 0))))
(ann (ann (box-mutable2 (Box2 (λ () 0))) (Mutable-Box2 Integer)) (Box2 Number))
