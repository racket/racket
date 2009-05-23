#lang typed-scheme

(: f (Number String -> Number))
(define (f x z) #;(f x z) 7)
(lambda: ([x : Any] [y : Any]) (values (number? y) (number? x)))
(lambda: ([x : Any] [y : Any]) (values (number? x) (number? y)))
(lambda: ([x : Any] [y : Any]) (values (and (number? x) (boolean? y)) (number? y)))
(lambda: ([x : Any]) (values (number? x) (number? x)))
(: g (Any -> Boolean : Number))
(define g (lambda: ([x : Any]) (number? x)))
(: q ((Number -> Number) -> Number))
(define q (lambda: ([x : (Number -> Number)]) (x 1)))
;(q (lambda (z) (f z "foo")))

(: p (Number * -> Number))
(define (p . x) 7)

(lambda x (number? x))
(+)
(+ 1 2 3)
(+ 1 2 3.5)

(define-struct: (Z) X ([y : Z]))
(define:  my-x : (X Number) (make-X 1))
(X-y my-x)

; FIXME - doesn't work yet
(number? (X-y my-x))
(if (number? (X-y my-x)) (+ 1 (X-y my-x)) 7)


(define: (f2) : (U) (error 'foo))
(lambda: ([x : Number]) #{((f2)) :: (U)})

(: f3 (U (Number -> Number) (Number -> String)))
(define (f3 x) 7)

(define: x : (List Any Any) (list 1 23 ))
(car x)
(if (number? (car x)) (add1 (car #{x :: (Pair Number Any)})) 7)
(if (number? (car x)) (add1 (car x)) 7)

;; error
;(f 12 "hi")

(map + (list 1 2 3))
(map + (list 1 2 3) (list 1 2 3))
;; error
;(map + (list 1 2 3) (list 1 2 "foo"))

((lambda (a b . c) (+ a b (car c))) 1 2 3 4)

