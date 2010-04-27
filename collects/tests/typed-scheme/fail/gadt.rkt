#lang typed-scheme
#|
data Exp a =
Num :: Int -> Exp Int
Sum :: Int -> Int -> Exp Int
Zero :: Exp Int -> Exp Bool
|#
(define-type-alias number Number)
(define-type-alias boolean Boolean)
(define-type-alias top Any)

#;(define-typed-struct (a) Exp ([flag : a]))
(define-typed-struct (a) Num ([v : number]))
(define-typed-struct (a) Zero ([e : (Un (Num number) (Zero number))]))

(define-type-alias (Expr a) (Un (Num a) (Zero a)))

(pdefine: (a) (ev [x : (Expr a)]) : a
	  (cond
	   [(Num? x) (Num-v x)]
	   [(Zero? x) (= 0 #{(#{ev :: (All (b) ((Expr b) -> b))} #{(Zero-e x) :: (Expr number)}) :: number})]))

