#lang typed-scheme

(: map-with-funcs (All (b a ...) ((a ... a -> b) * -> (a ... a -> (Listof b)))))

(define (map-with-funcs . fs)
 (lambda as
   (map (lambda: ([f : (a ... a -> b)])
          (apply f as))
        fs)))

(ann (map-with-funcs + - * /) (Number Number * -> (Listof Number)))
