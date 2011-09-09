#;
(exn-pred 3)
#lang typed-scheme

(: map-with-funcs (All (b a ...) ((a ... a -> b) * -> (a ... a -> (Listof b)))))

(define (map-with-funcs . fs)
 (lambda as
   (map (lambda: ([f : (a ... a -> b)])
          (apply f as))
        fs)))

(ann (map-with-funcs + - * /) (Number Number * -> (Listof Integer)))

(ann (map-with-funcs + - * /) (Number * -> (Listof Number)))

(ann (map-with-funcs + - * /) (Integer * -> (Listof Number)))
