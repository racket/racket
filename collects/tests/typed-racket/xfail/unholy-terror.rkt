#lang typed-scheme

;; Changes to the numeric tower cause this test to generate a ridiculous
;; number of inference constraints. Inference takes 5+ minutes.

(apply (plambda: (a ...) [ys : (a ... a -> Number) *]
         (lambda: [zs : a ... a]
            (map (lambda: ([y : (a ... a -> Number)])
                   (apply y zs))
                 ys)))
       (list (lambda: ([x : Number] [y : Number]) (+ x y))
             (lambda: ([x : Number] [y : Number]) (- x y))
             (lambda: ([x : Number] [y : Number]) (* x y))
             (lambda: ([x : Number] [y : Number]) (/ x y))))

((apply (plambda: (a ...) [ys : (a ... a -> Number) *]
          (lambda: [zs : a ... a]
            (map (lambda: ([y : (a ... a -> Number)])
                   (apply y zs))
                 ys)))
        (list (lambda: ([x : Number] [y : Number]) (+ x y))
              (lambda: ([x : Number] [y : Number]) (- x y))
              (lambda: ([x : Number] [y : Number]) (* x y))
              (lambda: ([x : Number] [y : Number]) (/ x y))))
 3 4)

(apply (plambda: (a ...) [ys : (a ... a -> Number) *]
         (lambda: [zs : a ... a]
                  (map (lambda: ([y : (a ... a -> Number)])
                                (apply y zs))
                       ys)))
       (list + - * /))

((plambda: (a ...) [ys : (a ... a -> Number) *]
   (lambda: [zs : a ... a]
     (map (lambda: ([y : (a ... a -> Number)])
            (apply y zs))
          ys)))
 (lambda: ([x : Number] [y : Number]) (+ x y))
 (lambda: ([x : Number] [y : Number]) (- x y))
 (lambda: ([x : Number] [y : Number]) (* x y))
 (lambda: ([x : Number] [y : Number]) (/ x y)))

(((plambda: (a ...) [ys : (a ... a -> Number) *]
    (lambda: [zs : a ... a]
      (map (lambda: ([y : (a ... a -> Number)])
             (apply y zs))
           ys)))
  (lambda: ([x : Number] [y : Number]) (+ x y))
  (lambda: ([x : Number] [y : Number]) (- x y))
  (lambda: ([x : Number] [y : Number]) (* x y))
  (lambda: ([x : Number] [y : Number]) (/ x y)))
 3 4)

((plambda: (a ...) [ys : (a ... a -> Number) *]
   (lambda: [zs : a ... a]
     (map (lambda: ([y : (a ... a -> Number)])
            (apply y zs))
          ys)))
 + - * /)

(: map-with-funcs (All (b a ...) ((a ... a -> b) * -> (a ... a -> (Listof b)))))
(define (map-with-funcs . fs)
  (lambda as
    (map (lambda: ([f : (a ... a -> b)])
           (apply f as))
         fs)))
(map-with-funcs + - * /)
