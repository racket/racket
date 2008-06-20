#lang typed-scheme


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

(plambda: (a ...) [ys : (a ... a -> Number) *]
          ((inst map Number (a ... a -> Number)) (ann (error 'fail) ((a ... a -> Number) -> Number)) ys))

(plambda: (a ...) [ys : (a ... a -> Number) *]
          (map (ann (error 'fail) ((a ... a -> Number) -> Number)) ys))

(apply (plambda: (a ...) [ys : (a ... a -> Number) *]
         (lambda: [zs : a ... a]
                  (map (lambda: ([y : (a ... a -> Number)])
                                (apply y zs))
                       ys)))
       (list + - * /))

(apply (plambda: (a ...) [ys : (a ... a -> Number) *]
         (lambda: [zs : a ... a]
            #{(error 'foo) :: (Listof Number)}))
       (list + - * /))