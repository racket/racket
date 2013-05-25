#lang typed/scheme/base

(require typed-racket/base-env/extra-procs)


(call-with-values (lambda () (values 1 2)) (lambda: ([x : Number] [y : Number]) (+ x y)))

(#{call-with-values @ Integer Integer Integer} (lambda () (values 1 2)) (lambda: ([x : Integer] [y : Integer]) (+ x y)))


(call-with-values (lambda () (values 1 2)) (lambda: ([x : Integer] [y : Integer]) (+ x y)))

(: map-with-funcs (All (b ...) ((b ... b -> b) ... b -> (b ... b -> (values b ... b)))))
(define (map-with-funcs . fs)
  (lambda bs
    (apply values (map (plambda: (c) ([f : (b ... b -> c)])
                         (apply f bs)) fs))))

(map-with-funcs + - * /)

(inst map-with-funcs Integer Integer)

(map-with-funcs
 (lambda: ([x : Integer] [y : Integer]) (+ x y))
 (lambda: ([x : Integer] [y : Integer]) (- x y)) )

(((inst map-with-funcs Integer Integer)
    (lambda: ([x : Integer] [y : Integer]) (+ x y))
    (lambda: ([x : Integer] [y : Integer]) (- x y)))
   3 4)
