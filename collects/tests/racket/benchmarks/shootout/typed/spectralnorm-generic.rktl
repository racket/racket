;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;; Translated from Mike Pall's Lua version.

(require racket/cmdline racket/trace racket/contract racket/flonum)

(let* ([A (lambda: ((i : Natural) (j : Natural))
            (let ([ij (+ i j)])
              (/ 1.0 (+ (* (* ij (+ ij 1))
                           0.5)
                        (+ i 1)))))]
       [Av 
        (lambda: ((x : (Vectorof Float)) (y : (Vectorof Float)) (N : Natural))
          (for: ([i : Natural (in-range N)])
            (vector-set!
             y i
             (let: L : Float ([a : Float 0.0] [j : Natural 0])
               (if (= j N) a
                   (L (+ a (* (vector-ref x j) (A i j)))
                      (+ j 1)))))))]
       [Atv
        (lambda: ((x : (Vectorof Float)) (y : (Vectorof Float)) (N : Natural))
          (for: ([i : Natural (in-range N)])
            (vector-set!
             y i
             (let: L : Float ([a : Float 0.0] [j : Natural 0])
               (if (= j N) a
                   (L (+ a (* (vector-ref x j) (A j i)))
                      (+ j 1)))))))]
       [AtAv (lambda: ((x : (Vectorof Float)) (y : (Vectorof Float)) (t : (Vectorof Float)) (N : Natural))
               (Av x t N) (Atv t y N))]
       [N (command-line #:args (#{n : String}) (assert (string->number n) exact-nonnegative-integer?))]
       [u (make-vector N 1.0)]
       [v (make-vector N 0.0)]
       [t (make-vector N 0.0)])
  (for: ([i : Natural (in-range 10)])
    (AtAv u v t N)
    (AtAv v u t N))
  (displayln (real->decimal-string 
              (flsqrt 
               (let: L : Float ([vBv : Float 0.0] [vv : Float 0.0] [i : Natural 0])
                 (if (= i N) (/ vBv vv)
                     (let ([ui (vector-ref u i)] [vi (vector-ref v i)])
                       (L (+ vBv (* ui vi))
                          (+ vv (* vi vi))
                          (+ i 1))))))
              9)))
