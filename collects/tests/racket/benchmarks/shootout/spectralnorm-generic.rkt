#lang racket/base
;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;; Translated from Mike Pall's Lua version.

(require racket/cmdline racket/trace racket/contract)

(let* ([A (lambda (i j)
            (let ([ij (+ i j)])
              (/ 1.0 (+ (* (* ij (+ ij 1))
                           0.5)
                        (+ i 1)))))]
       [Av 
        (lambda (x y N)
          (for ([i (in-range N)])
               (vector-set!
                y i
                (let L ([a 0.0] [j 0])
                  (if (= j N) a
                      (L (+ a (* (vector-ref x j) (A i j)))
                         (+ j 1)))))))]
       [Atv
        (lambda (x y N)
          (for ([i (in-range N)])
               (vector-set!
                y i
                (let L ([a 0.0] [j 0])
                  (if (= j N) a
                      (L (+ a (* (vector-ref x j) (A j i)))
                         (+ j 1)))))))]
       [AtAv (lambda (x y t N) (Av x t N) (Atv t y N))]
       [N (command-line #:args (n) (string->number n))]
       [u (make-vector N 1.0)]
       [v (make-vector N)]
       [t (make-vector N)])
  (for ([i (in-range 10)])
    (AtAv u v t N)
    (AtAv v u t N))
  (displayln (real->decimal-string 
              (sqrt 
               (let L ([vBv 0.0] [vv 0.0] [i 0])
                 (if (= i N) (/ vBv vv)
                     (let ([ui (vector-ref u i)] [vi (vector-ref v i)])
                       (L (+ vBv (* ui vi))
                          (+ vv (* vi vi))
                          (+ i 1))))))
              9)))
