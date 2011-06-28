#lang racket/base
;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;; Translated from Mike Pall's Lua version.

(require racket/cmdline racket/trace racket/contract
         racket/unsafe/ops racket/flonum)

(let* ([A (lambda (i j)
            (let ([ij (unsafe-fx+ i j)])
              (unsafe-fl/ 1.0 (unsafe-fl+ (unsafe-fl* (unsafe-fl* (unsafe-fx->fl ij)
                                                                  (unsafe-fx->fl (unsafe-fx+ ij 1)))
                                                      0.5) 
                                          (unsafe-fx->fl (unsafe-fx+ i 1))))))]
       [Av 
        (lambda (x y N)
          (for ([i (in-range N)])
               (unsafe-flvector-set!
                y i
                (let L ([a 0.0] [j 0])
                  (if (unsafe-fx= j N) a
                      (L (unsafe-fl+ a (unsafe-fl* (unsafe-flvector-ref x j) (A i j)))
                         (unsafe-fx+ j 1)))))))]
       [Atv
        (lambda (x y N)
          (for ([i (in-range N)])
               (unsafe-flvector-set!
                y i
                (let L ([a 0.0] [j 0])
                  (if (unsafe-fx= j N) a
                      (L (unsafe-fl+ a (unsafe-fl* (unsafe-flvector-ref x j) (A j i)))
                         (unsafe-fx+ j 1)))))))]
       [AtAv (lambda (x y t N) (Av x t N) (Atv t y N))]
       [N (command-line #:args (n) (string->number n))]
       [u (make-flvector N 1.0)]
       [v (make-flvector N)]
       [t (make-flvector N)])
  (for ([i (in-range 10)])
    (AtAv u v t N)
    (AtAv v u t N))
  (displayln (real->decimal-string 
              (unsafe-flsqrt 
               (let L ([vBv 0.0] [vv 0.0] [i 0])
                 (if (unsafe-fx= i N) (unsafe-fl/ vBv vv)
                     (let ([ui (unsafe-flvector-ref u i)] [vi (unsafe-flvector-ref v i)])
                       (L (unsafe-fl+ vBv (unsafe-fl* ui vi))
                          (unsafe-fl+ vv (unsafe-fl* vi vi))
                          (unsafe-fx+ i 1))))))
              9)))
