;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;; Translated from Mike Pall's Lua version.

(require racket/cmdline racket/trace racket/contract
         racket/unsafe/ops racket/flonum)

(let* ([A (lambda: ((i : Integer) (j : Integer))
            (let ([ij (unsafe-fx+ i j)])
              (unsafe-fl/ 1.0 (unsafe-fl+ (unsafe-fl* (unsafe-fl* (unsafe-fx->fl ij)
                                                                  (unsafe-fx->fl (unsafe-fx+ ij 1)))
                                                      0.5) 
                                          (unsafe-fx->fl (unsafe-fx+ i 1))))))]
       [Av 
        (lambda: ((x : FlVector) (y : FlVector) (N : Integer))
          (for: ([i : Integer (in-range N)])
               (unsafe-flvector-set!
                y i
                (let: L : Float ([a : Float 0.0] [j : Integer 0])
                  (if (unsafe-fx= j N) a
                      (L (unsafe-fl+ a (unsafe-fl* (unsafe-flvector-ref x j) (A i j)))
                         (unsafe-fx+ j 1)))))))]
       [Atv
        (lambda: ((x : FlVector) (y : FlVector) (N : Integer))
          (for: ([i : Integer (in-range N)])
               (unsafe-flvector-set!
                y i
                (let: L : Float ([a : Float 0.0] [j : Integer 0])
                  (if (unsafe-fx= j N) a
                      (L (unsafe-fl+ a (unsafe-fl* (unsafe-flvector-ref x j) (A j i)))
                         (unsafe-fx+ j 1)))))))]
       [AtAv (lambda: ((x : FlVector) (y : FlVector) (t : FlVector) (N : Integer))
               (Av x t N) (Atv t y N))]
       [N (command-line #:args (#{n : String}) (assert (string->number n) exact-nonnegative-integer?))]
       [u (make-flvector N 1.0)]
       [v (make-flvector N)]
       [t (make-flvector N)])
  (for: ([i : Integer (in-range 10)])
    (AtAv u v t N)
    (AtAv v u t N))
  (displayln (real->decimal-string 
              (unsafe-flsqrt 
               (let: L : Float ([vBv : Float 0.0] [vv : Float 0.0] [i : Integer 0])
                 (if (unsafe-fx= i N) (unsafe-fl/ vBv vv)
                     (let ([ui (unsafe-flvector-ref u i)] [vi (unsafe-flvector-ref v i)])
                       (L (unsafe-fl+ vBv (unsafe-fl* ui vi))
                          (unsafe-fl+ vv (unsafe-fl* vi vi))
                          (unsafe-fx+ i 1))))))
              9)))
