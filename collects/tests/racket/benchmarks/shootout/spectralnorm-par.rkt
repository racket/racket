#lang racket/base
;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;; Translated from Mike Pall's Lua version.
;; Parallelized by Sam Tobin-Hochstadt

(require racket/cmdline racket/future
         racket/require (for-syntax racket/base)
         (filtered-in (λ (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops)
         (only-in racket/flonum make-flvector))

(define-syntax-rule (for/par k ([i N]) b)  
  (let ([stride (fxquotient N k)])
    (define fs 
      (for/list ([n k])
        (future (λ () (for ([i (in-range (fx* n stride) (fxmin N (fx* (fx+ n 1) stride)))]) b)))))
    (for-each touch fs)))


;; the big let improves performance by about 20%
(let* ()
  (define N (command-line #:args (n) (string->number n)))
  (define C (processor-count))
  (define (A i j)
    (let ([ij (fx+ i j)])
      (fl/ 1.0 (fl+ (fl* (fl* (fx->fl ij)
                              (fx->fl (fx+ ij 1)))
                         0.5) 
                    (fx->fl (fx+ i 1))))))
  (define (Av x y N)
    (for/par C ([i N])
      (flvector-set!
       y i
       (let L ([a 0.0] [j 0])
         (if (fx= j N) a
             (L (fl+ a (fl* (flvector-ref x j) (A i j)))
                (fx+ j 1)))))))
  (define (Atv x y N)
    (for/par C ([i N])
      (flvector-set!
       y i
       (let L ([a 0.0] [j 0])
         (if (fx= j N) a
             (L (fl+ a (fl* (flvector-ref x j) (A j i)))
                (fx+ j 1)))))))
  (define (AtAv x y t N) (Av x t N) (Atv t y N))
  (define u (make-flvector N 1.0))
  (define v (make-flvector N))
  (define t (make-flvector N))
  (for ([i (in-range 10)])
    (AtAv u v t N) (AtAv v u t N))
  (displayln (real->decimal-string 
              (flsqrt 
               (let L ([vBv 0.0] [vv 0.0] [i 0])
                 (if (fx= i N) (fl/ vBv vv)
                     (let ([ui (flvector-ref u i)] [vi (flvector-ref v i)])
                       (L (fl+ vBv (fl* ui vi))
                          (fl+ vv (fl* vi vi))
                          (fx+ i 1))))))
              9)))
