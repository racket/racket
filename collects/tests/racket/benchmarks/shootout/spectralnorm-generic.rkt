#lang racket/base

;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/

;; Translated directly from the C# version, which was:
;;   contributed by Isaac Gouy

(require racket/cmdline)

(define (Approximate n)
  (let ([u (make-vector n 1.0)]
        [v (make-vector n 0.0)])
    ;; 20 steps of the power method
    (for ([i (in-range 10)])
      (MultiplyAtAv n u v)
      (MultiplyAtAv n v u))
    
    ;; B=AtA         A multiplied by A transposed
    ;; v.Bv /(v.v)   eigenvalue of v
    (let loop ([i 0][vBv 0][vv 0])
      (if (= i n)
          (sqrt (/ vBv vv))
          (let ([vi (vector-ref v i)])
            (loop (add1 i)
                  (+ vBv (* (vector-ref u i) vi))
                  (+ vv (* vi vi))))))))

;; return element i,j of infinite matrix A
(define (A i j)
  (/ 1.0 (+ (* (+ i j) (/ (+ i (+ j 1)) 2.0)) (+ i 1))))

;; multiply vector v by matrix A
(define (MultiplyAv n v Av)
  (for ([i (in-range n)])
    (vector-set! Av i 
                 (for/fold ([r 0])
                     ([j (in-range n)])
                   (+ r (* (A i j) (vector-ref v j)))))))

;; multiply vector v by matrix A transposed
(define (MultiplyAtv n v Atv)
  (for ([i (in-range n)])
    (vector-set! Atv i
                 (for/fold ([r 0])
                     ([j (in-range n)])
                   (+ r (* (A j i) (vector-ref v j)))))))

;; multiply vector v by matrix A and then by matrix A transposed 
(define (MultiplyAtAv n v AtAv)
  (let ([u (make-vector n 0.0)])
    (MultiplyAv n v u)
    (MultiplyAtv n u AtAv)))

(printf "~a\n"
        (real->decimal-string
         (Approximate (command-line #:args (n) (string->number n)))
         9))

