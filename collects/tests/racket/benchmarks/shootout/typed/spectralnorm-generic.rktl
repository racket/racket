;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/

;; Translated directly from the C# version, which was:
;;   contributed by Isaac Gouy

(require racket/cmdline)

(: Approximate (Natural -> Float))
(define (Approximate n)
  (let: ([u : (Vectorof Float) (make-vector n 1.0)]
         [v : (Vectorof Float) (make-vector n 0.0)])
    ;; 20 steps of the power method
    (for: : Void ([i : Natural (in-range 10)])
      (MultiplyAtAv n u v)
      (MultiplyAtAv n v u))
    
    ;; B=AtA         A multiplied by A transposed
    ;; v.Bv /(v.v)   eigenvalue of v
    (let: loop : Float ([i : Natural 0][vBv : Float 0.0][vv : Float 0.0])
      (if (= i n)
          (assert (sqrt (/ vBv vv)) inexact-real?)
          (let ([vi (vector-ref v i)])
            (loop (add1 i)
                  (+ vBv (* (vector-ref u i) vi))
                  (+ vv (* vi vi))))))))

;; return element i,j of infinite matrix A
(: A (Natural Natural -> Float))
(define (A i j)
  (exact->inexact (/ 1.0 (+ (* (+ i j) (/ (+ i (+ j 1)) 2.0)) (+ i 1)))))

;; multiply vector v by matrix A
(: MultiplyAv (Natural (Vectorof Float) (Vectorof Float) -> Void))
(define (MultiplyAv n v Av)
  (for: : Void ([i : Natural (in-range n)])
    (vector-set! Av i 
                 (for/fold: : Float ([r : Float 0.0])
                     ([j : Natural (in-range n)])
                   (+ r (* (A i j) (vector-ref v j)))))))

;; multiply vector v by matrix A transposed
(: MultiplyAtv (Natural (Vectorof Float) (Vectorof Float) -> Void))
(define (MultiplyAtv n v Atv)
  (for: : Void ([i : Natural (in-range n)])
    (vector-set! Atv i
                 (for/fold: : Float ([r : Float 0.0])
                     ([j : Natural (in-range n)])
                   (+ r (* (A j i) (vector-ref v j)))))))

;; multiply vector v by matrix A and then by matrix A transposed 
(: MultiplyAtAv (Natural (Vectorof Float) (Vectorof Float) -> Void))
(define (MultiplyAtAv n v AtAv)
  (let: ([u : (Vectorof Float) (make-vector n 0.0)])
    (MultiplyAv n v u)
    (MultiplyAtv n u AtAv)))

(printf "~a\n"
        (real->decimal-string
         (Approximate (command-line #:args (n) (assert (string->number (assert n string?)) exact-nonnegative-integer?)))
         9))

