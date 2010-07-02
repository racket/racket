;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/

;; Translated directly from the C# version, which was:
;;   contributed by Isaac Gouy

(require racket/cmdline
	 racket/require (for-syntax racket/base)
	 (rename-in
          (filtered-in
           (lambda (name) (regexp-replace #rx"unsafe-" name ""))
           racket/unsafe/ops)
          [fx->fl ->fl])
         (only-in racket/flonum make-flvector))


(: Approximate (Natural -> Float))
(define (Approximate n)
  (let ([u (make-flvector n 1.0)]
        [v (make-flvector n 0.0)])
    ;; 20 steps of the power method
    (for ([i (in-range 10)])
      (MultiplyAtAv n u v)
      (MultiplyAtAv n v u))
    
    ;; B=AtA         A multiplied by A transposed
    ;; v.Bv /(v.v)   eigenvalue of v
    (let: loop : Float ([i : Natural 0][vBv : Float 0.0][vv : Float 0.0])
      (if (= i n)
          (flsqrt (fl/ vBv vv))
          (let ([vi (flvector-ref v i)])
            (loop (add1 i)
                  (fl+ vBv (fl* (flvector-ref u i) vi))
                  (fl+ vv (fl* vi vi))))))))

;; return element i,j of infinite matrix A
(: A (Natural Natural -> Float))
(define (A i j)
  (fl/ 1.0 (fl+ (fl* (->fl (+ i j))
                     (fl/ (->fl (+ i (+ j 1))) 2.0)) 
                (->fl (+ i 1)))))

;; multiply vector v by matrix A
(: MultiplyAv (Natural FlVector FlVector -> Void))
(define (MultiplyAv n v Av)
  (for ([i (in-range n)])
    (flvector-set! Av i 
                   (for/fold: : Float ([r : Float 0.0])
                       ([j : Natural (in-range n)])
                     (fl+ r (fl* (A i j) (flvector-ref v j)))))))

;; multiply vector v by matrix A transposed
(: MultiplyAtv (Natural FlVector FlVector -> Void))
(define (MultiplyAtv n v Atv)
  (for ([i (in-range n)])
    (flvector-set! Atv i
                   (for/fold: : Float ([r : Float 0.0])
                       ([j : Natural (in-range n)])
                     (fl+ r (fl* (A j i) (flvector-ref v j)))))))

;; multiply vector v by matrix A and then by matrix A transposed 
(: MultiplyAtAv (Natural FlVector FlVector -> Void))
(define (MultiplyAtAv n v AtAv)
  (let ([u (make-flvector n 0.0)])
    (MultiplyAv n v u)
    (MultiplyAtv n u AtAv)))

(printf "~a\n"
        (real->decimal-string
         (Approximate (command-line #:args (n) (assert (string->number (assert n string?)) exact-nonnegative-integer?)))
         9))
