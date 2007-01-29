;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/

;; Translated directly from the C# version, which was:
;;   contributed by Isaac Gouy

(module spectralnorm mzscheme
  (require (lib "string.ss"))

  (define (Approximate n)
    (let ([u (make-vector n 1.0)]
          [v (make-vector n 0.0)])
      ;; 20 steps of the power method
      (let loop ([i 0])
        (unless (= i 10)
          (MultiplyAtAv n u v)
          (MultiplyAtAv n v u)
          (loop (add1 i))))
      
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
    (/ 1.0 (+ (* (+ i j) (/ (+ i j 1) 2)) i 1)))

  ;; multiply vector v by matrix A
  (define (MultiplyAv n v Av)
    (let loop ([i 0])
      (unless (= i n)
        (let jloop ([j 0][r 0])
          (if (= j n)
              (vector-set! Av i r)
              (jloop (add1 j)
                     (+ r (* (A i j) (vector-ref v j))))))
        (loop (add1 i)))))

  ;; multiply vector v by matrix A transposed
  (define (MultiplyAtv n v Atv)
    (let loop ([i 0])
      (unless (= i n)
        (let jloop ([j 0][r 0])
          (if (= j n)
              (vector-set! Atv i r)
              (jloop (add1 j)
                     (+ r (* (A j i) (vector-ref v j))))))
        (loop (add1 i)))))

  ;; multiply vector v by matrix A and then by matrix A transposed 
  (define (MultiplyAtAv n v AtAv)
    (let ([u (make-vector n 0.0)])
      (MultiplyAv n v u)
      (MultiplyAtv n u AtAv)))

  (printf "~a\n"
          (real->decimal-string
           (Approximate (string->number (vector-ref
                                         (current-command-line-arguments)
                                         0)))
           9)))
