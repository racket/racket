;; Matrix.scm

(define-type Matrix (Vectorof (Vectorof Natural)))

(define size 30)

(: 1+ (Natural -> Natural))
(define (1+ x) (+ x 1))

(: mkmatrix (Natural Natural -> Matrix))
(define (mkmatrix rows cols)
  (let: ((mx : Matrix (make-vector rows (ann (vector 0) (Vectorof Natural))))
         (count : Natural 1))
    (do: : Void
         ((i : Natural 0 (1+ i)))
         ((= i rows))
      (let: ((row : (Vectorof Natural) (make-vector cols 0)))
        (do: : Void
             ((j : Natural 0 (1+ j)))
             ((= j cols))
          (vector-set! row j count)
          (set! count (+ count 1)))
        (vector-set! mx i row)))
    mx))

(: num-cols (Matrix -> Natural))
(define (num-cols mx)
  (let ((row (vector-ref mx 0)))
    (vector-length row)))

(: num-rows (Matrix -> Natural))
(define (num-rows mx)
  (vector-length mx))

(: mmult (Natural Natural Matrix Matrix -> Matrix))
(define (mmult rows cols m1 m2)
  (let: ((m3 : Matrix (make-vector rows (ann (vector 0) (Vectorof Natural)))))
    (do: : Void
         ((i : Natural  0 (1+ i)))
         ((= i rows))
      (let: ((m1i : (Vectorof Natural) (vector-ref m1 i))
             (row : (Vectorof Natural) (make-vector cols 0)))
        (do: : Void
             ((j : Natural 0 (1+ j)))
             ((= j cols))
          (let: ((val : Natural 0))
            (do: : Void
                 ((k : Natural 0 (1+ k)))
                 ((= k cols))
              (set! val (+ val (* (vector-ref m1i k)
                                  (vector-ref (vector-ref m2 k) j)))))
            (vector-set! row j val)))
        (vector-set! m3 i row)))
    m3))

(: matrix-print (Matrix -> Void))
(define (matrix-print m)
  (do: : Void
       ((i : Natural 0 (1+ i)))
       ((= i (num-rows m)))
    (let ((row (vector-ref m i)))
      (do: : Void
           ((j : Natural 0 (1+ j)))
           ((= j (num-cols m)))
        (display (vector-ref row j))
        (if (< j (num-cols m))
            (display " ")
            #t))
      (newline))))

(define (print-list . items) (for-each display items) (newline))

(: main ((Vectorof String) -> Void))
(define (main args)
  (let ((n (or (and (= (vector-length args) 1)
                    (assert (string->number (vector-ref args 0)) exact-integer?))
               1)))
    (let: ((mm : Matrix (vector ((inst vector Natural) 0)))
           (m1 : Matrix (mkmatrix size size))
           (m2 : Matrix (mkmatrix size size)))
      (let loop ((iter n))
        (cond ((> iter 0)
               (set! mm (mmult size size m1 m2))
               (loop (- iter 1)))))
      (let ((r0 (vector-ref mm 0))
            (r2 (vector-ref mm 2))
            (r3 (vector-ref mm 3))
            (r4 (vector-ref mm 4)))
        (print-list (vector-ref r0 0) " " (vector-ref r2 3) " "
                    (vector-ref r3 2) " " (vector-ref r4 4))))))

(main (current-command-line-arguments))
