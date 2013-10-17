; Matrix.scm

#lang racket/base

(define size 30)

(define (1+ x) (+ x 1))

(define (mkmatrix rows cols)
  (let ((mx (make-vector rows 0))
        (count 1))
    (do ((i 0 (1+ i)))
        ((= i rows))
      (let ((row (make-vector cols 0)))
        (do ((j 0 (1+ j)))
            ((= j cols))
          (vector-set! row j count)
          (set! count (+ count 1)))
        (vector-set! mx i row)))
    mx))

(define (num-cols mx)
  (let ((row (vector-ref mx 0)))
    (vector-length row)))

(define (num-rows mx)
  (vector-length mx))

(define (mmult rows cols m1 m2)
  (let ((m3 (make-vector rows 0)))
    (do ((i 0 (1+ i)))
        ((= i rows))
      (let ((m1i (vector-ref m1 i))
            (row (make-vector cols 0)))
        (do ((j 0 (1+ j)))
            ((= j cols))
          (let ((val 0))
            (do ((k 0 (1+ k)))
                ((= k cols))
              (set! val (+ val (* (vector-ref m1i k)
                                  (vector-ref (vector-ref m2 k) j)))))
            (vector-set! row j val)))
        (vector-set! m3 i row)))
    m3))

(define (matrix-print m)
  (do ((i 0 (1+ i)))
      ((= i (num-rows m)))
    (let ((row (vector-ref m i)))
      (do ((j 0 (1+ j)))
          ((= j (num-cols m)))
        (display (vector-ref row j))
        (if (< j (num-cols m))
            (display " ")
            #t))
      (newline))))

(define (print-list . items) (for-each display items) (newline))

(define (main args)
  (let ((n (or (and (= (vector-length args) 1) (string->number (vector-ref
                                                                args 0)))
               1)))
    (let ((mm 0)
          (m1 (mkmatrix size size))
          (m2 (mkmatrix size size)))
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
