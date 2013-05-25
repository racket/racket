#lang racket

(provide array-make array-ref array-set!
         array-mult array-mult-vector
         array-det array-sub array-inv)

;; creates a square matrix, nxn
(define (array-make n)
  (define a (make-vector n #f))
  (for ([i (in-range n)])
    (vector-set! a i (make-vector n 0.0)))
  a)

;; returns an array element
(define (array-ref m i j)
  (vector-ref (vector-ref m i) j))

;; sets an array element
(define (array-set! m i j val)
  (vector-set! (vector-ref m i) j val))

;; matrix - matrix multiplication
(define (array-mult a b)
  (define n (vector-length a))
  (define m (array-make n))
  (for* ([i (in-range n)]
         [j (in-range n)]
         [k (in-range n)])
    (array-set! m i j (+ (array-ref m i j)
                         (* (array-ref a i k)
                            (array-ref b k j)))))
  m)

;; vector - matrix multiplication
(define (array-mult-vector m v)
  (define r (make-vector 4 0))
  (for* ([i (in-range 4)]
         [j (in-range 4)])
    (vector-set! r i (+ (* (array-ref m i j) (vector-ref v j))
                        (vector-ref r i))))
  r)

;; calculates the determinant of a matrix
(define (array-det a)
  (cond [(= (vector-length a) 1)
         (array-ref a 0 0)]
        [(= (vector-length a) 2)
         (- (* (array-ref a 0 0) (array-ref a 1 1))
            (* (array-ref a 1 0) (array-ref a 0 1)))]
        [else
         (define n   (vector-length a))
         (define det 0.0)
         (define m   #f)
         (define j2  #f)
         (for ([j1 (in-range n)])
           ;; create sub-matrix
           (set! m (array-make (- n 1)))
           (for ([i (in-range 1 n)])
             (set! j2 0)
             (for ([j (in-range n)] #:unless (= j j1))
               (array-set! m (- i 1) j2 (array-ref a i j))
               (set! j2 (+ j2 1))))
           (set! det (+ det (* (expt -1 (+ 1 j1 1))
                               (array-ref a 0 j1)
                               (array-det m)))))
         ;; return the determinant
         det]))

;; creates a sub-matrix, except row 'in' and column 'jn'
(define (array-sub a in jn)
  (define n  (vector-length a))
  (define m  (array-make (- n 1)))
  (define ii 0)
  (define jj 0)
  (for ([i (in-range n)] #:unless (= i in))
    (set! jj 0)
    (for ([j (in-range n)] #:unless (= j jn))
      (array-set! m ii jj (array-ref a i j))
      (set! jj (+ jj 1)))
    (set! ii (+ ii 1)))
  m)

;; calculates the inverse of a matrix
(define (array-inv a)
  (define n (vector-length a))
  (define m (array-make n))
  (define det (array-det a))
  (for* ([i (in-range n)]
         [j (in-range n)])
    (array-set! m j i (/ (* (expt -1 (+ i j))
                            (array-det (array-sub a i j)))
                         det)))
  m)

;; (define aa '#(#(1 2 3) #(4 4 0) #(0 0 10)))
;; (define bb (array-inv aa))
;; (array-mult aa bb)
