; -*- Scheme -*-

(module array mzscheme

(provide array-make array-ref array-set!
         array-mult array-mult-vector
         array-det array-sub array-inv
)


; creates a square matrix, nxn
(define (array-make n)
  (let*
    ( (a (make-vector n #f)) )
    (do ((i 0 (+ i 1))) ((= i n))
      (vector-set! a i (make-vector n 0.0))
    )
    a
  )
)

; returns an array element
(define (array-ref m i j)
  (vector-ref (vector-ref m i) j)
)

; sets an array element
(define (array-set! m i j val)
  (let*
    ( (vect (vector-ref m i)) )
    (vector-set! vect j val)
  )
)

; matrix - matrix multiplication
(define (array-mult a b)
  (let*
    ( (n (vector-length a))
      (m (array-make n))
    )
    (do ((i 0 (+ i 1))) ((= i n))
      (do ((j 0 (+ j 1))) ((= j n))
        (do ((k 0 (+ k 1))) ((= k n))
          (array-set! m i j (+ (array-ref m i j)
                               (* (array-ref a i k)
                                  (array-ref b k j))))
        )
      )
    )
    m
  )
)

; vector - matrix multiplication
(define (array-mult-vector m v)
  (let* ( (r (make-vector 4 0)) )
    (do ((i 0 (+ 1 i))) ((= i 4))
      (do ((j 0 (+ 1 j))) ((= j 4))
        (vector-set! r
                     i
                     (+ (* (array-ref m i j) (vector-ref v j))
                        (vector-ref r i)))
      )
    )
    r
  )
)

; calculates the determinant of a matrix
(define (array-det a)
  (cond
    ( (= (vector-length a) 1) 
      (array-ref a 0 0)
    )
    ( (= (vector-length a) 2) 
      (- (* (array-ref a 0 0) (array-ref a 1 1))
         (* (array-ref a 1 0) (array-ref a 0 1)) )
    )
    ( else
      (let*
        ( (n   (vector-length a))
          (det 0.0)
          (m   #f)
          (j2  #f)
        )
        (do ((j1 0 (+ j1 1))) ((= j1 n))
          ; create sub-matrix
          (set! m (array-make (- n 1)))
          (do ((i 1 (+ i 1))) ((= i n))
            (set! j2 0)
            (do ((j 0 (+ j 1))) ((= j n))
              (if (not (=  j j1))
                (begin
                  (array-set! m (- i 1) j2 (array-ref a i j))
                  (set! j2 (+ j2 1))
                )
              )
            )
          )
          (set! det (+ det (* (expt -1 (+ 1 j1 1))
                              (array-ref a 0 j1)
                              (array-det m)
                           )
                    )
          )
        )
        ; return the determinant
        det
      )
    )
  )
)

; creates a sub-matrix, except row 'in' and column 'jn'
(define (array-sub a in jn)
  (let*
    ( (n  (vector-length a))
      (m  (array-make (- n 1)))
      (ii 0)
      (jj 0)
    )
    (do ((i 0 (+ i 1))) ((= i n))
      (if (not (= i in))
        (begin
          (set! jj 0)
          (do ((j 0 (+ j 1))) ((= j n))
            (if (not (= j jn))
              (begin
                (array-set! m ii jj (array-ref a i j))
                (set! jj (+ jj 1))
              )
            )
          )
          (set! ii (+ ii 1))
        )
      )
    )
    m
  )
)

; calculates the inverse of a matrix
(define (array-inv a)
  (let*
    ( (n (vector-length a))
      (m (array-make n))
      (det (array-det a))
    )
    (do ((i 0 (+ i 1))) ((= i n))
      (do ((j 0 (+ j 1))) ((= j n))
        (array-set! m j i (/ (* (expt -1 (+ i j))
                                (array-det (array-sub a i j))
                             )
                             det))
      )
    )
    m
  )
)



;  (define aa '#( #( 1 2 3) #( 4 4 0) #( 0 0 10) ) )
;  (define bb (array-inv aa))
;  (array-mult aa bb)

) ; end of module


