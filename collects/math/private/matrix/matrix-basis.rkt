#lang typed/racket

(require racket/fixnum
         math/array
         math/matrix
         "matrix-column.rkt"
         "utils.rkt"
         "../unsafe.rkt"
         "../vector/vector-mutate.rkt"
         )

(: col-matrix-project1 (case-> ((Matrix Real) (Matrix Real) Any -> (U #f (Matrix Real)))
                               ((Matrix Number) (Matrix Number) Any -> (U #f (Matrix Number)))))
(define (col-matrix-project1 v b unit?)
  (cond [unit?  (matrix-scale b (matrix-dot v b))]
        [else   (define b.b (matrix-dot b b))
                (cond [(and (zero? b.b) (exact? b.b))  #f]
                      [else  (matrix-scale b (/ (matrix-dot v b) b.b))])]))

(: col-matrix-project
   (All (A) (case-> ((Matrix Real) (Matrix Real) -> (Matrix Real))
                    ((Matrix Real) (Matrix Real) Any -> (U A (Matrix Real)))
                    ((Matrix Real) (Matrix Real) Any (-> A) -> (U A (Matrix Real)))
                    ((Matrix Number) (Matrix Number) -> (Matrix Number))
                    ((Matrix Number) (Matrix Number) Any -> (U A (Matrix Number)))
                    ((Matrix Number) (Matrix Number) Any (-> A) -> (U A (Matrix Number))))))
(define col-matrix-project
  (case-lambda
    [(v B)  (col-matrix-project v B #f)]
    [(v B unit?)
     (col-matrix-project
      v B unit?
      (λ () (error 'col-matrix-project "expected basis with nonzero column vectors; given ~e" B)))]
    [(v B unit? fail)
     (unless (col-matrix? v) (raise-argument-error 'col-matrix-project "col-matrix?" v))
     (define bs (matrix-cols (ensure-matrix 'col-matrix-project B)))
     (define p (col-matrix-project1 v (first bs) unit?))
     (cond [p  (let loop ([bs  (rest bs)] [p p])
                 (cond [(empty? bs)  p]
                       [else  (define q (col-matrix-project1 v (first bs) unit?))
                              (if q (loop (rest bs) (matrix+ p q)) (fail))]))]
           [else  (fail)])]))

(: find-nonzero-vector (case-> ((Vectorof (Vectorof Real)) -> (U #f Index))
                               ((Vectorof (Vectorof Number)) -> (U #f Index))))
(define (find-nonzero-vector vss)
  (define n (vector-length vss))
  (cond [(= n 0)  #f]
        [else  (let loop ([#{i : Nonnegative-Fixnum} 0])
                 (cond [(i . fx< . n)
                        (define vs (unsafe-vector-ref vss i))
                        (if (vector-zero? vs) (loop (fx+ i 1)) i)]
                       [else  #f]))]))

(: subtract-projections!
   (case-> ((Vectorof (Vectorof Real)) Index Index (Vectorof Real) Any -> Void)
           ((Vectorof (Vectorof Number)) Index Index (Vectorof Number) Any -> Void)))
(define (subtract-projections! cols n i ci unit?)
  (let j-loop ([#{j : Nonnegative-Fixnum} (fx+ i 1)])
    (when (j . fx< . n)
      (vector-sub-proj! (unsafe-vector-ref cols j) ci unit?)
      (j-loop (fx+ j 1)))))

(: matrix-gram-schmidt (All (A) (case-> ((Matrix Real) -> (Array Real))
                                        ((Matrix Real) Any -> (Array Real))
                                        ((Matrix Number) -> (Array Number))
                                        ((Matrix Number) Any -> (Array Number)))))
(define (matrix-gram-schmidt M [unit? #f])
  (define rows (matrix->vector* M))
  (define n (vector-length rows))
  (define i (find-nonzero-vector rows))
  (cond [i  (define rowi (unsafe-vector-ref rows i))
            (subtract-projections! rows n i rowi #f)
            (when unit? (vector-normalize! rowi))
            (let loop ([#{i : Nonnegative-Fixnum} (fx+ i 1)] [bs (list rowi)])
              (cond [(i . fx< . n)
                     (define rowi (unsafe-vector-ref rows i))
                     (cond [(vector-zero? rowi)  (loop (fx+ i 1) bs)]
                           [else  (subtract-projections! rows n i rowi #f)
                                  (when unit? (vector-normalize! rowi))
                                  (loop (fx+ i 1) (cons rowi bs))])]
                    [else
                     (vector*->matrix (list->vector (reverse bs)))]))]
        [else
         (make-array (vector 0 (matrix-num-cols M)) 0)]))
#|
(define a (col-matrix [1 2 1]))
(define b (col-matrix [1 -2 2]))

(define basis
  (gram-schmidt-orthogonal
   (matrix-cols
    (array #[#[2 1 0] #[2 2 1] #[0 2 0]]))))

(column-project a b)
(col-matrix-project a b)

(projection-on-orthogonal-basis a basis)
(col-matrix-project a (matrix-augment basis))
(projection-on-orthonormal-basis a basis)
(col-matrix-project a (matrix-augment basis) 'orthonormal)

(matrix-gram-schmidt
 (matrix [[0 1 2]
          [0 2 3]
          [0 1 5]]))

(matrix-gram-schmidt
 (matrix [[5 1 2]
          [2 2 3]
          [-3 1 5]]))
|#
