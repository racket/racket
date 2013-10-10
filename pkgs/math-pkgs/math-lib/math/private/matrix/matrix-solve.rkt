#lang typed/racket/base

(require racket/fixnum
         racket/match
         racket/list
         "matrix-types.rkt"
         "matrix-constructors.rkt"
         "matrix-conversion.rkt"
         "matrix-basic.rkt"
         "matrix-gauss-elim.rkt"
         "utils.rkt"
         "../vector/vector-mutate.rkt"
         "../array/array-indexing.rkt"
         "../array/mutable-array.rkt"
         "../array/array-struct.rkt")

(provide 
 matrix-determinant
 matrix-determinant/row-reduction  ; for testing
 matrix-invertible?
 matrix-inverse
 matrix-solve)

;; ===================================================================================================
;; Determinant

(: matrix-determinant (case-> ((Matrix Flonum) -> Flonum)
                              ((Matrix Real) -> Real)
                              ((Matrix Float-Complex) -> Float-Complex)
                              ((Matrix Number) -> Number)))
(define (matrix-determinant M)
  (define m (square-matrix-size M))
  (cond
    [(= m 1)  (matrix-ref M 0 0)]
    [(= m 2)  (match-define (vector a b c d)
                (mutable-array-data (array->mutable-array M)))
              (- (* a d) (* b c))]
    [(= m 3)  (match-define (vector a b c d e f g h i)
                (mutable-array-data (array->mutable-array M)))
              (+ (*    a  (- (* e i) (* f h)))
                 (* (- b) (- (* d i) (* f g)))
                 (*    c  (- (* d h) (* e g))))]
    [else
     (matrix-determinant/row-reduction M)]))

(: matrix-determinant/row-reduction (case-> ((Matrix Flonum) -> Flonum)
                                            ((Matrix Real) -> Real)
                                            ((Matrix Float-Complex) -> Float-Complex)
                                            ((Matrix Number) -> Number)))
(define (matrix-determinant/row-reduction M)
  (define m (square-matrix-size M))
  (define rows (matrix->vector* M))
  (let loop ([#{i : Nonnegative-Fixnum} 0] [#{sign : (U Positive-Fixnum Negative-Fixnum)} 1])
    (cond
      [(i . fx< . m)
       (define-values (p pivot) (find-partial-pivot rows m i i))
       (cond
         [(zero? pivot)  pivot]  ; no pivot means non-invertible matrix
         [else
          (let ([sign  (if (= i p) sign (begin (vector-swap! rows i p)  ; swapping negates sign
                                               (if (= sign 1) -1 1)))])
            (elim-rows! rows m i i pivot (fx+ i 1))  ; adding scaled rows doesn't change it
            (loop (fx+ i 1) sign))])]
      [else
       (define prod (unsafe-vector2d-ref rows 0 0))
       (let loop ([#{i : Nonnegative-Fixnum} 1] [prod prod])
         (cond [(i . fx< . m)
                (loop (fx+ i 1) (* prod (unsafe-vector2d-ref rows i i)))]
               [else  (* prod sign)]))])))

;; ===================================================================================================
;; Inversion

(: matrix-invertible? ((Matrix Number) -> Boolean))
(define (matrix-invertible? M)
  (and (square-matrix? M)
       (not (zero? (matrix-determinant M)))))

(: matrix-inverse (All (A) (case-> ((Matrix Flonum)        -> (Matrix Flonum))
                                   ((Matrix Flonum) (-> A) -> (U A (Matrix Flonum)))
                                   ((Matrix Real)        -> (Matrix Real))
                                   ((Matrix Real) (-> A) -> (U A (Matrix Real)))
                                   ((Matrix Float-Complex)        -> (Matrix Float-Complex))
                                   ((Matrix Float-Complex) (-> A) -> (U A (Matrix Float-Complex)))
                                   ((Matrix Number)        -> (Matrix Number))
                                   ((Matrix Number) (-> A) -> (U A (Matrix Number))))))
(define matrix-inverse
  (case-lambda
    [(M)  (matrix-inverse M (λ () (raise-argument-error 'matrix-inverse "matrix-invertible?" M)))]
    [(M fail)
     (define m (square-matrix-size M))
     (define x00 (matrix-ref M 0 0))
     (define I (identity-matrix m (one* x00) (zero* x00)))
     (define-values (IM^-1 wps) (parameterize ([array-strictness #f])
                                  (matrix-gauss-elim (matrix-augment (list M I)) #t #t)))
     (cond [(and (not (empty? wps)) (= (first wps) m))
            (submatrix IM^-1 (::) (:: m #f))]
           [else  (fail)])]))

;; ===================================================================================================
;; Solving linear systems

(: matrix-solve
   (All (A) (case->
             ((Matrix Flonum) (Matrix Flonum)        -> (Matrix Flonum))
             ((Matrix Flonum) (Matrix Flonum) (-> A) -> (U A (Matrix Flonum)))
             ((Matrix Real) (Matrix Real)        -> (Matrix Real))
             ((Matrix Real) (Matrix Real) (-> A) -> (U A (Matrix Real)))
             ((Matrix Float-Complex) (Matrix Float-Complex)        -> (Matrix Float-Complex))
             ((Matrix Float-Complex) (Matrix Float-Complex) (-> A) -> (U A (Matrix Float-Complex)))
             ((Matrix Number) (Matrix Number)        -> (Matrix Number))
             ((Matrix Number) (Matrix Number) (-> A) -> (U A (Matrix Number))))))
(define matrix-solve
  (case-lambda
    [(M B)  (matrix-solve M B (λ () (raise-argument-error 'matrix-solve "matrix-invertible?" 0 M B)))]
    [(M B fail)
     (define m (square-matrix-size M))
     (define-values (s t) (matrix-shape B))
     (cond [(= m s)
            (define-values (IX wps) (parameterize ([array-strictness #f])
                                      (matrix-gauss-elim (matrix-augment (list M B)) #t #t)))
            (cond [(and (not (empty? wps)) (= (first wps) m))
                   (submatrix IX (::) (:: m #f))]
                  [else  (fail)])]
           [else
            (error 'matrix-solve
                   "matrices must have the same number of rows; given ~e and ~e"
                   M B)])]))
