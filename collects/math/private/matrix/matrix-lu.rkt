#lang typed/racket/base

(require racket/fixnum
         "matrix-types.rkt"
         "matrix-conversion.rkt"
         "matrix-arithmetic.rkt"
         "utils.rkt"
         "../unsafe.rkt"
         "../vector/vector-mutate.rkt"
         "../array/mutable-array.rkt"
         "../array/array-struct.rkt")

(provide matrix-lu)

;; An LU factorization exists iff Gaussian elimination can be done without row swaps.

(: matrix-lu
   (All (A) (case-> ((Matrix Real)        -> (Values (Matrix Real) (Matrix Real)))
                    ((Matrix Real) (-> A) -> (Values (U A (Matrix Real)) (Matrix Real)))
                    ((Matrix Number)        -> (Values (Matrix Number) (Matrix Number)))
                    ((Matrix Number) (-> A) -> (Values (U A (Matrix Number)) (Matrix Number))))))
(define matrix-lu
  (case-lambda
    [(M)  (matrix-lu M (λ () (raise-argument-error 'matrix-lu "LU-decomposable matrix" M)))]
    [(M fail)
     (define m (square-matrix-size M))
     (define rows (matrix->vector* M))
     (define L
       (parameterize ([array-strictness #f])
         ;; Construct L in a weird way to prove to TR that it has the right type
         (array->mutable-array (matrix-scale M (ann 0 Real)))))
     ;; Going to fill in the lower triangle by banging values into `ys'
     (define ys (mutable-array-data L))
     (let loop ([#{i : Nonnegative-Fixnum} 0])
       (cond
         [(i . fx< . m)
          ;; Pivot must be on the diagonal
          (define pivot (unsafe-vector2d-ref rows i i))
          (cond
            [(zero? pivot)  (values (fail) M)]
            [else
             ;; Zero out everything below the pivot
             (let l-loop ([#{l : Nonnegative-Fixnum} (fx+ i 1)])
               (cond
                 [(l . fx< . m)
                  (define x_li (unsafe-vector2d-ref rows l i))
                  (define y_li (/ x_li pivot))
                  (unless (zero? x_li)
                    ;; Fill in lower triangle of L
                    (unsafe-vector-set! ys (+ (* l m) i) y_li)
                    ;; Add row i, scaled
                    (vector-scaled-add! (unsafe-vector-ref rows l)
                                        (unsafe-vector-ref rows i)
                                        (- y_li)))
                  (l-loop (fx+ l 1))]
                 [else
                  (loop (fx+ i 1))]))])]
         [else
          ;; L's lower triangle has been filled; now fill the diagonal with 1s
          (for: ([i : Integer (in-range 0 m)])
            (vector-set! ys (+ (* i m) i) 1))
          (values L (vector*->matrix rows))]))]))
