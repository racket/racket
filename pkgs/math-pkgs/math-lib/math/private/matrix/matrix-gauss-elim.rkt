#lang typed/racket/base

(require racket/fixnum
         racket/list
         "matrix-types.rkt"
         "matrix-conversion.rkt"
         "utils.rkt"
         "../unsafe.rkt"
         "../vector/vector-mutate.rkt")

(provide
 Pivoting
 matrix-gauss-elim
 matrix-row-echelon)

(define-type Pivoting (U 'first 'partial))

(: matrix-gauss-elim
   (case-> ((Matrix Flonum) -> (Values (Matrix Flonum) (Listof Index)))
           ((Matrix Flonum) Any -> (Values (Matrix Flonum) (Listof Index)))
           ((Matrix Flonum) Any Any -> (Values (Matrix Flonum) (Listof Index)))
           ((Matrix Flonum) Any Any Pivoting -> (Values (Matrix Flonum) (Listof Index)))
           ((Matrix Real) -> (Values (Matrix Real) (Listof Index)))
           ((Matrix Real) Any -> (Values (Matrix Real) (Listof Index)))
           ((Matrix Real) Any Any -> (Values (Matrix Real) (Listof Index)))
           ((Matrix Real) Any Any Pivoting -> (Values (Matrix Real) (Listof Index)))
           ((Matrix Float-Complex) -> (Values (Matrix Float-Complex) (Listof Index)))
           ((Matrix Float-Complex) Any -> (Values (Matrix Float-Complex) (Listof Index)))
           ((Matrix Float-Complex) Any Any -> (Values (Matrix Float-Complex) (Listof Index)))
           ((Matrix Float-Complex) Any Any Pivoting -> (Values (Matrix Float-Complex) (Listof Index)))
           ((Matrix Number) -> (Values (Matrix Number) (Listof Index)))
           ((Matrix Number) Any -> (Values (Matrix Number) (Listof Index)))
           ((Matrix Number) Any Any -> (Values (Matrix Number) (Listof Index)))
           ((Matrix Number) Any Any Pivoting -> (Values (Matrix Number) (Listof Index)))))
(define (matrix-gauss-elim M [jordan? #f] [unitize-pivot? #f] [pivoting 'partial])
  (define-values (m n) (matrix-shape M))
  (define rows (matrix->vector* M))
  (let loop ([#{i : Nonnegative-Fixnum} 0]
             [#{j : Nonnegative-Fixnum} 0]
             [#{without-pivot : (Listof Index)} empty])
    (cond
      [(j . fx>= . n)
       (values (vector*->matrix rows)
               (reverse without-pivot))]
      [(i . fx>= . m)
       (values (vector*->matrix rows)
               ;; None of the rest of the columns can have pivots
               (let loop ([#{j : Nonnegative-Fixnum} j] [without-pivot without-pivot])
                 (cond [(j . fx< . n)  (loop (fx+ j 1) (cons j without-pivot))]
                       [else  (reverse without-pivot)])))]
      [else
       (define-values (p pivot)
         (case pivoting
           [(partial)  (find-partial-pivot rows m i j)]
           [(first)    (find-first-pivot rows m i j)]))
       (cond
         [(zero? pivot)  (loop i (fx+ j 1) (cons j without-pivot))]
         [else
          ;; Swap pivot row with current
          (vector-swap! rows i p)
          ;; Possibly unitize the new current row
          (let ([pivot  (if unitize-pivot?
                            (begin (vector-scale! (unsafe-vector-ref rows i) (/ 1 pivot))
                                   (/ pivot pivot))
                            pivot)])
            (elim-rows! rows m i j pivot (if jordan? 0 (fx+ i 1)))
            (loop (fx+ i 1) (fx+ j 1) without-pivot))])])))

(: matrix-row-echelon
   (case-> ((Matrix Flonum) -> (Matrix Flonum))
           ((Matrix Flonum) Any -> (Matrix Flonum))
           ((Matrix Flonum) Any Any -> (Matrix Flonum))
           ((Matrix Flonum) Any Any Pivoting -> (Matrix Flonum))
           ((Matrix Real) -> (Matrix Real))
           ((Matrix Real) Any -> (Matrix Real))
           ((Matrix Real) Any Any -> (Matrix Real))
           ((Matrix Real) Any Any Pivoting -> (Matrix Real))
           ((Matrix Float-Complex) -> (Matrix Float-Complex))
           ((Matrix Float-Complex) Any -> (Matrix Float-Complex))
           ((Matrix Float-Complex) Any Any -> (Matrix Float-Complex))
           ((Matrix Float-Complex) Any Any Pivoting -> (Matrix Float-Complex))
           ((Matrix Number) -> (Matrix Number))
           ((Matrix Number) Any -> (Matrix Number))
           ((Matrix Number) Any Any -> (Matrix Number))
           ((Matrix Number) Any Any Pivoting -> (Matrix Number))))
(define (matrix-row-echelon M [jordan? #f] [unitize-pivot? #f] [pivoting 'partial])
  (let-values ([(M _) (matrix-gauss-elim M jordan? unitize-pivot? pivoting)])
    M))
