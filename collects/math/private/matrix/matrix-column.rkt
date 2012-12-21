#lang typed/racket/base

(require math/array
         math/base
         "matrix-types.rkt"
         "matrix-conversion.rkt"
         "matrix-arithmetic.rkt"
         "../unsafe.rkt")

(provide unit-column
         column-height
         unsafe-column->vector
         column-scale
         column+
         column-dot
         column-norm
         column-project
         column-project/unit
         column-normalize)

(: unit-column : Integer Integer -> (Result-Column Number))
(define (unit-column m i)
  (cond
    [(and (index? m) (index? i))
     (define v (make-vector m 0))
     (if (< i m)
         (vector-set! v i 1)
         (error 'unit-vector "dimension must be largest"))
     (vector->matrix m 1 v)]
    [else
     (error 'unit-vector "expected two indices")]))

(: column-height : (Column Number) -> Index)
(define (column-height v)
  (if (vector? v)
      (vector-length v)
      (matrix-num-rows v)))

(: unsafe-column->vector : (Column Number) -> (Vectorof Number))
(define (unsafe-column->vector v)
  (if (vector? v) v
      (let ()
        (define-values (m n) (matrix-shape v))
        (if (= n 1)
            (mutable-array-data (array->mutable-array v))
            (error 'unsafe-column->vector
                   "expected a column (vector or mx1 matrix), got ~a" v)))))

(: column-scale : (Column Number) Number -> (Result-Column Number))
(define (column-scale a s)
  (if (vector? a)
      (let*: ([n (vector-length a)]
              [v : (Vectorof Number) (make-vector n 0)])
        (for: ([i (in-range 0 n)]
               [x : Number (in-vector a)]) 
          (vector-set! v i (* s x)))
        (->col-matrix v))
      (matrix-scale a s)))

(: column+ : (Column Number) (Column Number) -> (Result-Column Number))
(define (column+ v w)
  (cond [(and (vector? v) (vector? w))
         (let ([n (vector-length v)]
               [m (vector-length w)])
           (unless (= m n)
             (error 'column+ 
                    "expected two column vectors of the same length, got ~a and ~a" v w))
           (define: v+w : (Vectorof Number) (make-vector n 0))
           (for: ([i (in-range 0 n)]
                  [x : Number (in-vector v)]
                  [y : Number (in-vector w)])
             (vector-set! v+w i (+ x y)))
           (->col-matrix v+w))]
        [else 
         (unless (= (column-height v) (column-height w))
           (error 'column+ 
                  "expected two column vectors of the same length, got ~a and ~a" v w))
         (array+ (->col-matrix v) (->col-matrix w))]))

(: column-dot : (Column Number) (Column Number) -> Number)
(define (column-dot c d)  
  (define v (unsafe-column->vector c))
  (define w (unsafe-column->vector d))
  (define m (column-height v))
  (define s (column-height w))
  (cond
    [(not (= m s)) (error 'column-dot 
                          "expected two mx1 matrices with same number of rows, got ~a and ~a"
                          c d)]
    [else
     (for/sum: : Number ([i (in-range 0 m)])
       (assert i index?)
       ; Note: If d is a vector of reals, 
       ;       then the conjugate is a no-op
       (* (unsafe-vector-ref v i)
          (conjugate (unsafe-vector-ref w i))))]))

(: column-norm : (Column Number) -> Real)
(define (column-norm v)
  (define norm (sqrt (column-dot v v)))
  (assert norm real?))

(: column-project : (Column Number) (Column Number) -> (Result-Column Number))
; (column-project v w)
;    Return the projection og vector v on vector w.
(define (column-project v w)
  (let ([w.w (column-dot w w)])
    (if (zero? w.w)
        (error 'column-project "projection on the zero vector not defined")
        (matrix-scale (->col-matrix w) (/ (column-dot v w) w.w)))))

(: column-project/unit : (Column Number) (Column Number) -> (Result-Column Number))
; (column-project-on-unit v w)
;    Return the projection og vector v on a unit vector w.
(define (column-project/unit v w)
  (matrix-scale (->col-matrix w) (column-dot v w)))

(: column-normalize : (Column Number) -> (Result-Column Number))
; (column-vector-normalize v)
;    Return unit vector with same direction as v.
;    If v is the zero vector, the zero vector is returned.
(define (column-normalize w)
  (let ([norm (column-norm w)]
        [w (->col-matrix w)])
    (cond [(zero? norm) w]
          [else (matrix-scale w (/ norm))])))
