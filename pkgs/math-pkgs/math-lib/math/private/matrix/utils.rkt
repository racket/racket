#lang typed/racket/base

(require racket/performance-hint
         racket/string
         racket/fixnum
         math/base
         "matrix-types.rkt"
         "../unsafe.rkt"
         "../array/array-struct.rkt"
         "../vector/vector-mutate.rkt")

(provide (all-defined-out))

(: format-matrices/error ((Listof (Array Any)) -> String))
(define (format-matrices/error as)
  (string-join (map (λ: ([a : (Array Any)]) (format "~e" a)) as)))

(: matrix-shapes (Symbol (Matrix Any) (Matrix Any) * -> (Values Index Index)))
(define (matrix-shapes name arr . brrs)
  (define-values (m n) (matrix-shape arr))
  (unless (andmap (λ: ([brr : (Matrix Any)])
                    (define-values (bm bn) (matrix-shape brr))
                    (and (= bm m) (= bn n)))
                  brrs)
    (error name
           "matrices must have the same shape; given ~a"
           (format-matrices/error (cons arr brrs))))
  (values m n))

(: matrix-multiply-shape ((Matrix Any) (Matrix Any) -> (Values Index Index Index)))
(define (matrix-multiply-shape arr brr)
  (define-values (ad0 ad1) (matrix-shape arr))
  (define-values (bd0 bd1) (matrix-shape brr))
  (unless (= ad1 bd0)
    (error 'matrix-multiply
           "1st argument column size and 2nd argument row size are not equal; given ~e and ~e"
           arr brr))
  (values ad0 ad1 bd1))

(: ensure-matrix (All (A) Symbol (Array A) -> (Array A)))
(define (ensure-matrix name a)
  (if (matrix? a) a (raise-argument-error name "matrix?" a)))

(: ensure-row-matrix (All (A) Symbol (Array A) -> (Array A)))
(define (ensure-row-matrix name a)
  (if (row-matrix? a) a (raise-argument-error name "row-matrix?" a)))

(: ensure-col-matrix (All (A) Symbol (Array A) -> (Array A)))
(define (ensure-col-matrix name a)
  (if (col-matrix? a) a (raise-argument-error name "col-matrix?" a)))

(: sort/key (All (A B) (case-> ((Listof A) (B B -> Boolean) (A -> B) -> (Listof A))
                               ((Listof A) (B B -> Boolean) (A -> B) Boolean -> (Listof A)))))
;; Sometimes necessary because TR can't do inference with keyword arguments yet
(define (sort/key lst lt? key [cache-keys? #f])
  ((inst sort A B) lst lt? #:key key #:cache-keys? cache-keys?))

(: unsafe-vector2d-ref (All (A) ((Vectorof (Vectorof A)) Index Index -> A)))
(define (unsafe-vector2d-ref vss i j)
  (unsafe-vector-ref (unsafe-vector-ref vss i) j))

;; Note: this accepts +nan.0
(define nonnegative?
  (λ: ([x : Real]) (not (x . < . 0))))

(define number-rational?
  (λ: ([z : Number])
    (cond [(real? z)  (rational? z)]
          [else  (and (rational? (real-part z))
                      (rational? (imag-part z)))])))

(: find-partial-pivot
   (case-> ((Vectorof (Vectorof Flonum)) Index Index Index -> (Values Index Flonum))
           ((Vectorof (Vectorof Real)) Index Index Index -> (Values Index Real))
           ((Vectorof (Vectorof Float-Complex)) Index Index Index -> (Values Index Float-Complex))
           ((Vectorof (Vectorof Number)) Index Index Index -> (Values Index Number))))
;; Find the element with maximum magnitude in a column
(define (find-partial-pivot rows m i j)
  (define l (fx+ i 1))
  (define pivot (unsafe-vector2d-ref rows i j))
  (define mag-pivot (magnitude pivot))
  (let loop ([#{l : Nonnegative-Fixnum} l] [#{p : Index} i] [pivot pivot] [mag-pivot mag-pivot])
    (cond [(l . fx< . m)
           (define new-pivot (unsafe-vector2d-ref rows l j))
           (define mag-new-pivot (magnitude new-pivot))
           (cond [(mag-new-pivot . > . mag-pivot)  (loop (fx+ l 1) l new-pivot mag-new-pivot)]
                 [else  (loop (fx+ l 1) p pivot mag-pivot)])]
          [else  (values p pivot)])))

(: find-first-pivot
   (case-> ((Vectorof (Vectorof Flonum)) Index Index Index -> (Values Index Flonum))
           ((Vectorof (Vectorof Real)) Index Index Index -> (Values Index Real))
           ((Vectorof (Vectorof Float-Complex)) Index Index Index -> (Values Index Float-Complex))
           ((Vectorof (Vectorof Number)) Index Index Index -> (Values Index Number))))
;; Find the first nonzero element in a column
(define (find-first-pivot rows m i j)
  (define pivot (unsafe-vector2d-ref rows i j))
  (if ((magnitude pivot) . > . 0)
      (values i pivot)
      (let loop ([#{l : Nonnegative-Fixnum} (fx+ i 1)])
        (cond [(l . fx< . m)
               (define pivot (unsafe-vector2d-ref rows l j))
               (if ((magnitude pivot) . > . 0) (values l pivot) (loop (fx+ l 1)))]
              [else
               (values i pivot)]))))

(: elim-rows!
   (case-> ((Vectorof (Vectorof Flonum)) Index Index Index Flonum Nonnegative-Fixnum -> Void)
           ((Vectorof (Vectorof Real)) Index Index Index Real Nonnegative-Fixnum -> Void)
           ((Vectorof (Vectorof Float-Complex)) Index Index Index Float-Complex Nonnegative-Fixnum 
                                                -> Void)
           ((Vectorof (Vectorof Number)) Index Index Index Number Nonnegative-Fixnum -> Void)))
(define (elim-rows! rows m i j pivot start)
  (define row_i (unsafe-vector-ref rows i))
  (let loop ([#{l : Nonnegative-Fixnum} start])
    (when (l . fx< . m)
      (unless (l . fx= . i)
        (define row_l (unsafe-vector-ref rows l))
        (define x_lj (unsafe-vector-ref row_l j))
        (unless (= x_lj 0)
          (vector-scaled-add! row_l row_i (* -1 (/ x_lj pivot)) j)
          ;; Make sure the element below the pivot is zero
          (unsafe-vector-set! row_l j (- x_lj x_lj))))
      (loop (fx+ l 1)))))

(begin-encourage-inline
  
  (: call/ns (All (A) ((-> (Matrix A)) -> (Matrix A))))
  (define (call/ns thnk)
    (array-default-strict
     (parameterize ([array-strictness #f])
       (thnk))))
  
  )  ; begin-encourage-inline

(: make-thread-local-box (All (A) (A -> (-> (Boxof A)))))
(define (make-thread-local-box contents)
  (let: ([val : (Thread-Cellof (U #f (Boxof A))) (make-thread-cell #f)])
    (λ () (or (thread-cell-ref val)
              (let: ([v : (Boxof A)  (box contents)])
                (thread-cell-set! val v)
                v)))))

(: one (case-> (Flonum -> Nonnegative-Flonum)
               (Real -> (U 1 Nonnegative-Flonum))
               (Float-Complex -> Nonnegative-Flonum)
               (Number -> (U 1 Nonnegative-Flonum))))
(define (one x)
  (cond [(flonum? x)  1.0]
        [(real? x)  1]
        [(float-complex? x)  1.0]
        [else  1]))

(: zero (case-> (Flonum -> Flonum-Positive-Zero)
                (Real -> (U 0 Flonum-Positive-Zero))
                (Float-Complex -> Flonum-Positive-Zero)
                (Number -> (U 0 Flonum-Positive-Zero))))
(define (zero x)
  (cond [(flonum? x)  0.0]
        [(real? x)  0]
        [(float-complex? x)  0.0]
        [else  0]))

(: one* (case-> (Flonum -> Nonnegative-Flonum)
                (Real -> (U 1 Nonnegative-Flonum))
                (Float-Complex -> Float-Complex)
                (Number -> (U 1 Nonnegative-Flonum Float-Complex))))
(define (one* x)
  (cond [(flonum? x)  1.0]
        [(real? x)  1]
        [(float-complex? x)  1.0+0.0i]
        [else  1]))

(: zero* (case-> (Flonum -> Flonum-Positive-Zero)
                 (Real -> (U 0 Flonum-Positive-Zero))
                 (Float-Complex -> Float-Complex)
                 (Number -> (U 0 Flonum-Positive-Zero Float-Complex))))
(define (zero* x)
  (cond [(flonum? x)  0.0]
        [(real? x)  0]
        [(float-complex? x)  0.0+0.0i]
        [else  0]))
