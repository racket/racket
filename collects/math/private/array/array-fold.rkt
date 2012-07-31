#lang typed/racket/base

(require racket/performance-hint
         "../unsafe.rkt"
         "array-struct.rkt"
         "array-ref.rkt"
         "utils.rkt")

(provide array-axis-fold
         array-axis-sum
         array-axis-prod
         array-axis-min
         array-axis-max
         array-fold
         array-sum
         array-prod
         array-min-value
         array-max-value)

;; ===================================================================================================
;; Per-axis folds

(: array-axis-fold/init (All (A B) ((Array A) Integer (A B -> B) B -> (view-array B))))
(define (array-axis-fold/init arr k f init)
  (let ([arr  (array-view arr)])
    (define ds (unsafe-array-shape arr))
    (define dims (vector-length ds))
    (cond
      [(= dims 0)  (raise-type-error 'array-axis-fold "Array with at least one axis" 0 arr k)]
      [(or (0 . > . k) (k . >= . dims))
       (raise-type-error 'array-sum (format "Index < ~a" dims) 1 arr k)]
      [else
       (define dk (unsafe-vector-ref ds k))
       (define new-ds (unsafe-vector-remove ds k))
       (define proc (unsafe-array-proc arr))
       (unsafe-view-array
        new-ds (λ: ([js : (Vectorof Index)])
                 (define old-js (unsafe-vector-insert js k 0))
                 (let: loop : B ([i : Nonnegative-Fixnum  0] [acc : B  init])
                   (cond [(i . < . dk)  (unsafe-vector-set! old-js k i)
                                        (loop (+ i 1) (f (proc old-js) acc))]
                         [else  acc]))))])))

(: array-axis-fold/no-init (All (A) ((Array A) Integer (A A -> A) -> (view-array A))))
(define (array-axis-fold/no-init arr k f)
  (let ([arr  (array-view arr)])
    (define ds (unsafe-array-shape arr))
    (define dims (vector-length ds))
    (define size (array-size arr))
    (cond
      [(= dims 0)  (raise-type-error 'array-axis-fold "Array with at least one axis" 0 arr k)]
      [(or (0 . > . k) (k . >= . dims))
       (raise-type-error 'array-sum (format "Index < ~a" dims) 1 arr k)]
      [else
       (define dk (unsafe-vector-ref ds k))
       (when (= dk 0) (raise-type-error 'array-axis-fold "nonzero axis" 0 arr k))
       (define new-ds (unsafe-vector-remove ds k))
       (define proc (unsafe-array-proc arr))
       (unsafe-view-array
        new-ds (λ: ([js : (Vectorof Index)])
                 (define old-js (unsafe-vector-insert js k 0))
                 (let: loop : A ([i : Nonnegative-Fixnum  1] [acc : A  (proc old-js)])
                   (cond [(i . < . dk)  (unsafe-vector-set! old-js k i)
                                        (loop (+ i 1) (f (proc old-js) acc))]
                         [else  acc]))))])))

(: array-axis-fold (All (A B) (case-> ((Array A) Integer (A A -> A) -> (view-array A))
                                      ((Array A) Integer (A B -> B) B -> (view-array B)))))
(define array-axis-fold
  (case-lambda
    [(arr k f)  (array-axis-fold/no-init arr k f)]
    [(arr k f init)  (array-axis-fold/init arr k f init)]))

;; ---------------------------------------------------------------------------------------------------
;; Whole-array fold

(: array-fold (All (A) ((Array A) ((Array A) Index -> (view-array A)) -> (view-array A))))
(begin-encourage-inline
  (define (array-fold arr f)
    (define dims (array-dims arr))
    (let loop ([#{k : Index} dims] [arr arr])
      (cond [(zero? k)  (array-view arr)]
            [else  (let ([k  (sub1 k)])
                     (loop k (f arr k)))]))))

;; ===================================================================================================

(define-syntax-rule (define-axis-fold name op T ...)
  (begin-encourage-inline
    (: name (case-> ((Array T) Integer -> (view-array T)) ...
                    ((Array T) Integer T -> (view-array T)) ...))
    (define name
      (case-lambda
        [(arr k)  (array-axis-fold arr k op)]
        [(arr k init)  (array-axis-fold arr k op init)]))))

(define-axis-fold array-axis-sum + Float Real Float-Complex Number)
(define-axis-fold array-axis-prod * Float Real Float-Complex Number)
(define-axis-fold array-axis-min min Float Real)
(define-axis-fold array-axis-max max Float Real)

;; ===================================================================================================

(define-syntax-rule (define-fold name array-axis-op T ...)
  (begin-encourage-inline
    (: name (case-> ((Array T) -> T) ...
                    ((Array T) T -> T) ...))
    (define name 
      (case-lambda
        [(arr)  (array-ref* (array-fold arr array-axis-op))]
        [(arr init)
         (plet: (A) ([arr : (Array A)  arr]
                     [array-axis-op : ((Array A) Index A -> (view-array A))  array-axis-op]
                     [init : A  init])
                (array-ref* (array-fold arr (λ: ([arr : (Array A)] [k : Index])
                                              (array-axis-op arr k init)))))]))))

(define-fold array-sum array-axis-sum Float Real Float-Complex Number)
(define-fold array-prod array-axis-prod Float Real Float-Complex Number)
(define-fold array-min-value array-axis-min Float Real)
(define-fold array-max-value array-axis-max Float Real)
