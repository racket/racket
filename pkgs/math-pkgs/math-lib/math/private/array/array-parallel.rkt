#lang typed/racket/base

(require racket/future
         racket/list
         "../unsafe.rkt"
         "../parameters.rkt"
         "array-struct.rkt"
         "mutable-array.rkt"
         "utils.rkt")

(provide parallel-array->mutable-array
         parallel-array-strict)

(: eval-array-proc! (All (A) (Indexes (Indexes -> A) Indexes (Vectorof A) Index Index -> Void)))
(define (eval-array-proc! ds proc js vs start end)
  (define dims (vector-length ds))
  (unsafe-value-index->array-index! ds start js)
  (let: k-loop : Nonnegative-Fixnum ([k : Nonnegative-Fixnum  0]
                                     [j : Nonnegative-Fixnum  start])
    (cond [(k . < . dims)
           (define: dk : Index (unsafe-vector-ref ds k))
           (let: jk-loop : Nonnegative-Fixnum ([jk : Nonnegative-Fixnum  (unsafe-vector-ref js k)]
                                               [j : Nonnegative-Fixnum  j])
             (cond [(jk . < . dk)
                    (unsafe-vector-set! js k jk)
                    (jk-loop (+ jk 1) (k-loop (+ k 1) j))]
                   [else
                    (unsafe-vector-set! js k 0)
                    j]))]
          [(j . >= . end)  j]
          [else  (define v (proc js))
                 (unsafe-vector-set! vs j v)
                 (unsafe-fx+ j 1)]))
  (void))

(: parallel-array->mutable-array (All (A) ((Array A) -> (Mutable-Array A))))
(define (parallel-array->mutable-array arr)
  (define size (array-size arr))
  (cond
    [(zero? size)  (unsafe-vector->array (array-shape arr) (vector))]
    [else
     (define ds (array-shape arr))
     (define dims (vector-length ds))
     (define proc (unsafe-array-proc arr))
     ;; Use all the available processors
     (define num-futures (max-math-threads))
     (parameterize ([max-math-threads  1])
       (define jss
         (for/list: : (Listof Indexes) ([i  (in-range num-futures)])
           (ann (make-vector dims 0) Indexes)))
       (define: vs : (Vectorof A) (make-vector size (proc (first jss))))
       (define stops
         (for/list: : (Listof Index) ([i  (in-range num-futures)])
           (assert (quotient (* (+ i 1) size) num-futures) index?)))
       (define futures
         (for/list: : (Listof (Futureof Void)) ([start  (in-list stops)]
                                                [end  (in-list (rest stops))]
                                                [js  (in-list (rest jss))])
           (future (λ () (eval-array-proc! ds proc js vs start end)))))
       (eval-array-proc! ds proc (first jss) vs 1 (first stops))
       (for: ([f  (in-list futures)])
         (touch f))
     
       (unsafe-vector->array ds vs))]))

(: parallel-array-strict (All (A) ((Array A) -> (Array A))))
(define (parallel-array-strict arr)
  (cond [(array-strict? arr)  arr]
        [else  (parallel-array->mutable-array arr)]))
