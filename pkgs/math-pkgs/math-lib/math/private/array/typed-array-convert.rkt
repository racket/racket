#lang typed/racket/base

(require "../unsafe.rkt"
         "array-struct.rkt"
         "mutable-array.rkt"
         "utils.rkt"
         (only-in "untyped-array-convert.rkt" make-to-array-functions))

(provide (all-defined-out))

;; Conversion to arrays
(make-to-array-functions list*->array vector*->array)

(: array->list (All (A) ((Array A) -> (Listof A))))
(define (array->list arr)
  (vector->list (array->vector arr)))

(: array->vector (All (A) ((Array A) -> (Vectorof A))))
(define (array->vector arr)
  (mutable-array-data ((inst array->mutable-array A) arr)))

;; ===================================================================================================
;; Conversion from arrays

(: array->list* (All (A) ((Array A) -> (Listof* A))))
(define (array->list* arr)
  (define ds (array-shape arr))
  (define proc (unsafe-array-proc arr))
  (define dims (vector-length ds))
  (define: js : Indexes (make-vector dims 0))
  (let: i-loop : (Listof* A) ([i : Nonnegative-Fixnum  0])
    (cond [(i . < . dims)
           (define di (unsafe-vector-ref ds i))
           (cond [(= di 0)  (list)]
                 [else
                  (define lsti null)
                  (let: j-loop : (Listof (Listof* A)) ([ji : Nonnegative-Fixnum  0]
                                                       [lsti : (Listof (Listof* A))  null])
                    (cond [(ji . < . di)
                           (unsafe-vector-set! js i ji)
                           (j-loop (+ ji 1) (cons (i-loop (+ i 1)) lsti))]
                          [else  (reverse lsti)]))])]
          [else  (proc js)])))

(: array->vector* (All (A) ((Array A) -> (Vectorof* A))))
(define (array->vector* arr)
  (define ds (array-shape arr))
  (define proc (unsafe-array-proc arr))
  (define dims (vector-length ds))
  (define: js : Indexes (make-vector dims 0))
  (let: i-loop : (Vectorof* A) ([i : Nonnegative-Fixnum  0])
    (cond [(i . < . dims)
           (define di (unsafe-vector-ref ds i))
           (cond [(= di 0)  (vector)]
                 [else
                  (define veci+1 (i-loop (+ i 1)))
                  (define veci (make-vector di veci+1))
                  (let: j-loop : (Vectorof* A) ([ji : Nonnegative-Fixnum  0])
                    (cond [(ji . < . di)
                           (unsafe-vector-set! js i ji)
                           (unsafe-vector-set! veci ji (i-loop (+ i 1)))
                           (j-loop (+ ji 1))]
                          [else  veci]))])]
          [else  (proc js)])))
