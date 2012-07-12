#lang typed/racket

(require racket/unsafe/ops
         racket/vector racket/flonum
         "array-struct.rkt"
         "utils.rkt")

(provide list->array
         array->list
         vector->array
         array->vector)

;; ===================================================================================================
;; Conversion from lists (conversion to lists is in "flarray-struct.rkt")

(: list->array (All (A) ((Any -> Boolean : A) (Listof* A) -> (lazy-array A))))
(define (list->array pred? lst)
  (define (raise-shape-error)
    ;; don't have to worry about non-Index size - can't fit in memory anyway
    (raise-type-error 'list->array "rectangular (Listof* A)" lst))
  
  (define ds (list-shape pred? lst))
  (cond [(pred? lst)  (unsafe-lazy-array #() (λ (js) lst))]
        [ds  (let ([ds  (array-shape-safe->unsafe ds raise-shape-error)])
               (array-lazy (unsafe-strict-array ds (list->vector (list-flatten pred? lst)))))]
        [else  (raise-shape-error)]))

(: array->list (All (A) ((Array A) -> (Listof* A))))
(define (array->list arr)
  (let ([arr  (array-lazy arr)])
    (define ds (unsafe-array-shape arr))
    (define proc (unsafe-array-proc arr))
    (define dims (vector-length ds))
    (define: js : (Vectorof Index) (make-vector dims 0))
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
            [else  (proc js)]))))

;; ===================================================================================================
;; Conversion to and from vectors

(: vector->array (All (A) ((Any -> Boolean : A) (Vectorof* A) -> (lazy-array A))))
(define (vector->array pred? vec)
  (define (raise-shape-error)
    ;; don't have to worry about non-Index size - can't fit in memory anyway
    (raise-type-error 'vector->array "rectangular (Vectorof* Float)" vec))
  
  (define ds (vector-shape pred?  vec))
  (cond [(pred? vec)  (unsafe-lazy-array #() (λ (js) vec))]
        [ds  (let ([ds  (array-shape-safe->unsafe ds raise-shape-error)])
               (define dims (vector-length ds))
               (unsafe-lazy-array
                ds (λ: ([js : (Vectorof Index)])
                     (let: loop : A ([i : Nonnegative-Fixnum  0] [vec : (Vectorof* A)  vec])
                       (cond [(pred? vec)  vec]
                             [(i . < . dims)
                              (define j_i (unsafe-vector-ref js i))
                              (loop (+ i 1) (vector-ref vec j_i))]
                             [else  (error 'vector->array "internal error")]
                             )))))]
        [else  (raise-shape-error)]))

(: array->vector (All (A) ((Array A) -> (Vectorof* A))))
(define (array->vector arr)
  (let ([arr  (array-lazy arr)])
    (define ds (unsafe-array-shape arr))
    (define proc (unsafe-array-proc arr))
    (define dims (vector-length ds))
    (define: js : (Vectorof Index) (make-vector dims 0))
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
            [else  (proc js)]))))

(module* test typed/racket
  (require typed/rackunit
           (submod "..")
           "array-struct.rkt")
  (check-equal? (array->list (list->array flonum? '(1.0 2.0)))
                '(1.0 2.0))
  (check-equal? (array->list (list->array flonum? '((1.0 2.0) (3.0 4.0))))
                '((1.0 2.0) (3.0 4.0)))
  
  (check-equal? (array->vector (list->array flonum? '(1.0 2.0)))
                '#(1.0 2.0))
  (check-equal? (array->vector (list->array flonum? '((1.0 2.0) (3.0 4.0))))
                '#(#(1.0 2.0) #(3.0 4.0)))
  
  (check-equal? (array->vector ((inst vector->array Flonum) flonum? '#(1.0 2.0)))
                '#(1.0 2.0))
  (check-equal? (array->vector ((inst vector->array Flonum) flonum? '#(#(1.0 2.0) #(3.0 4.0))))
                '#(#(1.0 2.0) #(3.0 4.0)))
  
  (check-equal? (array->list (list->array flonum? 1.0)) 1.0)
  (check-equal? (array->vector (list->array flonum? 1.0)) 1.0))