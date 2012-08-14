#lang typed/racket

(require racket/vector racket/flonum
         "../unsafe.rkt"
         "array-struct.rkt"
         "mutable-array.rkt"
         "utils.rkt")

(provide list->array
         array->list
         vector->array
         array->vector)

;; ===================================================================================================
;; Conversion to and from lists

(: first* (All (A) ((Listof* A) ((Listof* A) -> Boolean : A) -> A)))
(define (first* lst pred?)
  (let/ec: return : A
    (let: loop : A ([lst : (Listof* A)  lst])
      (cond [(pred? lst)  (return lst)]
            [else  (for ([lst  (in-list lst)])
                     (loop lst))
                   (error 'first* "no first* element")]))))

(: list->flat-vector (All (A) ((Listof* A) Integer ((Listof* A) -> Boolean : A) -> (Vectorof A))))
(define (list->flat-vector lst size pred?)
  (cond [(zero? size)  (vector)]
        [else
         (define vec (make-vector size (first* lst pred?)))
         (let: loop : Fixnum ([lst : (Listof* A)  lst] [i : Fixnum  0])
           (cond [(pred? lst)  (unsafe-vector-set! vec i lst)
                               (unsafe-fx+ i 1)]
                 [else  (for/fold: ([i : Fixnum  i]) ([lst  (in-list lst)])
                          (loop lst i))]))
         vec]))

(: list->array (All (A) ((Listof* A) (Any -> Boolean : A) -> (Array A))))
(define (list->array lst pred?)
  (define (raise-shape-error)
    ;; don't have to worry about non-Index size - can't fit in memory anyway
    (raise-type-error 'list->array "rectangular (Listof* A)" lst))
  
  (define ds (list-shape pred? lst))
  (cond [(pred? lst)  (unsafe-build-array #() (λ (js) lst))]
        [ds  (let ([ds  (check-array-shape ds raise-shape-error)])
               (define size (array-shape-size ds))
               (unsafe-mutable-array ds (list->flat-vector lst size pred?)))]
        [else  (raise-shape-error)]))

(: array->list (All (A) ((Array A) -> (Listof* A))))
(define (array->list arr)
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

;; ===================================================================================================
;; Conversion to and from vectors

(: vector->array (All (A) ((Vectorof* A) (Any -> Boolean : A) -> (Array A))))
(define (vector->array vec pred?)
  (define (raise-shape-error)
    ;; don't have to worry about non-Index size - can't fit in memory anyway
    (raise-type-error 'vector->array "rectangular (Vectorof* A)" vec))
  
  (define ds (vector-shape pred?  vec))
  (cond [(pred? vec)  (unsafe-build-array #() (λ (js) vec))]
        [ds  (let ([ds  (check-array-shape ds raise-shape-error)])
               (define dims (vector-length ds))
               (unsafe-build-array
                ds (λ: ([js : Indexes])
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
