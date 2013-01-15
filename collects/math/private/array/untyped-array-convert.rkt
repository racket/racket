#lang racket/base

(require racket/unsafe/ops
         typed/racket/base
         "array-struct.rkt"
         "mutable-array.rkt"
         "utils.rkt")

(provide make-to-array-functions list*->array vector*->array)

(define-syntax-rule (make-to-array-functions list*->array vector*->array)
  (begin
    ;; ===============================================================================================
    ;; Helpers

    (: maybe-list->vector (All (A) ((U #f (Listof A)) -> (U #f (Vectorof A)))))
    (define (maybe-list->vector vs)
      (and vs (list->vector vs)))
    
    (: vector-shape (All (A) ((Vectorof* A) ((Vectorof* A) -> Any : A)
                                            -> (U #f (Vectorof Integer)))))
    (define (vector-shape vec pred?)
      (maybe-list->vector
       (let: vector-shape : (U #f (Listof Integer)) ([vec : (Vectorof* A)  vec])
         (cond [(pred? vec)  (list)]
               [else
                (define d (vector-length vec))
                (cond [(= d 0)  (list 0)]
                      [else
                       (define ds (vector-shape (vector-ref vec 0)))
                       (if ds
                           (let: loop : (U #f (Listof Integer)) ([i : Nonnegative-Fixnum 1])
                             (cond [(i . >= . d)  (cons d ds)]
                                   [(equal? ds (vector-shape (vector-ref vec i)))
                                    (loop (+ i 1))]
                                   [else  #f]))
                           #f)])]))))
    
    (: list-shape (All (A) ((Listof* A) ((Listof* A) -> Any : A) -> (U #f (Vectorof Integer)))))
    (define (list-shape lst pred?)
      (maybe-list->vector
       (let: list-shape : (U #f (Listof Integer)) ([lst : (Listof* A)  lst])
         (cond [(pred? lst)  (list)]
               [(null? lst)  (list 0)]
               [else
                (define d (length lst))
                (define ds (list-shape (car lst)))
                (if ds
                    (let loop ([lst  (cdr lst)])
                      (cond [(null? lst)  (cons d ds)]
                            [(equal? ds (list-shape (car lst)))
                             (loop (cdr lst))]
                            [else  #f]))
                    #f)]))))
    
    ;; ===============================================================================================
    ;; Conversion to arrays
    
    (: first* (All (A) ((Listof* A) ((Listof* A) -> Any : A) -> A)))
    (define (first* lst pred?)
      (let/ec: return : A
        (let: loop : A ([lst : (Listof* A)  lst])
          (cond [(pred? lst)  (return lst)]
                [else  (for ([lst  (in-list lst)])
                         (loop lst))
                       (error 'first* "no first* element")]))))
    
    (: list*->flat-vector (All (A) ((Listof* A) Integer ((Listof* A) -> Any : A) -> (Vectorof A))))
    (define (list*->flat-vector lst size pred?)
      (cond [(zero? size)  (vector)]
            [else
             (define vec (make-vector size (first* lst pred?)))
             (let: loop : Fixnum ([lst : (Listof* A)  lst] [i : Fixnum  0])
               (cond [(pred? lst)  (unsafe-vector-set! vec i lst)
                                   (unsafe-fx+ i 1)]
                     [else  (for/fold: ([i : Fixnum  i]) ([lst  (in-list lst)])
                              (loop lst i))]))
             vec]))
    
    (: list*->array (All (A) ((Listof* A) ((Listof* A) -> Any : A) -> (Array A))))
    (define (list*->array lst pred?)
      (define (raise-shape-error)
        ;; don't have to worry about non-Index size - can't fit in memory anyway
        (raise-argument-error 'list*->array "rectangular (Listof* A)" lst))
      
      (define ds (list-shape lst pred?))
      (cond [(pred? lst)  (unsafe-build-simple-array #() (λ (js) lst))]
            [ds  (let ([ds  (check-array-shape ds raise-shape-error)])
                   (define size (array-shape-size ds))
                   (unsafe-vector->array ds (list*->flat-vector lst size pred?)))]
            [else  (raise-shape-error)]))
    
    (: vector*->array (All (A) ((Vectorof* A) ((Vectorof* A) -> Any : A) -> (Mutable-Array A))))
    (define (vector*->array vec pred?)
      (define (raise-shape-error)
        ;; don't have to worry about non-Index size - can't fit in memory anyway
        (raise-argument-error 'vector*->array "rectangular (Vectorof* A)" vec))
      
      (define ds (vector-shape vec pred?))
      (cond [(pred? vec)  (array->mutable-array (unsafe-build-simple-array #() (λ (js) vec)))]
            [ds  (let ([ds  (check-array-shape ds raise-shape-error)])
                   (define dims (vector-length ds))
                   (array->mutable-array
                    (unsafe-build-array
                     ds (λ: ([js : Indexes])
                          (let: loop : A ([i : Nonnegative-Fixnum  0] [vec : (Vectorof* A)  vec])
                            (cond [(pred? vec)  vec]
                                  [(i . < . dims)
                                   (define j_i (unsafe-vector-ref js i))
                                   (loop (+ i 1) (vector-ref vec j_i))]
                                  [else  (error 'vector*->array "internal error")]))))))]
            [else  (raise-shape-error)]))
    )  ; begin
  )  ; make-conversion-functions

(make-to-array-functions list*->array vector*->array)
