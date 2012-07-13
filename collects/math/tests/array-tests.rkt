#lang typed/racket

(require racket/flonum
         math/array
         typed/rackunit)

#|
TODO

Printing tests
Moar FFT tests
Ref tests
Transform tests
|#

(define-predicate listof-index? (Listof Index))
(define-predicate listof-flonum? (Listof Float))

;; ---------------------------------------------------------------------------------------------------
;; array-strict

(let ([arr  (make-strict-array '(4) #(a b c d))])
  (check-eq? arr (array-strict arr)))

(let ([arr  (make-lazy-array '() (λ (js) 'foo))])
  (check-equal? (unsafe-array-data (array-strict arr))
                #(foo)))

(let ([arr  ((inst make-lazy-array Float) '(4) (λ (js) (exact->inexact (first js))))])
  (check-equal? (unsafe-array-data (array-strict arr))
                #(0.0 1.0 2.0 3.0)))

(let ([arr  ((inst make-lazy-array Float) '(0) (λ (js) (exact->inexact (first js))))])
  (check-equal? (unsafe-array-data (array-strict arr))
                #()))

(let ([arr  (make-lazy-array '(3 3) (λ (js) js))])
  (check-equal? (unsafe-array-data (array-strict arr))
                #((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))))

(let ([arr  (make-lazy-array '(2 2 2) (λ (js) js))])
  (check-equal? (unsafe-array-data (array-strict arr))
                #((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))))

(let ([arr  (make-lazy-array '(2 2 2 2) (λ (js) js))])
  (check-equal? (unsafe-array-data (array-strict arr))
                (vector '(0 0 0 0) '(0 0 0 1) '(0 0 1 0) '(0 0 1 1)
                        '(0 1 0 0) '(0 1 0 1) '(0 1 1 0) '(0 1 1 1)
                        '(1 0 0 0) '(1 0 0 1) '(1 0 1 0) '(1 0 1 1)
                        '(1 1 0 0) '(1 1 0 1) '(1 1 1 0) '(1 1 1 1))))

;; ---------------------------------------------------------------------------------------------------
;; Printing

;; ---------------------------------------------------------------------------------------------------
;; Conversion

;; list->array

(check-equal? (array-strict (list->array flonum? 1.0))
              (make-strict-array '() #(1.0)))

(check-equal? (array-strict (list->array flonum? '()))
              (make-strict-array '(0) #()))

(check-equal? (array-strict (list->array flonum? '(())))
              (make-strict-array '(1 0) #()))

(check-equal? (array-strict (list->array flonum? '(1.0)))
              (make-strict-array '(1) #(1.0)))

(check-equal? (array-strict (list->array flonum? '((1.0))))
              (make-strict-array '(1 1) #(1.0)))

(check-equal? (array-strict (list->array flonum? '(((1.0)))))
              (make-strict-array '(1 1 1) #(1.0)))

;; vector->array

(check-equal? (array-strict (vector->array flonum? 1.0))
              (make-strict-array '() #(1.0)))

(check-equal? (array-strict ((inst vector->array Float) flonum? #()))
              (make-strict-array '(0) #()))

(check-equal? (array-strict ((inst vector->array Float) flonum? #(#())))
              (make-strict-array '(1 0) #()))

(check-equal? (array-strict ((inst vector->array Float) flonum? #(1.0)))
              (make-strict-array '(1) #(1.0)))

(check-equal? (array-strict ((inst vector->array Float) flonum? #(#(1.0))))
              (make-strict-array '(1 1) #(1.0)))

(check-equal? (array-strict ((inst vector->array Float) flonum? #(#(#(1.0)))))
              (make-strict-array '(1 1 1) #(1.0)))

;; array->list

(let ([arr  (make-lazy-array '() (λ (js) 'foo))])
  (check-equal? (array->list arr) 'foo)
  (check-equal? arr (list->array symbol? (array->list arr))))

(let ([arr  ((inst make-lazy-array Float) '(4) (λ (js) (exact->inexact (first js))))])
  (check-equal? (array->list arr) '(0.0 1.0 2.0 3.0))
  (check-equal? arr (list->array flonum? (array->list arr))))

(let ([arr  (make-lazy-array '(3 3) (λ: ([js : (Listof Index)]) js))])
  (check-equal? (array->list arr) '(((0 0) (0 1) (0 2))
                                    ((1 0) (1 1) (1 2))
                                    ((2 0) (2 1) (2 2))))
  (check-equal? arr (list->array listof-index? (array->list arr))))

(let ([arr  (make-lazy-array '(2 2 2) (λ: ([js : (Listof Index)]) js))])
  (check-equal? (array->list arr)
                '((((0 0 0) (0 0 1)) ((0 1 0) (0 1 1)))
                  (((1 0 0) (1 0 1)) ((1 1 0) (1 1 1)))))
  (check-equal? arr (list->array listof-index? (array->list arr))))

(let ([arr  (make-lazy-array '(2 2 2 2) (λ: ([js : (Listof Index)]) js))])
  (check-equal? (array->list arr)
                '(((((0 0 0 0) (0 0 0 1)) ((0 0 1 0) (0 0 1 1)))
                   (((0 1 0 0) (0 1 0 1)) ((0 1 1 0) (0 1 1 1))))
                  ((((1 0 0 0) (1 0 0 1)) ((1 0 1 0) (1 0 1 1)))
                   (((1 1 0 0) (1 1 0 1)) ((1 1 1 0) (1 1 1 1))))))
  (check-equal? arr (list->array listof-index? (array->list arr))))

;; array->vector

(let ([arr  (make-lazy-array '() (λ (js) 'foo))])
  (check-equal? (array->vector arr) 'foo)
  (check-equal? arr (vector->array symbol? (array->vector arr))))

(let ([arr  ((inst make-lazy-array Float) '(4) (λ (js) (exact->inexact (first js))))])
  (check-equal? (array->vector arr) #(0.0 1.0 2.0 3.0))
  (check-equal? arr (vector->array flonum? (array->vector arr))))

(let ([arr  (make-lazy-array '(3 3) (λ: ([js : (Listof Index)]) js))])
  (check-equal? (array->vector arr) #(#((0 0) (0 1) (0 2))
                                      #((1 0) (1 1) (1 2))
                                      #((2 0) (2 1) (2 2))))
  (check-equal? arr (vector->array listof-index? (array->vector arr))))

(let ([arr  (make-lazy-array '(2 2 2) (λ: ([js : (Listof Index)]) js))])
  (check-equal? (array->vector arr)
                #(#(#((0 0 0) (0 0 1)) #((0 1 0) (0 1 1)))
                  #(#((1 0 0) (1 0 1)) #((1 1 0) (1 1 1)))))
  (check-equal? arr (vector->array listof-index? (array->vector arr))))

(let ([arr  (make-lazy-array '(2 2 2 2) (λ: ([js : (Listof Index)]) js))])
  (check-equal? (array->vector arr)
                #(#(#(#((0 0 0 0) (0 0 0 1)) #((0 0 1 0) (0 0 1 1)))
                    #(#((0 1 0 0) (0 1 0 1)) #((0 1 1 0) (0 1 1 1))))
                  #(#(#((1 0 0 0) (1 0 0 1)) #((1 0 1 0) (1 0 1 1)))
                    #(#((1 1 0 0) (1 1 0 1)) #((1 1 1 0) (1 1 1 1))))))
  (check-equal? arr (vector->array listof-index? (array->vector arr))))

(check-equal? (array->list (list->array flonum? '(1.0 2.0)))
              '(1.0 2.0))
(check-equal? (array->list (list->array flonum? '((1.0 2.0) (3.0 4.0))))
              '((1.0 2.0) (3.0 4.0)))

(check-equal? (array->vector ((inst vector->array Flonum) flonum? '#(1.0 2.0)))
              '#(1.0 2.0))
(check-equal? (array->vector ((inst vector->array Flonum) flonum? '#(#(1.0 2.0) #(3.0 4.0))))
              '#(#(1.0 2.0) #(3.0 4.0)))

(check-equal? (array->list ((inst vector->array Flonum) flonum? #(1.0 2.0)))
              '(1.0 2.0))
(check-equal? (array->list ((inst vector->array Flonum) flonum? #(#(1.0 2.0) #(3.0 4.0))))
              '((1.0 2.0) (3.0 4.0)))

;; ---------------------------------------------------------------------------------------------------
;; Other constructors

(check-equal? (const-array '(3 3) 0)
              (make-lazy-array '(3 3) (λ (js) 0)))

(check-equal? (index-array '(3 3) 0)
              (make-lazy-array '(3 3) (λ: ([js : (Listof Integer)]) (first js))))

(check-exn exn? (λ () (index-array '(3 3) -1)))
(check-exn exn? (λ () (index-array '() 0)))

(check-equal? (indexes-array '(3 3))
              (make-lazy-array '(3 3) (λ: ([js : (Listof Integer)]) js)))

(check-equal? (diagonal-array 0 0 1.0 0.0)
              (list->array flonum? 1.0))

(check-equal? (diagonal-array 1 0 1.0 0.0)
              (list->array flonum? '()))

(check-equal? (diagonal-array 1 1 1.0 0.0)
              (list->array flonum? '(1.0)))

(check-equal? (diagonal-array 2 1 1.0 0.0)
              (list->array flonum? '((1.0))))

(check-equal? (diagonal-array 2 2 1.0 0.0)
              (list->array flonum? '((1.0 0.0) (0.0 1.0))))

(check-equal? (diagonal-array 3 2 1.0 0.0)
              (list->array flonum? '(((1.0 0.0) (0.0 0.0))
                                     ((0.0 0.0) (0.0 1.0)))))

;; ---------------------------------------------------------------------------------------------------
;; Pointwise

(check-equal? (array-sqrt (list->array flonum? '(1.0 4.0 9.0 16.0)))
              (list->array flonum? '(1.0 2.0 3.0 4.0)))

(check-equal? (array+ (list->array flonum? '(1.0 2.0))
                      (list->array flonum? '(10.0 20.0)))
              (list->array flonum? '(11.0 22.0)))

(check-equal? (array-map inexact->exact (list->array flonum? '(1.0 2.0)))
              (list->array number? '(1 2)))

(check-equal? (array-map2 (inst cons Float Float)
                          (list->array flonum? '(1.0 2.0))
                          (list->array flonum? '(10.0 20.0)))
              (make-strict-array '(2) #((1.0 . 10.0) (2.0 . 20.0))))

;; ---------------------------------------------------------------------------------------------------
;; Fold

(let ([arr  (list->array flonum? '(( 1.0  4.0  9.0  16.0)
                                   (-1.0 -4.0 -9.0 -16.0)))])
  (check-equal? (array-axis-sum arr 0) (list->array flonum? '(0.0 0.0 0.0 0.0)))
  (check-equal? (array-axis-min arr 0) (list->array flonum? '(-1.0 -4.0 -9.0 -16.0)))
  (check-equal? (array-axis-max arr 0) (list->array flonum? '( 1.0  4.0  9.0  16.0)))
  (check-equal? (array-axis-fold (inst cons Float (Listof Float)) null arr 0)
                (list->array listof-flonum? '((-1.0 1.0) (-4.0 4.0) (-9.0 9.0) (-16.0 16.0))))
  (check-equal? (array-axis-sum arr 1) (list->array flonum? '(30.0 -30.0)))
  (check-equal? (array-axis-min arr 1) (list->array flonum? '(1.0 -16.0)))
  (check-equal? (array-axis-max arr 1) (list->array flonum? '(16.0 -1.0)))
  (check-equal? (array-axis-fold (inst cons Float (Listof Float)) null arr 1)
                (list->array listof-flonum? '(( 16.0  9.0  4.0  1.0)
                                              (-16.0 -9.0 -4.0 -1.0)))))

(let ([arr  (const-array '(3 0) 0)])
  (check-equal? (array-axis-sum arr 0) (list->array number? '()))
  (check-equal? (array-axis-min arr 0) (list->array number? '()))
  (check-equal? (array-axis-max arr 0) (list->array number? '()))
  (check-equal? (array-axis-sum arr 1) (list->array number? '(0 0 0)))
  (check-equal? (array-axis-min arr 1) (list->array number? '(+inf.0 +inf.0 +inf.0)))
  (check-equal? (array-axis-max arr 1) (list->array number? '(-inf.0 -inf.0 -inf.0))))

(check-exn exn? (λ () (array-axis-sum (const-array '() 0) 0)))
(check-equal? (array-axis-sum (const-array '(4) 1) 0)
              (list->array number? 4))

;; ---------------------------------------------------------------------------------------------------
;; FFT

(let ([arr  (const-array '(2 2) 1)])
  (check-true (array= (array-fft arr) (list->array number? '((4 0) (0 0)))))
  (check-true (array= arr (array-inverse-fft (array-fft arr)))))

;; ---------------------------------------------------------------------------------------------------

#|

(let ([arr  (array-map (inst list->vector Integer) (indexes-array '(2 3)))])
  (values (list arr)
          (array-permute arr (list 0 (new-axis 4) 1 (new-axis 4)))))

(struct: bob ([a : Integer] [b : Integer] [c : Symbol]) #:transparent)

(list->array bob? (list (list (bob 1 2 'c) (bob 3 4 'd))
                        (list (bob 10 20 'e) (bob 30 40 'z))))

(define iters 2)

(: fladd1 (Float -> Float))
(define (fladd1 x) (+ x 1.0))

(define ds '(1000 1000))
(define arr0 (array->flarray (index-array ds 0)))
(define arr1 (array-map fladd1 arr0))

(define arrn
  (for/fold: : (Array Float) ([arr arr0]) ([n  (in-range 3)])
    (array-map fladd1 arr)))

(define 10x10 (array+ (index-array '(10 10) 0)
                      (index-array '(10 10) 1)))

(array-strict 10x10)

(array-strict
 (array-slice 10x10
              (list (in-range 0 10 2)
                    (in-range 0 9))))

(array-strict
 (array-slice 10x10
              (list (in-range 0 10 2)
                    (in-range 9 -1 -1))))

(array-strict
 (array-transpose 10x10))

(array-strict
 (array-transpose
  (array-slice 10x10
               (list (in-range 0 10 2)
                     (in-range 9 -1 -1)))))

(array-strict
 (array-transform
  10x10
  (array-shape 10x10)
  (λ: ([js : (Listof Integer)])
    (match js
      [(list j0 j1)  (list (* 2 (floor (/ j1 2))) j0)]))))


(check-equal? (list-shape flonum? 0.0) '())
(check-equal? (list-shape flonum? '()) '(0))
(check-equal? (list-shape flonum? '(0.0)) '(1))
(check-equal? (list-shape flonum? '(0.0 0.0)) '(2))
(check-equal? (list-shape flonum? '((0.0 0.0 0.0) (0.0 0.0 0.0))) '(2 3))
(check-equal? (list-shape flonum? '((0.0 0.0 0.0) (0.0 0.0))) #f)
(check-equal? (list-shape flonum? '(() () ())) '(3 0))
(check-equal? (list-shape flonum? '((()) (()) (()))) '(3 1 0))
(check-equal? (list-shape flonum? '(((0.0)))) '(1 1 1))

(check-equal? (vector-shape flonum? 0.0) '())
(check-equal? (vector-shape flonum? #()) '(0))
(check-equal? ((inst vector-shape Float) flonum? #(0.0)) '(1))
(check-equal? ((inst vector-shape Float) flonum? #(0.0 0.0)) '(2))
(check-equal? ((inst vector-shape Float) flonum? #(#(0.0 0.0 0.0) #(0.0 0.0 0.0))) '(2 3))
(check-equal? ((inst vector-shape Float) flonum? #(#(0.0 0.0 0.0) #(0.0 0.0))) #f)
(check-equal? ((inst vector-shape Float) flonum? #(#() #() #())) '(3 0))
(check-equal? ((inst vector-shape Float) flonum? #(#(#()) #(#()) #(#()))) '(3 1 0))
(check-equal? ((inst vector-shape Float) flonum? #(#(#(0.0)))) '(1 1 1))

(define arr2  (make-strict-array '(2 3 2) (vector 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0)))
(check-equal? (array->vector arr2)
              #(#(#(1.0 2.0) #(3.0 4.0) #(5.0 6.0))
                #(#(7.0 8.0) #(9.0 10.0) #(11.0 12.0))))
(check-equal? arr2 (array-strict (vector->array flonum? (array->vector arr2))))
(check-equal? (array->list arr2)
              '(((1.0 2.0) (3.0 4.0) (5.0 6.0))
                ((7.0 8.0) (9.0 10.0) (11.0 12.0))))

; comparing two equal strict arrays with equal? does not work ?
(check-equal? arr2 (array-strict (list->array flonum? (array->list arr2))))

(define arr3  (make-strict-array '(3 0 0) (vector)))
(check-equal? (array->vector arr3) #(#() #() #()))
(check-equal? (array->list arr3) '(() () ()))

;; Timing tests
#|
(for ([_  (in-range 5)])
  (time (for ([_  (in-range iters)])
          (array-strict arr0))))

(newline)

(for ([_  (in-range 5)])
  (time (for ([_  (in-range iters)])
          (array-strict arr1))))

(newline)

(for ([_  (in-range 5)])
  (time (for ([_  (in-range iters)])
          (array-strict arrn))))

(newline)

(let ([arr  (const-array '(10 10) 1.0)])
  (for ([_  (in-range 5)])
    (time (for ([_  (in-range 100000)])
            (array-strict (array-transpose arr 0 1))))))

(newline)

(let ([arr  (const-array '(10 10) 1.0)]
      [slice  (list (in-range 0 10) (in-range 0 10))])
  (for ([_  (in-range 5)])
    (time (for ([_  (in-range 100000)])
            (array-strict (array-slice arr slice))))))
|#
|#