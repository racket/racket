#lang typed/racket

(require racket/flonum
         math/array)

(printf "starting...~n")

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

(equal? (list-shape flonum? 0.0) '())
(equal? (list-shape flonum? '()) '(0))
(equal? (list-shape flonum? '(0.0)) '(1))
(equal? (list-shape flonum? '(0.0 0.0)) '(2))
(equal? (list-shape flonum? '((0.0 0.0 0.0) (0.0 0.0 0.0))) '(2 3))
(equal? (list-shape flonum? '((0.0 0.0 0.0) (0.0 0.0))) #f)
(equal? (list-shape flonum? '(() () ())) '(3 0))
(equal? (list-shape flonum? '((()) (()) (()))) '(3 1 0))
(equal? (list-shape flonum? '(((0.0)))) '(1 1 1))

(equal? (vector-shape flonum? 0.0) '())
(equal? (vector-shape flonum? #()) '(0))
(equal? ((inst vector-shape Float) flonum? #(0.0)) '(1))
(equal? ((inst vector-shape Float) flonum? #(0.0 0.0)) '(2))
(equal? ((inst vector-shape Float) flonum? #(#(0.0 0.0 0.0) #(0.0 0.0 0.0))) '(2 3))
(equal? ((inst vector-shape Float) flonum? #(#(0.0 0.0 0.0) #(0.0 0.0))) #f)
(equal? ((inst vector-shape Float) flonum? #(#() #() #())) '(3 0))
(equal? ((inst vector-shape Float) flonum? #(#(#()) #(#()) #(#()))) '(3 1 0))
(equal? ((inst vector-shape Float) flonum? #(#(#(0.0)))) '(1 1 1))

(let ([arr  (make-strict-array '(2 3 2) (vector 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0))])
  (values (equal? (array->vector arr)
                  #(#(#(1.0 2.0) #(3.0 4.0) #(5.0 6.0))
                    #(#(7.0 8.0) #(9.0 10.0) #(11.0 12.0))))
          (equal? arr (array-strict (vector->array flonum? (array->vector arr))))
          (equal? (array->list arr)
                  '(((1.0 2.0) (3.0 4.0) (5.0 6.0))
                    ((7.0 8.0) (9.0 10.0) (11.0 12.0))))
          (equal? arr (list->array flonum? (array->list arr)))))

(let ([arr  (make-strict-array '(3 0 0) (vector))])
  (values (equal? (array->vector arr) #(#() #() #()))
          (equal? (array->list arr) '(() () ()))))

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
