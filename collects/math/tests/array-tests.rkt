#lang typed/racket

(require racket/flonum
         math/array
         typed/rackunit)

(define-predicate listof-index? (Listof Index))
(define-predicate listof-flonum? (Listof Float))

;; ---------------------------------------------------------------------------------------------------
;; array-strict

(: make-index-vector (Integer Integer -> (Vectorof (Listof Integer))))
(define (make-index-vector n dims)
  (list->vector
   (let: loop : (Listof (Listof Integer)) ([n : Integer  n])
     (cond [(= n 0)  (list null)]
           [else  (define lsts (loop (- n 1)))
                  (append*
                   (for/list: : (Listof (Listof (Listof Integer))) ([k  (in-range dims)])
                     (map (λ: ([lst : (Listof Integer)]) (cons k lst)) lsts)))]))))

(check-equal? (make-index-vector 2 3)
              #((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))

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
                (make-index-vector 2 3)))

(let ([arr  (make-lazy-array '(2 2 2) (λ (js) js))])
  (check-equal? (unsafe-array-data (array-strict arr))
                (make-index-vector 3 2)))

(let ([arr  (make-lazy-array '(2 2 2 2) (λ (js) js))])
  (check-equal? (unsafe-array-data (array-strict arr))
                (make-index-vector 4 2)))

(let ([arr  (make-lazy-array '(2 2 2 2 2) (λ (js) js))])
  (check-equal? (unsafe-array-data (array-strict arr))
                (make-index-vector 5 2)))

(let ([arr  (make-lazy-array '(2 2 2 2 2 2) (λ (js) js))])
  (check-equal? (unsafe-array-data (array-strict arr))
                (make-index-vector 6 2)))

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
;; Not much to test here because most pointwise ops are defined by the same two lifts

;; array= is tested on other tests (e.g. FFT)

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

(check-exn exn? (λ () (array-fft (const-array '() 1))))
(check-exn exn? (λ () (array-fft (const-array '(0) 1))))
(check-exn exn? (λ () (array-fft (const-array '(3) 1))))

(let ([arr  (const-array '(4) 1)])
  (check array= (array-fft arr) (list->array number? '(4 0 0 0)))
  (check array= (array-inverse-fft (array-fft arr)) arr))

(let ([arr  (const-array '(2 2) 1)])
  (check array= (array-fft arr) (list->array number? '((4 0) (0 0))))
  (check array= (array-inverse-fft (array-fft arr)) arr))

;; ---------------------------------------------------------------------------------------------------
;; Ref

;; Unsafe

(let* ([l-arr  (const-array '() 0)]
       [s-arr  (array-strict l-arr)])
  (check-equal? (unsafe-array-ref* l-arr) 0)
  (check-equal? (unsafe-array-ref* s-arr) 0)
  (check-equal? (unsafe-array-ref l-arr (vector)) 0)
  (check-equal? (unsafe-array-ref s-arr (vector)) 0))

(let* ([l-arr  (indexes-array '(2))]
       [s-arr  (array-strict l-arr)])
  (check-equal? (unsafe-array-ref* l-arr 0) '(0))
  (check-equal? (unsafe-array-ref* s-arr 0) '(0))
  (check-equal? (unsafe-array-ref* l-arr 1) '(1))
  (check-equal? (unsafe-array-ref* s-arr 1) '(1))
  (check-equal? (unsafe-array-ref l-arr (ann (vector 0) (Vectorof Index))) '(0))
  (check-equal? (unsafe-array-ref s-arr (ann (vector 0) (Vectorof Index))) '(0))
  (check-equal? (unsafe-array-ref l-arr (ann (vector 1) (Vectorof Index))) '(1))
  (check-equal? (unsafe-array-ref s-arr (ann (vector 1) (Vectorof Index))) '(1)))

(let* ([l-arr  (indexes-array '(2 2))]
       [s-arr  (array-strict l-arr)])
  (check-equal? (unsafe-array-ref* l-arr 0 0) '(0 0))
  (check-equal? (unsafe-array-ref* s-arr 0 0) '(0 0))
  (check-equal? (unsafe-array-ref* l-arr 1 1) '(1 1))
  (check-equal? (unsafe-array-ref* s-arr 1 1) '(1 1))
  (check-equal? (unsafe-array-ref l-arr (ann (vector 0 0) (Vectorof Index))) '(0 0))
  (check-equal? (unsafe-array-ref s-arr (ann (vector 0 0) (Vectorof Index))) '(0 0))
  (check-equal? (unsafe-array-ref l-arr (ann (vector 1 1) (Vectorof Index))) '(1 1))
  (check-equal? (unsafe-array-ref s-arr (ann (vector 1 1) (Vectorof Index))) '(1 1)))

;; Safe

(let* ([l-arr  (const-array '() 0)]
       [s-arr  (array-strict l-arr)])
  (check-equal? (array-ref* l-arr) 0)
  (check-equal? (array-ref* s-arr) 0)
  (check-equal? (array-ref l-arr '()) 0)
  (check-equal? (array-ref s-arr '()) 0)
  (check-exn exn? (λ () (array-ref* l-arr 1)))
  (check-exn exn? (λ () (array-ref* s-arr 1)))
  (check-exn exn? (λ () (array-ref l-arr '(1))))
  (check-exn exn? (λ () (array-ref s-arr '(1)))))

(let* ([l-arr  (indexes-array '(2))]
       [s-arr  (array-strict l-arr)])
  (check-equal? (array-ref* l-arr 0) '(0))
  (check-equal? (array-ref* s-arr 0) '(0))
  (check-equal? (array-ref* l-arr 1) '(1))
  (check-equal? (array-ref* s-arr 1) '(1))
  (check-equal? (array-ref l-arr '(0)) '(0))
  (check-equal? (array-ref s-arr '(0)) '(0))
  (check-equal? (array-ref l-arr '(1)) '(1))
  (check-equal? (array-ref s-arr '(1)) '(1))
  (check-exn exn? (λ () (array-ref* l-arr 0 0)))
  (check-exn exn? (λ () (array-ref* s-arr 0 0)))
  (check-exn exn? (λ () (array-ref* l-arr -1)))
  (check-exn exn? (λ () (array-ref* s-arr -1)))
  (check-exn exn? (λ () (array-ref* l-arr 2)))
  (check-exn exn? (λ () (array-ref* s-arr 2)))
  (check-exn exn? (λ () (array-ref l-arr '(0 0))))
  (check-exn exn? (λ () (array-ref s-arr '(0 0))))
  (check-exn exn? (λ () (array-ref l-arr '(-1))))
  (check-exn exn? (λ () (array-ref s-arr '(-1))))
  (check-exn exn? (λ () (array-ref l-arr '(2))))
  (check-exn exn? (λ () (array-ref s-arr '(2)))))

(let* ([l-arr  (indexes-array '(2 2))]
       [s-arr  (array-strict l-arr)])
  (check-equal? (array-ref* l-arr 0 0) '(0 0))
  (check-equal? (array-ref* s-arr 0 0) '(0 0))
  (check-equal? (array-ref* l-arr 1 1) '(1 1))
  (check-equal? (array-ref* s-arr 1 1) '(1 1))
  (check-equal? (array-ref l-arr '(0 0)) '(0 0))
  (check-equal? (array-ref s-arr '(0 0)) '(0 0))
  (check-equal? (array-ref l-arr '(1 1)) '(1 1))
  (check-equal? (array-ref s-arr '(1 1)) '(1 1)))

;; ---------------------------------------------------------------------------------------------------
;; Transforms

(check-equal? (array->list
               (array-transform (indexes-array '(4 2))
                                '(2 4) (λ: ([js : (Listof Integer)])
                                         (match-define (list j0 j1) js)
                                         (list j1 j0))))
              '[[(0 0) (1 0) (2 0) (3 0)]
                [(0 1) (1 1) (2 1) (3 1)]])

(check-exn exn? (λ () (array->list (array-transform (indexes-array '(2 2)) '(3 3) identity))))
(check-exn exn? (λ () (array->list (array-transform (indexes-array '(2 2)) '(2) identity))))

(check-equal? (array->list
               (unsafe-array-transform (indexes-array '(4 2))
                                       (ann (vector 2 4) (Vectorof Index))
                                       (λ: ([js : (Vectorof Index)])
                                         (match-define (vector j0 j1) js)
                                         (ann (vector j1 j0) (Vectorof Index)))))
              '[[(0 0) (1 0) (2 0) (3 0)]
                [(0 1) (1 1) (2 1) (3 1)]])

;; Separable transforms (and slicing)

(check-equal? (array->list (array-axis-transform
                            (indexes-array '(4 4))
                            '((3 0) (1 2))))
              '[[(3 1) (3 2)]
                [(0 1) (0 2)]])

(check-exn exn? (λ () (array-axis-transform (indexes-array '(4 4)) '((0 3)))))
(check-exn exn? (λ () (array-axis-transform (indexes-array '(4 4)) '((0 4) (1 2)))))

(check-equal? (array->list (array-slice (indexes-array '(4 4))
                                        (list (in-range 0 4 2) (in-range 2 -1 -2))))
              '[[(0 2) (0 0)]
                [(2 2) (2 0)]])

;; Permutation

(let ([arr  (const-array '() 0)])
  (check-equal? (array->list (array-axis-permute arr '()))
                0)
  (check-exn exn? (λ () (array-axis-permute arr '(0)))))

(let ([arr  (indexes-array '(4))])
  (check-exn exn? (λ () (array-axis-permute arr '())))
  (check-equal? (array-axis-permute arr '(0)) arr)
  (check-exn exn? (λ () (array-axis-permute arr '(1)))))

(let ([arr  (indexes-array '(2 2))])
  (check-exn exn? (λ () (array-axis-permute arr '())))
  (check-equal? (array-axis-permute arr '(0 1)) arr)
  (check-equal? (array->list (array-axis-permute arr '(1 0)))
                '[[(0 0) (1 0)]
                  [(0 1) (1 1)]]))

;; Transposition

(let ([arr  (indexes-array '(4))])
  (check-exn exn? (λ () (array-axis-swap arr 0 1)))
  (check-equal? (array-axis-swap arr 0 0) arr))

(let ([arr  (indexes-array '(2 2))])
  (check-exn exn? (λ () (array-axis-swap arr 0 2)))
  (check-equal? (array-axis-swap arr 0 0) arr)
  (check-equal? (array->list (array-axis-swap arr 1 0))
                '[[(0 0) (1 0)]
                  [(0 1) (1 1)]]))

(check-equal? (array->list (array-axis-swap (indexes-array '(2 2 2)) 1 2))
              '[[[(0 0 0) (0 1 0)]
                 [(0 0 1) (0 1 1)]]
                [[(1 0 0) (1 1 0)]
                 [(1 0 1) (1 1 1)]]])

;; Adding axes

(let ([arr  (indexes-array '())])
  (check-exn exn? (λ () (array-axis-insert arr 1 1)))
  (check-equal? (array->list (array-axis-insert arr 0 2))
                '[() ()]))

(let ([arr  (indexes-array '(4))])
  (check-equal? (array->list (array-axis-insert arr 0 2))
                '[[(0) (1) (2) (3)]
                  [(0) (1) (2) (3)]])
  (check-equal? (array->list (array-axis-insert arr 1 2))
                '[[(0) (0)] [(1) (1)] [(2) (2)] [(3) (3)]]))

;; Removing axes

(let ([arr  (indexes-array '())])
  (check-exn exn? (λ () (array-axis-remove arr 0 0))))

(let ([arr  (indexes-array '(4))])
  (check-exn exn? (λ () (array-axis-remove arr 1 0)))
  (check-equal? (array->list (array-axis-remove arr 0 2))
                '(2)))

(let ([arr  (indexes-array '(2 2))])
  (check-equal? (array->list (array-axis-remove arr 0 0))
                '[(0 0) (0 1)])
  (check-equal? (array->list (array-axis-remove arr 1 0))
                '[(0 0) (1 0)]))
