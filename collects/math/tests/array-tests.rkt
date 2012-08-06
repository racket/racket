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

(let ([arr  (make-view-array '() (λ (js) 'foo))])
  (check-equal? (strict-array-data (array-strict arr))
                #(foo)))

(let ([arr  ((inst make-view-array Float) '(4) (λ (js) (exact->inexact (first js))))])
  (check-equal? (strict-array-data (array-strict arr))
                #(0.0 1.0 2.0 3.0)))

(let ([arr  ((inst make-view-array Float) '(0) (λ (js) (exact->inexact (first js))))])
  (check-equal? (strict-array-data (array-strict arr))
                #()))

(let ([arr  (make-view-array '(3 3) (λ (js) js))])
  (check-equal? (strict-array-data (array-strict arr))
                (make-index-vector 2 3)))

(let ([arr  (make-view-array '(2 2 2) (λ (js) js))])
  (check-equal? (strict-array-data (array-strict arr))
                (make-index-vector 3 2)))

(let ([arr  (make-view-array '(2 2 2 2) (λ (js) js))])
  (check-equal? (strict-array-data (array-strict arr))
                (make-index-vector 4 2)))

(let ([arr  (make-view-array '(2 2 2 2 2) (λ (js) js))])
  (check-equal? (strict-array-data (array-strict arr))
                (make-index-vector 5 2)))

(let ([arr  (make-view-array '(2 2 2 2 2 2) (λ (js) js))])
  (check-equal? (strict-array-data (array-strict arr))
                (make-index-vector 6 2)))

;; ---------------------------------------------------------------------------------------------------
;; array-copy

(let* ([arr  ((inst array-strict Byte) (make-array '() 0))]
       [brr  (array-copy arr)])
  (array-set! arr '() 1)
  (check-equal? (array-ref brr '()) 0)
  (array-set! brr '() 2)
  (check-equal? (array-ref arr '()) 1))

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

(let ([arr  (make-view-array '() (λ (js) 'foo))])
  (check-equal? (array->list arr) 'foo)
  (check-equal? arr (list->array symbol? (array->list arr))))

(let ([arr  ((inst make-view-array Float) '(4) (λ (js) (exact->inexact (first js))))])
  (check-equal? (array->list arr) '(0.0 1.0 2.0 3.0))
  (check-equal? arr (list->array flonum? (array->list arr))))

(let ([arr  (make-view-array '(3 3) (λ: ([js : (Listof Index)]) js))])
  (check-equal? (array->list arr) '(((0 0) (0 1) (0 2))
                                    ((1 0) (1 1) (1 2))
                                    ((2 0) (2 1) (2 2))))
  (check-equal? arr (list->array listof-index? (array->list arr))))

(let ([arr  (make-view-array '(2 2 2) (λ: ([js : (Listof Index)]) js))])
  (check-equal? (array->list arr)
                '((((0 0 0) (0 0 1)) ((0 1 0) (0 1 1)))
                  (((1 0 0) (1 0 1)) ((1 1 0) (1 1 1)))))
  (check-equal? arr (list->array listof-index? (array->list arr))))

(let ([arr  (make-view-array '(2 2 2 2) (λ: ([js : (Listof Index)]) js))])
  (check-equal? (array->list arr)
                '(((((0 0 0 0) (0 0 0 1)) ((0 0 1 0) (0 0 1 1)))
                   (((0 1 0 0) (0 1 0 1)) ((0 1 1 0) (0 1 1 1))))
                  ((((1 0 0 0) (1 0 0 1)) ((1 0 1 0) (1 0 1 1)))
                   (((1 1 0 0) (1 1 0 1)) ((1 1 1 0) (1 1 1 1))))))
  (check-equal? arr (list->array listof-index? (array->list arr))))

;; array->vector

(let ([arr  (make-view-array '() (λ (js) 'foo))])
  (check-equal? (array->vector arr) 'foo)
  (check-equal? arr (vector->array symbol? (array->vector arr))))

(let ([arr  ((inst make-view-array Float) '(4) (λ (js) (exact->inexact (first js))))])
  (check-equal? (array->vector arr) #(0.0 1.0 2.0 3.0))
  (check-equal? arr (vector->array flonum? (array->vector arr))))

(let ([arr  (make-view-array '(3 3) (λ: ([js : (Listof Index)]) js))])
  (check-equal? (array->vector arr) #(#((0 0) (0 1) (0 2))
                                      #((1 0) (1 1) (1 2))
                                      #((2 0) (2 1) (2 2))))
  (check-equal? arr (vector->array listof-index? (array->vector arr))))

(let ([arr  (make-view-array '(2 2 2) (λ: ([js : (Listof Index)]) js))])
  (check-equal? (array->vector arr)
                #(#(#((0 0 0) (0 0 1)) #((0 1 0) (0 1 1)))
                  #(#((1 0 0) (1 0 1)) #((1 1 0) (1 1 1)))))
  (check-equal? arr (vector->array listof-index? (array->vector arr))))

(let ([arr  (make-view-array '(2 2 2 2) (λ: ([js : (Listof Index)]) js))])
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

(check-equal? (make-array '(3 3) 0)
              (make-view-array '(3 3) (λ (js) 0)))

(check-equal? (index-array '(3 3) 0)
              (make-view-array '(3 3) (λ: ([js : (Listof Integer)]) (first js))))

(check-exn exn? (λ () (index-array '(3 3) -1)))
(check-exn exn? (λ () (index-array '() 0)))

(check-equal? (indexes-array '(3 3))
              (make-view-array '(3 3) (λ: ([js : (Listof Integer)]) js)))

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
  (check-equal? (array-axis-prod arr 0) (list->array flonum? '(-1.0 -16.0 -81.0 -256.0)))
  (check-equal? (array-axis-min arr 0) (list->array flonum? '(-1.0 -4.0 -9.0 -16.0)))
  (check-equal? (array-axis-max arr 0) (list->array flonum? '( 1.0  4.0  9.0  16.0)))
  (check-equal? (array-axis-fold arr 0 (inst cons Float (Listof Float)) null)
                (list->array listof-flonum? '((-1.0 1.0) (-4.0 4.0) (-9.0 9.0) (-16.0 16.0))))
  (check-equal? (array-axis-sum arr 1) (list->array flonum? '(30.0 -30.0)))
  (check-equal? (array-axis-prod arr 1) (list->array flonum? '(576.0 576.0)))
  (check-equal? (array-axis-min arr 1) (list->array flonum? '(1.0 -16.0)))
  (check-equal? (array-axis-max arr 1) (list->array flonum? '(16.0 -1.0)))
  (check-equal? (array-axis-fold arr 1 (inst cons Float (Listof Float)) null)
                (list->array listof-flonum? '(( 16.0  9.0  4.0  1.0)
                                              (-16.0 -9.0 -4.0 -1.0))))
  (check-equal? (array-sum arr) 0.0)
  (check-equal? (array-prod arr) (* 576.0 576.0))
  (check-equal? (array-min-value arr) -16.0)
  (check-equal? (array-max-value arr)  16.0))

(let ([arr  (make-array '(3 0) 0)])
  (check-equal? (array-axis-sum arr 0) (list->array number? '()))
  (check-equal? (array-axis-prod arr 0) (list->array number? '()))
  (check-equal? (array-axis-min arr 0) (list->array number? '()))
  (check-equal? (array-axis-max arr 0) (list->array number? '()))
  (check-exn exn? (λ () (array-axis-sum arr 1)))
  (check-exn exn? (λ () (array-axis-prod arr 1)))
  (check-exn exn? (λ () (array-axis-min arr 1)))
  (check-exn exn? (λ () (array-axis-max arr 1)))
  (check-equal? (array-axis-sum arr 1 0) (list->array number? '(0 0 0)))
  (check-equal? (array-axis-min arr 1 +inf.0) (list->array number? '(+inf.0 +inf.0 +inf.0)))
  (check-equal? (array-axis-max arr 1 -inf.0) (list->array number? '(-inf.0 -inf.0 -inf.0)))
  (check-equal? (array-axis-prod arr 1 1) (list->array number? '(1 1 1)))
  (check-exn exn? (λ () (array-sum arr)))
  (check-exn exn? (λ () (array-prod arr)))
  (check-exn exn? (λ () (array-min-value arr)))
  (check-exn exn? (λ () (array-max-value arr)))
  (check-equal? (array-sum arr 0) 0)
  (check-equal? (array-prod arr 1) 1)
  (check-equal? (array-min-value arr +inf.0) +inf.0)
  (check-equal? (array-max-value arr -inf.0) -inf.0))

(let ([arr  (make-array '() 0)])
  (check-exn exn? (λ () (array-axis-sum arr 0)))
  (check-exn exn? (λ () (array-axis-prod arr 0)))
  (check-exn exn? (λ () (array-axis-min arr 0)))
  (check-exn exn? (λ () (array-axis-max arr 0)))
  (check-equal? (array-sum arr) 0)
  (check-equal? (array-prod arr) 0)
  (check-equal? (array-min-value arr) 0)
  (check-equal? (array-max-value arr) 0))

(let ([arr  (list->array flonum? '((1.0 1.0 2.0 3.0) (0.0 -1.0 2.0 3.0)))])
  (check-equal? (array-axis-count arr 0 positive?) (list->array index? '[1 1 2 2]))
  (check-equal? (array-axis-count arr 1 positive?) (list->array index? '[4 2]))
  (check-equal? (array-count arr positive?) 6)
  (check-equal? (array-count (array-strict arr) positive?) 6))

(let ([arr  (list->array flonum? '((1.0 1.0 2.0 3.0) (0.0 -1.0 2.0 3.0)))])
  (check-equal? (array-axis-andmap arr 0 positive?) (list->array boolean? '[#f #f #t #t]))
  (check-equal? (array-axis-andmap arr 1 positive?) (list->array boolean? '[#t #f]))
  (check-equal? (array-andmap arr positive?) #f)
  (check-equal? (array-andmap (array-strict arr) positive?) #f))

(let ([arr  (list->array flonum? '((1.0 1.0 2.0 3.0) (2.0 3.0 2.0 3.0)))])
  (check-equal? (array-axis-andmap arr 0 positive?) (list->array boolean? '[#t #t #t #t]))
  (check-equal? (array-axis-andmap arr 1 positive?) (list->array boolean? '[#t #t]))
  (check-equal? (array-andmap arr positive?) #t)
  (check-equal? (array-andmap (array-strict arr) positive?) #t))

(let ([arr  (list->array flonum? '((-1.0 -1.0 -2.0 -3.0) (0.0 -1.0 2.0 3.0)))])
  (check-equal? (array-axis-ormap arr 0 positive?) (list->array boolean? '[#f #f #t #t]))
  (check-equal? (array-axis-ormap arr 1 positive?) (list->array boolean? '[#f #t]))
  (check-equal? (array-ormap arr positive?) #t)
  (check-equal? (array-ormap (array-strict arr) positive?) #t))

(let ([arr  (list->array flonum? '((-1.0 -1.0 -2.0 -3.0) (-2.0 -3.0 -2.0 -3.0)))])
  (check-equal? (array-axis-ormap arr 0 positive?) (list->array boolean? '[#f #f #f #f]))
  (check-equal? (array-axis-ormap arr 1 positive?) (list->array boolean? '[#f #f]))
  (check-equal? (array-ormap arr positive?) #f)
  (check-equal? (array-ormap (array-strict arr) positive?) #f))

(let ([arr  (make-array '() 0.0)])
  (check-equal? (array-count arr positive?) 0)
  (check-equal? (array-andmap arr positive?) #f)
  (check-equal? (array-ormap arr positive?) #f))

(let ([arr  (make-array '() 1.0)])
  (check-equal? (array-count arr positive?) 1)
  (check-equal? (array-andmap arr positive?) #t)
  (check-equal? (array-ormap arr positive?) #t))

(let ([arr  (make-array '(4 0) 0.0)])
  (check-equal? (array-axis-count arr 0 positive?)  (list->array index? '[]))
  (check-equal? (array-axis-andmap arr 0 positive?) (list->array boolean? '[]))
  (check-equal? (array-axis-ormap arr 0 positive?)  (list->array boolean? '[]))
  (check-equal? (array-axis-count arr 1 positive?)  (list->array index? '[0 0 0 0]))
  (check-equal? (array-axis-andmap arr 1 positive?) (list->array boolean? '[#t #t #t #t]))
  (check-equal? (array-axis-ormap arr 1 positive?)  (list->array boolean? '[#f #f #f #f]))
  (check-equal? (array-count arr positive?) 0)
  (check-equal? (array-andmap arr positive?) #t)
  (check-equal? (array-ormap arr positive?) #f))

;; ---------------------------------------------------------------------------------------------------
;; FFT

(check-exn exn? (λ () (array-fft (make-array '() 1))))
(check-exn exn? (λ () (array-fft (make-array '(0) 1))))
(check-exn exn? (λ () (array-fft (make-array '(3) 1))))

(let ([arr  (make-array '(4) 1)])
  (check array= (array-fft arr) (list->array number? '(4 0 0 0)))
  (check array= (array-inverse-fft (array-fft arr)) arr))

(let ([arr  (make-array '(2 2) 1)])
  (check array= (array-fft arr) (list->array number? '((4 0) (0 0))))
  (check array= (array-inverse-fft (array-fft arr)) arr))

;; ---------------------------------------------------------------------------------------------------
;; Unsafe ref

(let* ([l-arr  (make-array '() 0)]
       [s-arr  (array-strict l-arr)]
       [idxs  (vector)])
  (check-equal? (unsafe-array-ref l-arr (vector)) 0)
  (check-equal? (unsafe-array-ref s-arr (vector)) 0)
  (check-equal? (unsafe-array-ref l-arr #()) 0)
  (check-equal? (unsafe-array-ref s-arr #()) 0)
  (check-equal? (unsafe-array-ref l-arr idxs) 0)
  (check-equal? (unsafe-array-ref s-arr idxs) 0))

(let* ([l-arr  (indexes-array '(2))]
       [s-arr  (array-strict l-arr)]
       [idxs0  ((inst vector Index) 0)]
       [idxs1  ((inst vector Index) 1)])
  (check-equal? (unsafe-array-ref l-arr (vector 0)) '(0))
  (check-equal? (unsafe-array-ref s-arr (vector 0)) '(0))
  (check-equal? (unsafe-array-ref l-arr (vector 1)) '(1))
  (check-equal? (unsafe-array-ref s-arr (vector 1)) '(1))
  (check-equal? (unsafe-array-ref l-arr #(0)) '(0))
  (check-equal? (unsafe-array-ref s-arr #(0)) '(0))
  (check-equal? (unsafe-array-ref l-arr #(1)) '(1))
  (check-equal? (unsafe-array-ref s-arr #(1)) '(1))
  (check-equal? (unsafe-array-ref l-arr idxs0) '(0))
  (check-equal? (unsafe-array-ref s-arr idxs0) '(0))
  (check-equal? (unsafe-array-ref l-arr idxs1) '(1))
  (check-equal? (unsafe-array-ref s-arr idxs1) '(1)))

(let* ([l-arr  (indexes-array '(2 2))]
       [s-arr  (array-strict l-arr)]
       [idxs0  ((inst vector Index) 0 0)]
       [idxs1  ((inst vector Index) 1 1)])
  (check-equal? (unsafe-array-ref l-arr (vector 0 0)) '(0 0))
  (check-equal? (unsafe-array-ref s-arr (vector 0 0)) '(0 0))
  (check-equal? (unsafe-array-ref l-arr (vector 1 1)) '(1 1))
  (check-equal? (unsafe-array-ref s-arr (vector 1 1)) '(1 1))
  (check-equal? (unsafe-array-ref l-arr #(0 0)) '(0 0))
  (check-equal? (unsafe-array-ref s-arr #(0 0)) '(0 0))
  (check-equal? (unsafe-array-ref l-arr #(1 1)) '(1 1))
  (check-equal? (unsafe-array-ref s-arr #(1 1)) '(1 1))
  (check-equal? (unsafe-array-ref l-arr idxs0) '(0 0))
  (check-equal? (unsafe-array-ref s-arr idxs0) '(0 0))
  (check-equal? (unsafe-array-ref l-arr idxs1) '(1 1))
  (check-equal? (unsafe-array-ref s-arr idxs1) '(1 1)))

;; ---------------------------------------------------------------------------------------------------
;; Unsafe set!

(let* ([arr  ((inst array-strict Byte) (make-array '() 0))]
       [idxs  (vector)])
  (unsafe-array-set! arr (vector) 1)
  (check-equal? (unsafe-array-ref arr #()) 1)
  (unsafe-array-set! arr #() 2)
  (check-equal? (unsafe-array-ref arr #()) 2)
  (unsafe-array-set! arr idxs 3)
  (check-equal? (unsafe-array-ref arr #()) 3))

(let* ([arr  (array-strict (indexes-array '(2)))]
       [idxs0  ((inst vector Index) 0)]
       [idxs1  ((inst vector Index) 1)])
  (unsafe-array-set! arr (vector 0) '(2))
  (unsafe-array-set! arr (vector 1) '(3))
  (check-equal? (unsafe-array-ref arr #(0)) '(2))
  (check-equal? (unsafe-array-ref arr #(1)) '(3))
  (unsafe-array-set! arr #(0) '(4))
  (unsafe-array-set! arr #(1) '(5))
  (check-equal? (unsafe-array-ref arr #(0)) '(4))
  (check-equal? (unsafe-array-ref arr #(1)) '(5))
  (unsafe-array-set! arr idxs0 '(6))
  (unsafe-array-set! arr idxs1 '(7))
  (check-equal? (unsafe-array-ref arr #(0)) '(6))
  (check-equal? (unsafe-array-ref arr #(1)) '(7)))

(let* ([arr  (array-strict (indexes-array '(2 2)))]
       [idxs0  ((inst vector Index) 0 0)]
       [idxs1  ((inst vector Index) 1 1)])
  (unsafe-array-set! arr (vector 0 0) '(2 2))
  (unsafe-array-set! arr (vector 1 1) '(3 3))
  (check-equal? (unsafe-array-ref arr #(0 0)) '(2 2))
  (check-equal? (unsafe-array-ref arr #(1 1)) '(3 3))
  (unsafe-array-set! arr #(0 0) '(4 4))
  (unsafe-array-set! arr #(1 1) '(5 5))
  (check-equal? (unsafe-array-ref arr #(0 0)) '(4 4))
  (check-equal? (unsafe-array-ref arr #(1 1)) '(5 5))
  (unsafe-array-set! arr idxs0 '(6 6))
  (unsafe-array-set! arr idxs1 '(7 7))
  (check-equal? (unsafe-array-ref arr #(0 0)) '(6 6))
  (check-equal? (unsafe-array-ref arr #(1 1)) '(7 7)))

;; ---------------------------------------------------------------------------------------------------
;; Safe ref

(let* ([l-arr  (make-array '() 0)]
       [s-arr  (array-strict l-arr)]
       [idxs  '()]
       [bad-idxs  '(1)])
  (check-equal? (array-ref l-arr (list)) 0)
  (check-equal? (array-ref s-arr (list)) 0)
  (check-equal? (array-ref l-arr '()) 0)
  (check-equal? (array-ref s-arr '()) 0)
  (check-equal? (array-ref l-arr idxs) 0)
  (check-equal? (array-ref s-arr idxs) 0)
  (check-exn exn? (λ () (array-ref l-arr (list 1))))
  (check-exn exn? (λ () (array-ref s-arr (list 1))))
  (check-exn exn? (λ () (array-ref l-arr '(1))))
  (check-exn exn? (λ () (array-ref s-arr '(1))))
  (check-exn exn? (λ () (array-ref l-arr bad-idxs)))
  (check-exn exn? (λ () (array-ref s-arr bad-idxs))))

(let* ([l-arr  (indexes-array '(2))]
       [s-arr  (array-strict l-arr)]
       [idxs0  '(0)]
       [idxs1  '(1)]
       [bad-idxs0  '(0 0)]
       [bad-idxs1  '(-1)]
       [bad-idxs2  '( 2)])
  (check-equal? (array-ref l-arr (list 0)) '(0))
  (check-equal? (array-ref s-arr (list 0)) '(0))
  (check-equal? (array-ref l-arr (list 1)) '(1))
  (check-equal? (array-ref s-arr (list 1)) '(1))
  (check-equal? (array-ref l-arr '(0)) '(0))
  (check-equal? (array-ref s-arr '(0)) '(0))
  (check-equal? (array-ref l-arr '(1)) '(1))
  (check-equal? (array-ref s-arr '(1)) '(1))
  (check-equal? (array-ref l-arr idxs0) '(0))
  (check-equal? (array-ref s-arr idxs0) '(0))
  (check-equal? (array-ref l-arr idxs1) '(1))
  (check-equal? (array-ref s-arr idxs1) '(1))
  (check-exn exn? (λ () (array-ref l-arr (list 0 0))))
  (check-exn exn? (λ () (array-ref s-arr (list 0 0))))
  (check-exn exn? (λ () (array-ref l-arr (list -1))))
  (check-exn exn? (λ () (array-ref s-arr (list -1))))
  (check-exn exn? (λ () (array-ref l-arr (list 2))))
  (check-exn exn? (λ () (array-ref s-arr (list 2))))
  (check-exn exn? (λ () (array-ref l-arr '(0 0))))
  (check-exn exn? (λ () (array-ref s-arr '(0 0))))
  (check-exn exn? (λ () (array-ref l-arr '(-1))))
  (check-exn exn? (λ () (array-ref s-arr '(-1))))
  (check-exn exn? (λ () (array-ref l-arr '(2))))
  (check-exn exn? (λ () (array-ref s-arr '(2))))
  (check-exn exn? (λ () (array-ref l-arr bad-idxs0)))
  (check-exn exn? (λ () (array-ref s-arr bad-idxs0)))
  (check-exn exn? (λ () (array-ref l-arr bad-idxs1)))
  (check-exn exn? (λ () (array-ref s-arr bad-idxs1)))
  (check-exn exn? (λ () (array-ref l-arr bad-idxs2)))
  (check-exn exn? (λ () (array-ref s-arr bad-idxs2))))

(let* ([l-arr  (indexes-array '(2 2))]
       [s-arr  (array-strict l-arr)]
       [idxs0  '(0 0)]
       [idxs1  '(1 1)])
  (check-equal? (array-ref l-arr (list 0 0)) '(0 0))
  (check-equal? (array-ref s-arr (list 0 0)) '(0 0))
  (check-equal? (array-ref l-arr (list 1 1)) '(1 1))
  (check-equal? (array-ref s-arr (list 1 1)) '(1 1))
  (check-equal? (array-ref l-arr '(0 0)) '(0 0))
  (check-equal? (array-ref s-arr '(0 0)) '(0 0))
  (check-equal? (array-ref l-arr '(1 1)) '(1 1))
  (check-equal? (array-ref s-arr '(1 1)) '(1 1))
  (check-equal? (array-ref l-arr idxs0) '(0 0))
  (check-equal? (array-ref s-arr idxs0) '(0 0))
  (check-equal? (array-ref l-arr idxs1) '(1 1))
  (check-equal? (array-ref s-arr idxs1) '(1 1)))

;; ---------------------------------------------------------------------------------------------------
;; Safe set!

(let* ([arr  ((inst array-strict Byte) (make-array '() 0))]
       [idxs  '()]
       [bad-idxs '(1)])
  (array-set! arr (list) 1)
  (check-equal? (array-ref arr '()) 1)
  (array-set! arr '() 2)
  (check-equal? (array-ref arr '()) 2)
  (array-set! arr idxs 3)
  (check-equal? (array-ref arr '()) 3)
  (check-exn exn? (λ () (array-set! arr (list 1) 2)))
  (check-exn exn? (λ () (array-set! arr '(1) 2)))
  (check-exn exn? (λ () (array-set! arr bad-idxs 2))))

(let* ([arr  (array-strict (indexes-array '(2)))]
       [idxs0  '(0)]
       [idxs1  '(1)]
       [bad-idxs0  '(0 0)]
       [bad-idxs1  '(-1)]
       [bad-idxs2  '(2)])
  (array-set! arr (list 0) '(2))
  (array-set! arr (list 1) '(3))
  (check-equal? (array-ref arr '(0)) '(2))
  (check-equal? (array-ref arr '(1)) '(3))
  (array-set! arr '(0) '(4))
  (array-set! arr '(1) '(5))
  (check-equal? (array-ref arr '(0)) '(4))
  (check-equal? (array-ref arr '(1)) '(5))
  (array-set! arr idxs0 '(6))
  (array-set! arr idxs1 '(7))
  (check-equal? (array-ref arr '(0)) '(6))
  (check-equal? (array-ref arr '(1)) '(7))
  (check-exn exn? (λ () (array-set! arr (list 0 0) '(1))))
  (check-exn exn? (λ () (array-set! arr (list -1) '(1))))
  (check-exn exn? (λ () (array-set! arr (list 2) '(1))))
  (check-exn exn? (λ () (array-set! arr '(0 0) '(1))))
  (check-exn exn? (λ () (array-set! arr '(-1) '(1))))
  (check-exn exn? (λ () (array-set! arr '(2) '(1))))
  (check-exn exn? (λ () (array-set! arr bad-idxs0 '(1))))
  (check-exn exn? (λ () (array-set! arr bad-idxs1 '(1))))
  (check-exn exn? (λ () (array-set! arr bad-idxs2 '(1)))))

(let* ([arr  (array-strict (indexes-array '(2 2)))]
       [idxs0  '(0 0)]
       [idxs1  '(1 1)])
  (array-set! arr (list 0 0) '(2 2))
  (array-set! arr (list 1 1) '(3 3))
  (check-equal? (array-ref arr '(0 0)) '(2 2))
  (check-equal? (array-ref arr '(1 1)) '(3 3))
  (array-set! arr '(0 0) '(4 4))
  (array-set! arr '(1 1) '(5 5))
  (check-equal? (array-ref arr '(0 0)) '(4 4))
  (check-equal? (array-ref arr '(1 1)) '(5 5))
  (array-set! arr idxs0 '(6 6))
  (array-set! arr idxs1 '(7 7))
  (check-equal? (array-ref arr '(0 0)) '(6 6))
  (check-equal? (array-ref arr '(1 1)) '(7 7)))

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
                                       ((inst vector Index) 2 4)
                                       (λ: ([js : (Vectorof Index)])
                                         (match-define (vector j0 j1) js)
                                         ((inst vector Index) j1 j0))))
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
                                        (list (:: 0 4 2) (:: 2 -1 -2))))
              '[[(0 2) (0 0)]
                [(2 2) (2 0)]])

;; Permutation

(let ([arr  (make-array '() 0)])
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

;; Reshape, flatten

(let ([arr  (indexes-array '())])
  (check-exn exn? (λ () (array-reshape arr '(0))))
  (check-equal? (array->list (array-reshape arr '(1)))
                '[()])
  (check-equal? (array->list (array-reshape (array-strict arr) '(1)))
                '[()])
  (check-equal? (array->list (array-flatten arr))
                '[()])
  (check-equal? (array->list (array-flatten (array-strict arr)))
                '[()]))

(let ([arr  ((inst array-map (Listof Integer) Integer) first (indexes-array '(4)))])
  (check-exn exn? (λ () (array-reshape arr '())))
  (check-equal? (array->list (array-reshape arr '(2 2)))
                '[[0 1] [2 3]])
  (check-equal? (array->list (array-reshape (array-strict arr) '(2 2)))
                '[[0 1] [2 3]])
  (check-equal? (array->list (array-flatten arr))
                '[0 1 2 3])
  (check-equal? (array->list (array-flatten (array-strict arr)))
                '[0 1 2 3]))

;; Append

(let ([arr  (indexes-array '(3 2))]
      [brr  (indexes-array '(2 2))])
  (check-equal? (array->list (array-append arr 0 brr))
                '[[(0 0) (0 1)]
                  [(1 0) (1 1)]
                  [(2 0) (2 1)]
                  [(0 0) (0 1)]
                  [(1 0) (1 1)]])
  (check-exn exn? (λ () (array-append arr 1 brr)))
  (check-exn exn? (λ () (array-append arr 2 brr))))

(let ([arr  (indexes-array '(2 2))]
      [brr  (indexes-array '(2 3))])
  (check-equal? (array->list (array-append arr 1 brr))
                '[[(0 0) (0 1) (0 0) (0 1) (0 2)]
                  [(1 0) (1 1) (1 0) (1 1) (1 2)]])
  (check-exn exn? (λ () (array-append arr 0 brr)))
  (check-exn exn? (λ () (array-append arr 2 brr))))

(check-exn exn? (λ () (array-append (indexes-array '()) 0 (indexes-array '()))))
(check-exn exn? (λ () (array-append (indexes-array '(4)) 0 (indexes-array '(4 5)))))

(check-equal? (array->list (array-append (indexes-array '(4)) 0
                                         (indexes-array '(3))
                                         (indexes-array '(2))))
              '[(0) (1) (2) (3) (0) (1) (2) (0) (1)])
