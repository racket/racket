#lang typed/racket

(require racket/flonum
         math/array
         (rename-in
          (except-in typed/rackunit check-eq?)
          [check-equal?  old:check-equal?])
         math/private/array/utils)

(define-predicate listof-index? (Listof Index))
(define-predicate listof-flonum? (Listof Float))
(define-predicate undefined? Undefined)

;; This gets around the fact that typed/rackunit can no longer test higher-order values for equality,
;; since TR has firmed up its rules on passing `Any' types in and out of untyped code
(define-syntax-rule (check-equal? a b . message)
  (check-true (equal? a b) . message))

(define-syntax-rule (check-eq? a b . message)
  (check-equal? (eq-hash-code a) (eq-hash-code b) . message))

(define-syntax-rule (array-axis-andmap arr k pred?)
  (array-axis-and (array-map pred? arr) k))

(define-syntax-rule (array-axis-ormap arr k pred?)
  (array-axis-or (array-map pred? arr) k))

;; ---------------------------------------------------------------------------------------------------
;; array-mutable

(: make-indexes-vector (Integer Integer -> (Vectorof (Vectorof Integer))))
(define (make-indexes-vector n dims)
  (list->vector
   ((inst map (Vectorof Integer) (Listof Integer))
    list->vector
    (let: loop : (Listof (Listof Integer)) ([n : Integer  n])
      (cond [(= n 0)  (list null)]
            [else  (define lsts (loop (- n 1)))
                   (append* (for/list: : (Listof (Listof (Listof Integer))) ([k  (in-range dims)])
                              (map (λ: ([lst : (Listof Integer)]) (cons k lst)) lsts)))])))))

(check-equal? (make-indexes-vector 2 3)
              #(#(0 0) #(0 1) #(0 2) #(1 0) #(1 1) #(1 2) #(2 0) #(2 1) #(2 2)))

(let ([arr  (vector->array #(4) #(a b c d))])
  (check-eq? arr (array-strict arr)))

(let ([arr  (build-array #() (λ (js) 'foo))])
  (check-equal? (mutable-array-data (array->mutable-array arr))
                #(foo)))

(let ([arr  ((inst build-array Float) #(4) (λ (js) (exact->inexact (vector-ref js 0))))])
  (check-equal? (mutable-array-data (array->mutable-array arr))
                #(0.0 1.0 2.0 3.0)))

(let ([arr  ((inst build-array Float) #(0) (λ (js) (exact->inexact (vector-ref js 0))))])
  (check-equal? (mutable-array-data (array->mutable-array arr))
                #()))

(let ([arr  (build-array #(3 3) (λ (js) js))])
  (check-equal? (mutable-array-data (array->mutable-array arr))
                (make-indexes-vector 2 3)))

(let ([arr  (build-array #(2 2 2) (λ (js) js))])
  (check-equal? (mutable-array-data (array->mutable-array arr))
                (make-indexes-vector 3 2)))

(let ([arr  (build-array #(2 2 2 2) (λ (js) js))])
  (check-equal? (mutable-array-data (array->mutable-array arr))
                (make-indexes-vector 4 2)))

(let ([arr  (build-array #(2 2 2 2 2) (λ (js) js))])
  (check-equal? (mutable-array-data (array->mutable-array arr))
                (make-indexes-vector 5 2)))

(let ([arr  (build-array #(2 2 2 2 2 2) (λ (js) js))])
  (check-equal? (mutable-array-data (array->mutable-array arr))
                (make-indexes-vector 6 2)))

;; ---------------------------------------------------------------------------------------------------
;; list*->array

(check-equal? (list*->array 1.0 flonum?)
              (vector->array #() #(1.0)))

(check-equal? (list*->array '[] flonum?)
              (vector->array #(0) #()))

(check-equal? (list*->array '[[]] flonum?)
              (vector->array #(1 0) #()))

(check-equal? (list*->array '[1.0] flonum?)
              (vector->array #(1) #(1.0)))

(check-equal? (list*->array '[[1.0]] flonum?)
              (vector->array #(1 1) #(1.0)))

(check-equal? (list*->array '[[[1.0]]] flonum?)
              (vector->array #(1 1 1) #(1.0)))

(check-equal? (list*->array '() listof-flonum?)
              (vector->array #() #(())))

(check-equal? (list*->array '[()] listof-flonum?)
              (vector->array #(1) #(())))

(check-equal? (list*->array '[(1.0) (2.0)] listof-flonum?)
              (vector->array #(2) #((1.0) (2.0))))

(check-equal? (list*->array '[((1.0)) ((2.0))] listof-flonum?)
              (vector->array #(2 1) #((1.0) (2.0))))

;; ---------------------------------------------------------------------------------------------------
;; array->list*

(let ([arr  (build-array #() (λ (js) 'foo))])
  (check-equal? (array->list* arr) 'foo)
  (check-equal? arr (list*->array (array->list* arr) symbol?)))

(let ([arr  ((inst build-array Float) #(4) (λ (js) (exact->inexact (vector-ref js 0))))])
  (check-equal? (array->list* arr) '[0.0 1.0 2.0 3.0])
  (check-equal? arr (list*->array (array->list* arr) flonum?)))

(let ([arr  (build-array #(3 3) (inst vector->list Index))])
  (check-equal? (array->list* arr) '[[(0 0) (0 1) (0 2)]
                                     [(1 0) (1 1) (1 2)]
                                     [(2 0) (2 1) (2 2)]])
  (check-equal? arr (list*->array (array->list* arr) listof-index?)))

(let ([arr  (build-array #(2 2 2) (inst vector->list Index))])
  (check-equal? (array->list* arr)
                '[[[(0 0 0) (0 0 1)] [(0 1 0) (0 1 1)]]
                  [[(1 0 0) (1 0 1)] [(1 1 0) (1 1 1)]]])
  (check-equal? arr (list*->array (array->list* arr) listof-index?)))

(let ([arr  (build-array #(2 2 2 2) (inst vector->list Index))])
  (check-equal? (array->list* arr)
                '[[[[(0 0 0 0) (0 0 0 1)] [(0 0 1 0) (0 0 1 1)]]
                   [[(0 1 0 0) (0 1 0 1)] [(0 1 1 0) (0 1 1 1)]]]
                  [[[(1 0 0 0) (1 0 0 1)] [(1 0 1 0) (1 0 1 1)]]
                   [[(1 1 0 0) (1 1 0 1)] [(1 1 1 0) (1 1 1 1)]]]])
  (check-equal? arr (list*->array (array->list* arr) listof-index?)))

(check-equal? (array->list* (list*->array '[1.0 2.0] flonum?))
              '[1.0 2.0])
(check-equal? (array->list* (list*->array '[[1.0 2.0] [3.0 4.0]] flonum?))
              '[[1.0 2.0] [3.0 4.0]])

;; ---------------------------------------------------------------------------------------------------
;; vector*->array

(check-equal? (vector*->array 1.0 flonum?)
              (vector->array #() #(1.0)))

(check-equal? ((inst vector*->array Float) #() flonum?)
              (vector->array #(0) #()))

(check-equal? ((inst vector*->array Float) #(#()) flonum?)
              (vector->array #(1 0) #()))

(check-equal? ((inst vector*->array Float) #(1.0) flonum?)
              (vector->array #(1) #(1.0)))

(check-equal? ((inst vector*->array Float) #(#(1.0)) flonum?)
              (vector->array #(1 1) #(1.0)))

(check-equal? ((inst vector*->array Float) #(#(#(1.0))) flonum?)
              (vector->array #(1 1 1) #(1.0)))

;; ---------------------------------------------------------------------------------------------------
;; array->vector

(let ([arr : (Array Symbol) (build-array #() (λ (js) 'foo))])
  (check-equal? (array->vector* arr) 'foo)
  (check-equal? arr (vector*->array (array->vector* arr) symbol?)))

(let ([arr  ((inst build-array Float) #(4) (λ (js) (exact->inexact (vector-ref js 0))))])
  (check-equal? (array->vector* arr) #(0.0 1.0 2.0 3.0))
  (check-equal? arr (vector*->array (array->vector* arr) flonum?)))

(let ([arr  (build-array #(3 3) (inst vector->list Index))])
  (check-equal? (array->vector* arr) #[#[(0 0) (0 1) (0 2)]
                                       #[(1 0) (1 1) (1 2)]
                                       #[(2 0) (2 1) (2 2)]])
  (check-equal? arr (vector*->array (array->vector* arr) listof-index?)))

(let ([arr  (build-array #(2 2 2) (inst vector->list Index))])
  (check-equal? (array->vector* arr)
                #[#[#[(0 0 0) (0 0 1)] #[(0 1 0) (0 1 1)]]
                  #[#[(1 0 0) (1 0 1)] #[(1 1 0) (1 1 1)]]])
  (check-equal? arr (vector*->array (array->vector* arr) listof-index?)))

(let ([arr  (build-array #(2 2 2 2) (inst vector->list Index))])
  (check-equal? (array->vector* arr)
                #[#[#[#[(0 0 0 0) (0 0 0 1)] #[(0 0 1 0) (0 0 1 1)]]
                    #[#[(0 1 0 0) (0 1 0 1)] #[(0 1 1 0) (0 1 1 1)]]]
                  #[#[#[(1 0 0 0) (1 0 0 1)] #[(1 0 1 0) (1 0 1 1)]]
                    #[#[(1 1 0 0) (1 1 0 1)] #[(1 1 1 0) (1 1 1 1)]]]])
  (check-equal? arr (vector*->array (array->vector* arr) listof-index?)))

(check-equal? (array->vector* ((inst vector*->array Flonum) #[1.0 2.0] flonum?))
              #[1.0 2.0])
(check-equal? (array->vector* ((inst vector*->array Flonum) #[#[1.0 2.0] #[3.0 4.0]] flonum?))
              #[#[1.0 2.0] #[3.0 4.0]])

;; ---------------------------------------------------------------------------------------------------
;; Array syntax

(check-equal? (array 1.0)
              (list*->array 1.0 flonum?))

(check-equal? (array #[])
              (list*->array '[] flonum?))

(check-equal? (array #[#[]])
              (list*->array '[[]] flonum?))

(check-equal? (array #[1.0])
              (list*->array '[1.0] flonum?))

(check-equal? (array #[#[1.0]])
              (list*->array '[[1.0]] flonum?))

(check-equal? (array #[#[#[1.0]]])
              (list*->array '[[[1.0]]] flonum?))

(check-equal? (mutable-array 1.0)
              (list*->array 1.0 flonum?))

(check-equal? (mutable-array #[])
              (list*->array '[] flonum?))

(check-equal? (mutable-array #[#[]])
              (list*->array '[[]] flonum?))

(check-equal? (mutable-array #[1.0])
              (list*->array '[1.0] flonum?))

(check-equal? (mutable-array #[#[1.0]])
              (list*->array '[[1.0]] flonum?))

(check-equal? (mutable-array #[#[#[1.0]]])
              (list*->array '[[[1.0]]] flonum?))

;; ---------------------------------------------------------------------------------------------------
;; Other constructors

(check-equal? (make-array #(3 3) 0)
              (build-array #(3 3) (λ (js) 0)))

(check-equal? (axis-index-array #(3 3) 0)
              (build-array #(3 3) (λ: ([js : Indexes]) (vector-ref js 0))))

(check-exn exn? (λ () (axis-index-array #(3 3) -1)))
(check-exn exn? (λ () (axis-index-array #() 0)))

(check-equal? (index-array #()) (array 0))
(check-equal? (index-array #(4)) (array #[0 1 2 3]))
(check-equal? (index-array #(2 2)) (array #[#[0 1] #[2 3]]))

(check-equal? (indexes-array #(3 3))
              (build-array #(3 3) (λ: ([js : Indexes]) (vector-copy js))))

(check-equal? (diagonal-array 0 0 1.0 0.0)
              (array 1.0))

(check-equal? (diagonal-array 1 0 1.0 0.0)
              (array #[]))

(check-equal? (diagonal-array 1 1 1.0 0.0)
              (array #[1.0]))

(check-equal? (diagonal-array 2 1 1.0 0.0)
              (array #[#[1.0]]))

(check-equal? (diagonal-array 2 2 1.0 0.0)
              (array #[#[1.0 0.0] #[0.0 1.0]]))

(check-equal? (diagonal-array 3 2 1.0 0.0)
              (array #[#[#[1.0 0.0] #[0.0 0.0]]
                       #[#[0.0 0.0] #[0.0 1.0]]]))

;; ---------------------------------------------------------------------------------------------------
;; Pointwise
;; Not much to test here because most pointwise ops are defined by the same two lifts

(check-equal? (array-sqrt (array #[1.0 4.0 9.0 16.0]))
              (array #[1.0 2.0 3.0 4.0]))

(check-equal? (array+ (array #[1.0 2.0])
                      (array #[10.0 20.0]))
              (array #[11.0 22.0]))

(check-equal? (array-map inexact->exact (array #[1.0 2.0]))
              (array #[1 2]))

(check-equal? (array-map (inst cons Float Float)
                         (array #[1.0 2.0])
                         (array #[10.0 20.0]))
              (vector->array #(2) #[(1.0 . 10.0) (2.0 . 20.0)]))

;; ---------------------------------------------------------------------------------------------------
;; Fold

(let ([arr  (array #[#[ 1.0  4.0  9.0  16.0]
                     #[-1.0 -4.0 -9.0 -16.0]])])
  (check-equal? (array-axis-sum arr 0) (array #[0.0 0.0 0.0 0.0]))
  (check-equal? (array-axis-prod arr 0) (array #[-1.0 -16.0 -81.0 -256.0]))
  (check-equal? (array-axis-min arr 0) (array #[-1.0 -4.0 -9.0 -16.0]))
  (check-equal? (array-axis-max arr 0) (array #[ 1.0  4.0  9.0  16.0]))
  (check-equal? (array-axis-fold arr 0 (inst cons Float (Listof Float)) null)
                (list*->array '[[-1.0 1.0] [-4.0 4.0] [-9.0 9.0] [-16.0 16.0]]
                             listof-flonum?))
  (check-equal? (array-axis-sum arr 1) (array #[30.0 -30.0]))
  (check-equal? (array-axis-prod arr 1) (array #[576.0 576.0]))
  (check-equal? (array-axis-min arr 1) (array #[1.0 -16.0]))
  (check-equal? (array-axis-max arr 1) (array #[16.0 -1.0]))
  (check-equal? (array-axis-fold arr 1 (inst cons Float (Listof Float)) null)
                (list*->array '[[ 16.0  9.0  4.0  1.0]
                               [-16.0 -9.0 -4.0 -1.0]]
                             listof-flonum?))
  (check-equal? (array-all-sum arr) 0.0)
  (check-equal? (array-all-prod arr) (* 576.0 576.0))
  (check-equal? (array-all-min arr) -16.0)
  (check-equal? (array-all-max arr)  16.0))

(let ([arr  (make-array #(3 0) 0)])
  (check-equal? (array-axis-sum arr 0) (array #[]))
  (check-equal? (array-axis-prod arr 0) (array #[]))
  (check-equal? (array-axis-min arr 0) (array #[]))
  (check-equal? (array-axis-max arr 0) (array #[]))
  (check-exn exn? (λ () (array-axis-sum arr 1)))
  (check-exn exn? (λ () (array-axis-prod arr 1)))
  (check-exn exn? (λ () (array-axis-min arr 1)))
  (check-exn exn? (λ () (array-axis-max arr 1)))
  (check-equal? (array-axis-sum arr 1 0) (array #[0 0 0]))
  (check-equal? (array-axis-min arr 1 +inf.0) (array #[+inf.0 +inf.0 +inf.0]))
  (check-equal? (array-axis-max arr 1 -inf.0) (array #[-inf.0 -inf.0 -inf.0]))
  (check-equal? (array-axis-prod arr 1 1) (array #[1 1 1]))
  (check-exn exn? (λ () (array-all-sum arr)))
  (check-exn exn? (λ () (array-all-prod arr)))
  (check-exn exn? (λ () (array-all-min arr)))
  (check-exn exn? (λ () (array-all-max arr)))
  (check-equal? (array-all-sum arr 0) 0)
  (check-equal? (array-all-prod arr 1) 1)
  (check-equal? (array-all-min arr +inf.0) +inf.0)
  (check-equal? (array-all-max arr -inf.0) -inf.0))

(let ([arr  (make-array #() 0)])
  (check-exn exn? (λ () (array-axis-sum arr 0)))
  (check-exn exn? (λ () (array-axis-prod arr 0)))
  (check-exn exn? (λ () (array-axis-min arr 0)))
  (check-exn exn? (λ () (array-axis-max arr 0)))
  (check-equal? (array-all-sum arr) 0)
  (check-equal? (array-all-prod arr) 0)
  (check-equal? (array-all-min arr) 0)
  (check-equal? (array-all-max arr) 0))

(let ([arr  (array #[#[1.0 1.0 2.0 3.0] #[0.0 -1.0 2.0 3.0]])])
  (check-equal? (array-axis-count arr 0 positive?) (array #[1 1 2 2]))
  (check-equal? (array-axis-count arr 1 positive?) (array #[4 2]))
  (check-equal? (array-count positive? arr) 6))

(let ([arr  (array #[#[1.0 1.0 2.0 3.0] #[0.0 -1.0 2.0 3.0]])])
  (check-equal? (array-axis-andmap arr 0 positive?) (array #[#f #f #t #t]))
  (check-equal? (array-axis-andmap arr 1 positive?) (array #[#t #f]))
  (check-equal? (array-andmap positive? arr) #f))

(let ([arr  (array #[#[1.0 1.0 2.0 3.0] #[2.0 3.0 2.0 3.0]])])
  (check-equal? (array-axis-andmap arr 0 positive?) (array #[#t #t #t #t]))
  (check-equal? (array-axis-andmap arr 1 positive?) (array #[#t #t]))
  (check-equal? (array-andmap positive? arr) #t))

(let ([arr  (array #[#[-1.0 -1.0 -2.0 -3.0] #[0.0 -1.0 2.0 3.0]])])
  (check-equal? (array-axis-ormap arr 0 positive?) (array #[#f #f #t #t]))
  (check-equal? (array-axis-ormap arr 1 positive?) (array #[#f #t]))
  (check-equal? (array-ormap positive? arr) #t))

(let ([arr  (array #[#[-1.0 -1.0 -2.0 -3.0] #[-2.0 -3.0 -2.0 -3.0]])])
  (check-equal? (array-axis-ormap arr 0 positive?) (array #[#f #f #f #f]))
  (check-equal? (array-axis-ormap arr 1 positive?) (array #[#f #f]))
  (check-equal? (array-ormap positive? arr) #f))

(let ([arr  (make-array #() 0.0)])
  (check-equal? (array-count positive? arr) 0)
  (check-equal? (array-andmap positive? arr) #f)
  (check-equal? (array-ormap positive? arr) #f))

(let ([arr  (make-array #() 1.0)])
  (check-equal? (array-count positive? arr) 1)
  (check-equal? (array-andmap positive? arr) #t)
  (check-equal? (array-ormap positive? arr) #t))

(let ([arr  (make-array #(4 0) 0.0)])
  (check-equal? (array-axis-count arr 0 positive?)  (array #[]))
  (check-equal? (array-axis-andmap arr 0 positive?) (array #[]))
  (check-equal? (array-axis-ormap arr 0 positive?)  (array #[]))
  (check-equal? (array-axis-count arr 1 positive?)  (array #[0 0 0 0]))
  (check-equal? (array-axis-andmap arr 1 positive?) (array #[#t #t #t #t]))
  (check-equal? (array-axis-ormap arr 1 positive?)  (array #[#f #f #f #f]))
  (check-equal? (array-count positive? arr) 0)
  (check-equal? (array-andmap positive? arr) #t)
  (check-equal? (array-ormap positive? arr) #f))

;; ---------------------------------------------------------------------------------------------------
;; FFT

(check-exn exn? (λ () (array-fft (make-array #() 1))))
(check-exn exn? (λ () (array-fft (make-array #(0) 1))))
(check-exn exn? (λ () (array-fft (make-array #(3) 1))))

(let ([arr  (make-array #(4) 1)])
  (check-true (array-all-and (array= (array-fft arr) (array #[4 0 0 0]))))
  (check-true (array-all-and (array= (array-inverse-fft (array-fft arr)) arr))))

(let ([arr  (make-array #(2 2) 1)])
  (check-true (array-all-and (array= (array-fft arr) (array #[#[4 0] #[0 0]]))))
  (check-true (array-all-and (array= (array-inverse-fft (array-fft arr)) arr))))

;; ---------------------------------------------------------------------------------------------------
;; Unsafe ref

(check-equal? (unsafe-array-ref (make-array #() 0) (vector)) 0)

(let* ([arr  (indexes-array #(2))])
  (check-equal? (unsafe-array-ref arr ((inst vector Index) 0)) #(0))
  (check-equal? (unsafe-array-ref arr ((inst vector Index) 1)) #(1)))

(let* ([arr  (indexes-array #(2 2))])
  (check-equal? (unsafe-array-ref arr ((inst vector Index) 0 0)) #(0 0))
  (check-equal? (unsafe-array-ref arr ((inst vector Index) 1 1)) #(1 1)))

;; ---------------------------------------------------------------------------------------------------
;; Unsafe set!

(let* ([arr  ((inst array->mutable-array Byte) (make-array #() 0))]
       [idxs  (vector)])
  (unsafe-array-set! arr (vector) 1)
  (check-equal? (unsafe-array-ref arr #()) 1)
  (unsafe-array-set! arr #() 2)
  (check-equal? (unsafe-array-ref arr #()) 2)
  (unsafe-array-set! arr idxs 3)
  (check-equal? (unsafe-array-ref arr #()) 3))

(let* ([arr  (array->mutable-array (indexes-array #(2)))]
       [idxs0  ((inst vector Index) 0)]
       [idxs1  ((inst vector Index) 1)])
  (unsafe-array-set! arr idxs0 ((inst vector Index) 6))
  (unsafe-array-set! arr idxs1 ((inst vector Index) 7))
  (check-equal? (unsafe-array-ref arr idxs0) #(6))
  (check-equal? (unsafe-array-ref arr idxs1) #(7)))

(let* ([arr  (array->mutable-array (indexes-array #(2 2)))]
       [idxs0  ((inst vector Index) 0 0)]
       [idxs1  ((inst vector Index) 1 1)])
  (unsafe-array-set! arr idxs0 ((inst vector Index) 6 6))
  (unsafe-array-set! arr idxs1 ((inst vector Index) 7 7))
  (check-equal? (unsafe-array-ref arr idxs0) #(6 6))
  (check-equal? (unsafe-array-ref arr idxs1) #(7 7)))

;; ---------------------------------------------------------------------------------------------------
;; Safe ref

(let* ([l-arr  (make-array #() 0)]
       [s-arr  (array->mutable-array l-arr)]
       [idxs  #()]
       [bad-idxs  #(1)])
  (check-equal? (array-ref l-arr (vector)) 0)
  (check-equal? (array-ref s-arr (vector)) 0)
  (check-equal? (array-ref l-arr #()) 0)
  (check-equal? (array-ref s-arr #()) 0)
  (check-equal? (array-ref l-arr idxs) 0)
  (check-equal? (array-ref s-arr idxs) 0)
  (check-exn exn? (λ () (array-ref l-arr (vector 1))))
  (check-exn exn? (λ () (array-ref s-arr (vector 1))))
  (check-exn exn? (λ () (array-ref l-arr #(1))))
  (check-exn exn? (λ () (array-ref s-arr #(1))))
  (check-exn exn? (λ () (array-ref l-arr bad-idxs)))
  (check-exn exn? (λ () (array-ref s-arr bad-idxs))))

(let* ([l-arr  (indexes-array #(2))]
       [s-arr  (array->mutable-array l-arr)]
       [idxs0  #(0)]
       [idxs1  #(1)]
       [bad-idxs0  #(0 0)]
       [bad-idxs1  #(-1)]
       [bad-idxs2  #( 2)])
  (check-equal? (array-ref l-arr (vector 0)) #(0))
  (check-equal? (array-ref s-arr (vector 0)) #(0))
  (check-equal? (array-ref l-arr (vector 1)) #(1))
  (check-equal? (array-ref s-arr (vector 1)) #(1))
  (check-equal? (array-ref l-arr #(0)) #(0))
  (check-equal? (array-ref s-arr #(0)) #(0))
  (check-equal? (array-ref l-arr #(1)) #(1))
  (check-equal? (array-ref s-arr #(1)) #(1))
  (check-equal? (array-ref l-arr idxs0) #(0))
  (check-equal? (array-ref s-arr idxs0) #(0))
  (check-equal? (array-ref l-arr idxs1) #(1))
  (check-equal? (array-ref s-arr idxs1) #(1))
  (check-exn exn? (λ () (array-ref l-arr (vector 0 0))))
  (check-exn exn? (λ () (array-ref s-arr (vector 0 0))))
  (check-exn exn? (λ () (array-ref l-arr (vector -1))))
  (check-exn exn? (λ () (array-ref s-arr (vector -1))))
  (check-exn exn? (λ () (array-ref l-arr (vector 2))))
  (check-exn exn? (λ () (array-ref s-arr (vector 2))))
  (check-exn exn? (λ () (array-ref l-arr #(0 0))))
  (check-exn exn? (λ () (array-ref s-arr #(0 0))))
  (check-exn exn? (λ () (array-ref l-arr #(-1))))
  (check-exn exn? (λ () (array-ref s-arr #(-1))))
  (check-exn exn? (λ () (array-ref l-arr #(2))))
  (check-exn exn? (λ () (array-ref s-arr #(2))))
  (check-exn exn? (λ () (array-ref l-arr bad-idxs0)))
  (check-exn exn? (λ () (array-ref s-arr bad-idxs0)))
  (check-exn exn? (λ () (array-ref l-arr bad-idxs1)))
  (check-exn exn? (λ () (array-ref s-arr bad-idxs1)))
  (check-exn exn? (λ () (array-ref l-arr bad-idxs2)))
  (check-exn exn? (λ () (array-ref s-arr bad-idxs2))))

(let* ([l-arr  (indexes-array #(2 2))]
       [s-arr  (array->mutable-array l-arr)]
       [idxs0  #(0 0)]
       [idxs1  #(1 1)])
  (check-equal? (array-ref l-arr (vector 0 0)) #(0 0))
  (check-equal? (array-ref s-arr (vector 0 0)) #(0 0))
  (check-equal? (array-ref l-arr (vector 1 1)) #(1 1))
  (check-equal? (array-ref s-arr (vector 1 1)) #(1 1))
  (check-equal? (array-ref l-arr #(0 0)) #(0 0))
  (check-equal? (array-ref s-arr #(0 0)) #(0 0))
  (check-equal? (array-ref l-arr #(1 1)) #(1 1))
  (check-equal? (array-ref s-arr #(1 1)) #(1 1))
  (check-equal? (array-ref l-arr idxs0) #(0 0))
  (check-equal? (array-ref s-arr idxs0) #(0 0))
  (check-equal? (array-ref l-arr idxs1) #(1 1))
  (check-equal? (array-ref s-arr idxs1) #(1 1)))

;; ---------------------------------------------------------------------------------------------------
;; Safe set!

(let* ([arr  ((inst array->mutable-array Byte) (make-array #() 0))]
       [idxs  #()]
       [bad-idxs #(1)])
  (array-set! arr (vector) 1)
  (check-equal? (array-ref arr #()) 1)
  (array-set! arr #() 2)
  (check-equal? (array-ref arr #()) 2)
  (array-set! arr idxs 3)
  (check-equal? (array-ref arr #()) 3)
  (check-exn exn? (λ () (array-set! arr (vector 1) 2)))
  (check-exn exn? (λ () (array-set! arr #(1) 2)))
  (check-exn exn? (λ () (array-set! arr bad-idxs 2))))

(let* ([arr  (array->mutable-array (indexes-array #(2)))]
       [idxs0  #(0)]
       [idxs1  #(1)]
       [bad-idxs0  #(0 0)]
       [bad-idxs1  #(-1)]
       [bad-idxs2  #(2)]
       [one  ((inst vector Index) 1)])
  (array-set! arr (vector 0) ((inst vector Index) 2))
  (array-set! arr (vector 1) ((inst vector Index) 3))
  (check-equal? (array-ref arr #(0)) #(2))
  (check-equal? (array-ref arr #(1)) #(3))
  (array-set! arr #(0) ((inst vector Index) 4))
  (array-set! arr #(1) ((inst vector Index) 5))
  (check-equal? (array-ref arr #(0)) #(4))
  (check-equal? (array-ref arr #(1)) #(5))
  (array-set! arr idxs0 ((inst vector Index) 6))
  (array-set! arr idxs1 ((inst vector Index) 7))
  (check-equal? (array-ref arr #(0)) #(6))
  (check-equal? (array-ref arr #(1)) #(7))
  (check-exn exn? (λ () (array-set! arr (vector 0 0) one)))
  (check-exn exn? (λ () (array-set! arr (vector -1) one)))
  (check-exn exn? (λ () (array-set! arr (vector 2) one)))
  (check-exn exn? (λ () (array-set! arr #(0 0) one)))
  (check-exn exn? (λ () (array-set! arr #(-1) one)))
  (check-exn exn? (λ () (array-set! arr #(2) one)))
  (check-exn exn? (λ () (array-set! arr bad-idxs0 one)))
  (check-exn exn? (λ () (array-set! arr bad-idxs1 one)))
  (check-exn exn? (λ () (array-set! arr bad-idxs2 one))))

(let* ([arr  (array->mutable-array (indexes-array #(2 2)))]
       [idxs0  #(0 0)]
       [idxs1  #(1 1)])
  (array-set! arr (vector 0 0) ((inst vector Index) 2 2))
  (array-set! arr (vector 1 1) ((inst vector Index) 3 3))
  (check-equal? (array-ref arr #(0 0)) #(2 2))
  (check-equal? (array-ref arr #(1 1)) #(3 3))
  (array-set! arr #(0 0) ((inst vector Index) 4 4))
  (array-set! arr #(1 1) ((inst vector Index) 5 5))
  (check-equal? (array-ref arr #(0 0)) #(4 4))
  (check-equal? (array-ref arr #(1 1)) #(5 5))
  (array-set! arr idxs0 ((inst vector Index) 6 6))
  (array-set! arr idxs1 ((inst vector Index) 7 7))
  (check-equal? (array-ref arr #(0 0)) #(6 6))
  (check-equal? (array-ref arr #(1 1)) #(7 7)))

;; ---------------------------------------------------------------------------------------------------
;; Indexing

(let ([arr  (mutable-array #[#[0 1 2 3 4 5]
                             #[1 2 3 4 5 6]
                             #[2 3 4 5 6 7]
                             #[3 4 5 6 7 8]]
                           : (U Integer Symbol))]
      [idxs  (array #['#(0 0) '#(1 1) '#(1 2) '#(2 3) '#(3 4) '#(3 5)])]
      [vals  (array #['a 'b 'c 'd 'e 'f])]
      [slices  (list (:: 0 4 2) (:: 2 -1 -2))])
  (check-equal? (array-indexes-ref arr idxs)
                (array #[0 2 3 5 7 8]))
  (check-equal? (array-indexes-ref arr (array-slice-ref (indexes-array (array-shape arr)) slices))
                (array-slice-ref arr slices))
  (array-indexes-set! arr idxs vals)
  (check-equal? arr (array #[#['a 1 2 3 4 5]
                             #[1 'b 'c 4 5 6]
                             #[2 3 4 'd 6 7]
                             #[3 4 5 6 'e 'f]])))

(check-equal? (array-slice-ref (indexes-array #()) (list (::new 2)))
              (make-array #(2) #()))

(let ([arr  (indexes-array #(2))])
  (check-equal? (array-slice-ref arr (list (::) (::new 2)))
                (array #[#['#(0) '#(0)] #['#(1) '#(1)]]))
  (check-equal? (array-slice-ref arr (list (::new 2) (::)))
                (array #[#['#(0) '#(1)] #['#(0) '#(1)]]))
  (check-equal? (array-slice-ref arr (list (::)))
                arr)
  (check-equal? (array-slice-ref arr (list ::...))
                arr))

(let ([arr  (index-array #(10))])
  (check-equal? (array-slice-ref arr (list (:: 0 10 2)))
                (array #[0 2 4 6 8]))
  (check-equal? (array-slice-ref arr (list (:: #f 10 2)))
                (array #[0 2 4 6 8]))
  (check-equal? (array-slice-ref arr (list (:: 0 #f 2)))
                (array #[0 2 4 6 8]))
  (check-equal? (array-slice-ref arr (list (:: #f #f 2)))
                (array #[0 2 4 6 8]))
  (check-equal? (array-slice-ref arr (list (:: 9 -1 -2)))
                (array #[9 7 5 3 1]))
  (check-equal? (array-slice-ref arr (list (:: 9 #f -2)))
                (array #[9 7 5 3 1]))
  (check-equal? (array-slice-ref arr (list (:: #f -1 -2)))
                (array #[9 7 5 3 1]))
  (check-equal? (array-slice-ref arr (list (:: #f #f -2)))
                (array #[9 7 5 3 1]))
  (check-equal? (array-slice-ref arr (list 4))
                (array 4))
  (check-exn exn? (λ () (array-slice-ref arr (list -1))))
  (check-exn exn? (λ () (array-slice-ref arr (list 10))))
  (check-exn exn? (λ () (array-slice-ref arr (list (:: 0 11 2)))))
  (check-exn exn? (λ () (array-slice-ref arr (list (:: -1 10 2)))))
  (check-exn exn? (λ () (array-slice-ref arr (list (:: 10 -1 -2)))))
  (check-exn exn? (λ () (array-slice-ref arr (list (:: 9 -2 -2))))))

(let ([arr  (index-array #(4 4))])
  (check-equal? (array-slice-ref arr (list ::...))
                arr)
  (check-equal? (array-slice-ref arr (list (:: 0 4 2) (:: 0 4 2)))
                (array #[#[0 2] #[8 10]]))
  (check-equal? (array-slice-ref arr (list (:: 0 4 2) ::...))
                (array #[#[0 1 2 3] #[8 9 10 11]]))
  (check-equal? (array-slice-ref arr (list ::... (:: 0 4 2)))
                (array #[#[0 2] #[4 6] #[8 10] #[12 14]]))
  (check-equal? (array-slice-ref arr (list (:: 0 4 2) 0))
                (array #[0 8]))
  (check-equal? (array-slice-ref arr (list 0 (:: 0 4 2)))
                (array #[0 2]))
  (check-equal? (array-slice-ref arr (list (::new 0) ::...))
                (make-array #(0 4 4) 0))
  (check-equal? (array-slice-ref arr (list (::new 1) ::...))
                (array-axis-insert arr 0 1))
  (check-equal? (array-slice-ref arr (list (::) (::new 1) (::)))
                (array-axis-insert arr 1 1))
  (check-equal? (array-slice-ref arr (list (::) (::) (::new 1)))
                (array-axis-insert arr 2 1)))

;; ---------------------------------------------------------------------------------------------------
;; Transforms

(check-equal? (array-transform (indexes-array #(4 2))
                               #(2 4) (λ: ([js : (Vectorof Index)])
                                        (match-define (vector j0 j1) js)
                                        (vector j1 j0)))
              (array #[#['#(0 0) '#(1 0) '#(2 0) '#(3 0)]
                       #['#(0 1) '#(1 1) '#(2 1) '#(3 1)]]))

(check-exn exn? (λ () (array-strict (array-transform (indexes-array #(2 2)) #(3 3) identity))))
(check-exn exn? (λ () (array-strict (array-transform (indexes-array #(2 2)) #(2) identity))))

(check-equal? (unsafe-array-transform (indexes-array #(4 2))
                                      ((inst vector Index) 2 4)
                                      (λ: ([js : (Vectorof Index)])
                                        (match-define (vector j0 j1) js)
                                        ((inst vector Index) j1 j0)))
              (array #[#['#(0 0) '#(1 0) '#(2 0) '#(3 0)]
                       #['#(0 1) '#(1 1) '#(2 1) '#(3 1)]]))

;; Permutation

(let ([arr  (make-array #() 0)])
  (check-equal? (array-axis-permute arr '())
                (array 0))
  (check-exn exn? (λ () (array-axis-permute arr '(0)))))

(let ([arr  (indexes-array #(4))])
  (check-exn exn? (λ () (array-axis-permute arr '())))
  (check-equal? (array-axis-permute arr '(0)) arr)
  (check-exn exn? (λ () (array-axis-permute arr '(1)))))

(let ([arr  (indexes-array #(2 2))])
  (check-exn exn? (λ () (array-axis-permute arr '())))
  (check-equal? (array-axis-permute arr '(0 1)) arr)
  (check-equal? (array-axis-permute arr '(1 0))
                (array #[#['#(0 0) '#(1 0)]
                         #['#(0 1) '#(1 1)]])))

;; Transposition

(let ([arr  (indexes-array #(4))])
  (check-exn exn? (λ () (array-axis-swap arr 0 1)))
  (check-equal? (array-axis-swap arr 0 0) arr))

(let ([arr  (indexes-array #(2 2))])
  (check-exn exn? (λ () (array-axis-swap arr 0 2)))
  (check-equal? (array-axis-swap arr 0 0) arr)
  (check-equal? (array-axis-swap arr 1 0)
                (array #[#['#(0 0) '#(1 0)]
                         #['#(0 1) '#(1 1)]])))

(check-equal? (array-axis-swap (indexes-array #(2 2 2)) 1 2)
              (array #[#[#['#(0 0 0) '#(0 1 0)]
                         #['#(0 0 1) '#(0 1 1)]]
                       #[#['#(1 0 0) '#(1 1 0)]
                         #['#(1 0 1) '#(1 1 1)]]]))

;; Adding axes

(let ([arr  (indexes-array #())])
  (check-exn exn? (λ () (array-axis-insert arr 1 1)))
  (check-equal? (array-axis-insert arr 0 2)
                (array #['#() '#()])))

(let ([arr  (indexes-array #(4))])
  (check-equal? (array-axis-insert arr 0 2)
                (array #[#['#(0) '#(1) '#(2) '#(3)]
                         #['#(0) '#(1) '#(2) '#(3)]]))
  (check-equal? (array-axis-insert arr 1 2)
                (array #[#['#(0) '#(0)]
                         #['#(1) '#(1)]
                         #['#(2) '#(2)]
                         #['#(3) '#(3)]])))

;; Removing axes

(let ([arr  (indexes-array #())])
  (check-exn exn? (λ () (array-axis-ref arr 0 0))))

(let ([arr  (indexes-array #(4))])
  (check-exn exn? (λ () (array-axis-ref arr 1 0)))
  (check-equal? (array-axis-ref arr 0 2)
                (array '#(2))))

(let ([arr  (indexes-array #(2 2))])
  (check-equal? (array-axis-ref arr 0 0)
                (array #['#(0 0) '#(0 1)]))
  (check-equal? (array-axis-ref arr 1 0)
                (array #['#(0 0) '#(1 0)])))

;; Reshape, flatten

(let ([arr  (indexes-array #())])
  (check-exn exn? (λ () (array-reshape arr #(0))))
  (check-equal? (array-reshape arr #(1))
                (array #['#()]))
  (check-equal? (array-flatten arr)
                (array #['#()])))

(let ([arr  (array-map (λ: ([js : Indexes]) (vector-ref js 0))
                       (indexes-array #(4)))])
  (check-exn exn? (λ () (array-reshape arr #())))
  (check-equal? (array-reshape arr #(2 2))
                (array #[#[0 1] #[2 3]]))
  (check-equal? (array-flatten arr)
                (array #[0 1 2 3])))

;; Append

(check-exn exn? (λ () (array-append* (list))))
(check-exn exn? (λ () (array-append* (list (array 0) (array 1)))))

(check-equal? (array-append* (list (array #[0])))
              (array #[0]))
(check-equal? (array-append* (list (array #[0]) (array #[1])))
              (array #[0 1]))
(check-equal? (array-append* (list (array #[0]) (array 1)))
              (array #[0 1]))
(check-equal? (array-append* (list (array 0) (array #[1])))
              (array #[0 1]))
(check-equal? (array-append* (list (array #[0 1 2]) (array #[3 4 5])))
              (array #[0 1 2 3 4 5]))
(check-exn exn? (λ () (array-append* (list (array #[0]) (array #[1])) 1)))

(check-equal? (array-append* (list (array #[#[0 1] #[2 3]]) (array #[#[4 5] #[6 7]])))
              (array #[#[0 1] #[2 3] #[4 5] #[6 7]]))
(check-equal? (array-append* (list (array #[#[0 1] #[2 3]]) (array #[#[4 5] #[6 7]])) 1)
              (array #[#[0 1 4 5] #[2 3 6 7]]))
(check-equal? (array-append* (list (array #[#[0 1] #[2 3]]) (array 0)) 0)
              (array #[#[0 1] #[2 3] #[0 0]]))
(check-equal? (array-append* (list (array #[#[0 1] #[2 3]]) (array 0)) 1)
              (array #[#[0 1 0] #[2 3 0]]))
(check-equal? (array-append* (list (array #[#[0 1 2] #[3 4 5]]) (array #[0 1])) 1)
              (array #[#[0 1 2 0 1] #[3 4 5 0 1]]))
(check-exn exn? (λ () (array-append* (list (array #[#[0 1 2] #[3 4 5]]) (array #[0 1])) 0)))

;; ---------------------------------------------------------------------------------------------------
;; Comprehensions

(check-equal? (for/array: #:shape #() () : Integer 3)
              (mutable-array 3))
(check-equal? (for/array: #:shape #() () : Symbol 'foo) 
              (mutable-array 'foo))
(check-equal? (for/array: #:shape #(2) ([x (in-naturals)]) : Integer x) 
              (mutable-array #[0 1]))
(check-equal? (for/array: #:shape #(2 3) ([i (in-range 0 6)]) : (Vectorof Integer)
                (vector (quotient i 3) (remainder i 3)))
              (indexes-array #(2 3)))

(check-equal? (for*/array: #:shape #() () : Integer 3)
              (mutable-array 3))
(check-equal? (for*/array: #:shape #() () : Symbol 'foo)
              (mutable-array 'foo))
(check-equal? (for*/array: #:shape #(2) ([x (in-naturals)]) : Integer x)
              (mutable-array #[0 1]))
(check-equal? (for*/array: #:shape #(2 3) ([i (in-range 0 2)]
                                           [j (in-range 0 3)]
                                           ) : (Vectorof Integer)
                (vector i j))
              (indexes-array #(2 3)))

(check-equal? (for*/array: #:shape #() () : Integer 3)
              (for*/array: #:shape #() () : Integer 3))
(check-equal? (for*/array: #:shape #() () : Symbol 'foo)
              (for*/array: #:shape #() () : Symbol 'foo))
(check-equal? (for*/array: #:shape #(2) ([x (in-naturals)]) : Integer x)
              (for*/array: #:shape #(2) ([x (in-naturals)]) : Integer x))
(check-equal? (for*/array: #:shape #(2 3) ([i (in-range 0 2)]
                                           [j (in-range 0 3)]
                                           ) : (Listof Integer)
                (list i j))
              (for*/array: #:shape #(2 3) ([i (in-range 0 2)]
                                           [j (in-range 0 3)]
                                           ) : (Listof Integer)
                (list i j)))

;; ---------------------------------------------------------------------------------------------------
;; Sequences

(check-equal? (for/list: : (Listof Number) ([x  (in-array (array #[#[1 2 3] #[4 5 6]]))]) x)
              '(1 2 3 4 5 6))

(check-equal? (for/list: : (Listof Indexes) ([js  (in-array (indexes-array #()))]) js)
              '(#()))
(check-equal? (for/list: : (Listof Indexes) ([js  (in-array (indexes-array #(0)))]) js)
              '())
(check-equal? (for/list: : (Listof Indexes) ([js  (in-array (indexes-array #(2 2)))]) js)
              '(#(0 0) #(0 1) #(1 0) #(1 1)))

(check-equal? (sequence->list (in-array (indexes-array #())))
              '(#()))
(check-equal? (sequence->list (in-array (indexes-array #(0))))
              '())
(check-equal? (sequence->list (in-array (indexes-array #(2 2))))
              '(#(0 0) #(0 1) #(1 0) #(1 1)))

(check-equal? (for/list: : (Listof Indexes) ([js  (in-array-indexes #())]) js)
              '(#()))
(check-equal? (for/list: : (Listof Indexes) ([js  (in-array-indexes #(0))]) js)
              '())
(check-equal? (for/list: : (Listof Indexes) ([js  (in-array-indexes #(2 2))]) js)
              '(#(0 0) #(0 1) #(1 0) #(1 1)))
(check-equal? (sequence->list (in-array-indexes #()))
              '(#()))
(check-equal? (sequence->list (in-array-indexes #(0)))
              '())
(check-equal? (sequence->list (in-array-indexes #(2 2)))
              '(#(0 0) #(0 1) #(1 0) #(1 1)))

(check-equal? (for/list: : (Listof Indexes) ([js  (in-unsafe-array-indexes #(2 2))]) js)
              '(#(0 0) #(0 0) #(0 0) #(0 0)))
(check-equal? (for/list: : (Listof Indexes) ([js  (in-unsafe-array-indexes #())]) (vector-copy js))
              '(#()))
(check-equal? (for/list: : (Listof Indexes) ([js  (in-unsafe-array-indexes #(0))]) (vector-copy js))
              '())
(check-equal? (for/list: : (Listof Indexes) ([js  (in-unsafe-array-indexes #(2 2))]) (vector-copy js))
              '(#(0 0) #(0 1) #(1 0) #(1 1)))
(check-equal? (sequence->list (in-unsafe-array-indexes #()))
              '(#()))
(check-equal? (sequence->list (in-unsafe-array-indexes #(0)))
              '())
(check-equal? (sequence->list (in-unsafe-array-indexes #(2 2)))
              '(#(0 0) #(0 1) #(1 0) #(1 1)))

(let ([arr  (indexes-array #(4 5))])
  (check-equal? (for/list: : (Listof (Listof Indexes)) ([brr  (in-array-axis arr 0)])
                  (for/list: : (Listof Indexes) ([js  (in-array brr)])
                    js))
                (array->list* arr))
  (check-equal? (for/list: : (Listof (Listof Indexes)) ([brr  (in-array-axis arr 1)])
                  (for/list: : (Listof Indexes) ([js  (in-array brr)])
                    js))
                (array->list* (array-axis-swap arr 0 1))))

(check-equal? (array-list->array empty 0)
              (array #[]))
(check-exn exn? (λ () (array-list->array empty 1)))

(check-equal? (array-list->array (list (array 0) (array 1) (array 2)) 0)
              (array #[0 1 2]))
(check-exn exn? (λ () (array-list->array (list (array 0) (array 1) (array 2)) 1)))

(check-equal? (array-list->array (list (array #[0 1]) (array #[2 3])) 0)
              (array #[#[0 1] #[2 3]]))
(check-equal? (array-list->array (list (array #[0 1]) (array #[2 3])) 1)
              (array #[#[0 2] #[1 3]]))

(check-equal? (array-list->array (list (array #[0 1 2]) (array 0)) 0)
              (array #[#[0 1 2] #[0 0 0]]))
(check-equal? (array-list->array (list (array #[0 1 2]) (array 0)) 1)
              (array #[#[0 0] #[1 0] #[2 0]]))
(check-exn exn? (λ () (array-list->array (list (array #[0 1 2]) (array 0)) 2)))

(check-equal? (array-list->array (list (array #[#[0 1] #[2 3]]) (array 0)) 0)
              (array #[#[#[0 1] #[2 3]] #[#[0 0] #[0 0]]]))

(check-equal? (array->array-list (array-list->array empty 0) 0)
              empty)
(check-exn exn? (λ () (array->array-list (array-list->array empty 0) 1)))

(check-equal? (array->array-list (array-list->array (list (array 0) (array 1) (array 2)) 0) 0)
              (list (array 0) (array 1) (array 2)))

(check-equal? (array->array-list (array-list->array (list (array #[0 1]) (array #[2 3])) 0) 0)
              (list (array #[0 1]) (array #[2 3])))
(check-equal? (array->array-list (array-list->array (list (array #[0 1]) (array #[2 3])) 1) 1)
              (list (array #[0 1]) (array #[2 3])))

(check-equal? (array->array-list (array-list->array (list (array #[0 1 2]) (array 0)) 0) 0)
              (list (array #[0 1 2]) (array #[0 0 0])))
(check-equal? (array->array-list (array-list->array (list (array #[0 1 2]) (array 0)) 1) 1)
              (list (array #[0 1 2]) (array #[0 0 0])))

(check-equal? (array->array-list (array-list->array (list (array #[#[0 1] #[2 3]]) (array 0)) 0) 0)
              (list (array #[#[0 1] #[2 3]]) (array #[#[0 0] #[0 0]])))

;; ---------------------------------------------------------------------------------------------------
;; Conditionals

(let ([arr  (build-array #(10 10) (λ: ([js : Indexes]) (apply + (vector->list js))))])
  (check-equal? (array-if (array< arr (array 5))
                          (array 0)
                          (array 1))
                (array-map (λ: ([v : Integer]) (if (v . < . 5) 0 1)) arr))
  (check-equal? (array-or (array< arr (make-array #(10 10) 9))
                          (array> arr (make-array #(10 10) 9)))
                (array-map (λ: ([v : Integer]) (or (< v 9) (> v 9))) arr))
  (check-equal? (array-and (array< arr (make-array #(10 10) 10))
                           (array> arr (make-array #(10 10) 8)))
                (array-map (λ: ([v : Integer]) (= v 9)) arr)))

;; ---------------------------------------------------------------------------------------------------
;; Lazy arrays

(define (make-lazy-arr)
  (: arr (Array Integer))
  (define arr
    (array-lazy
     (build-simple-array
      #(12 12)
      (λ: ([js : Indexes])
        (match-define (vector j0 j1) js)
        (cond [(or (= j0 0) (= j1 0))  1]
              [(undefined? arr)  (error 'undefined)]
              [else  (+ (array-ref arr (vector (- j0 1) j1))
                        (array-ref arr (vector j0 (- j1 1))))])))))
  arr)

(define (make-mutable-arr)
  (define: arr : (Mutable-Array Integer)
    (array->mutable-array
     (build-array
      #(12 12)
      (λ: ([js : Indexes])
        (match-define (vector j0 j1) js)
        (cond [(or (= j0 0) (= j1 0))  1]
              [else  0])))))
  (for*: ([j0  (in-range 1 12)]
          [j1  (in-range 1 12)])
    (array-set! arr (vector j0 j1)
                (+ (array-ref arr (vector (- j0 1) j1))
                   (array-ref arr (vector j0 (- j1 1))))))
  arr)

(check-equal? (make-lazy-arr)
              (make-mutable-arr))

(check-equal? (array-lazy (array-axis-swap (indexes-array #(4 4)) 0 1))
              (array-axis-swap (indexes-array #(4 4)) 0 1))
