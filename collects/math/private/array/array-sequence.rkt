#lang typed/racket

(require "../unsafe.rkt"
         "array-struct.rkt"
         "array-constructors.rkt"
         "array-transform.rkt"
         "array-broadcast.rkt"
         "utils.rkt")

(provide (rename-out [in-array-clause  in-array]
                     [in-array-indexes-clause  in-array-indexes]
                     [in-unsafe-array-indexes-clause  in-unsafe-array-indexes])
         in-array-axis
         array->array-list
         array-list->array)

;; ===================================================================================================
;; Sequence of array elements

(: in-array (All (A) ((Array A) -> (Sequenceof A))))
(define (in-array arr)
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (define size (array-size arr))
  (define proc (unsafe-array-proc arr))
  (define: js : Indexes (make-vector dims 0))
  (make-do-sequence
   (λ () (values (λ: ([jjs : (Pair Fixnum Indexes)]) (proc (cdr jjs)))
                 (λ: ([jjs : (Pair Fixnum Indexes)])
                   (define js (vector-copy-all (cdr jjs)))
                   (next-indexes! ds dims js)
                   (cons (unsafe-fx+ (car jjs) 1) js))
                 (cons 0 js)
                 (λ: ([jjs : (Pair Fixnum Indexes)]) ((car jjs) . < . size))
                 #f
                 #f))))

; (in-array a]
;     Returns a sequence of all elements of the array a.
(define-sequence-syntax in-array-clause
  (λ () #'in-array)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ arr-expr)]
       (syntax/loc stx
         [(x)
          (:do-in
           ([(ds size dims js proc)
             (plet: (A) ([arr : (Array A) arr-expr])
               (define ds (array-shape arr))
               (define dims (vector-length ds))
               (define size (array-size arr))
               (define proc (unsafe-array-proc arr))
               (define: js : Indexes (make-vector dims 0))
               (values ds size dims js proc))])
           (void)
           ([#{j : Nonnegative-Fixnum} 0])
           (< j size)
           ([(x)  (proc js)])
           #true
           #true
           [(begin (next-indexes! ds dims js)
                   (+ j 1))])])]
      [[_ clause] (raise-syntax-error 'in-array "expected (in-array <Array>)" #'clause #'clause)])))

;; ===================================================================================================
;; Sequence of indexes

(: in-array-indexes (User-Indexes -> (Sequenceof Indexes)))
(define (in-array-indexes ds)
  (let: ([ds : Indexes  (check-array-shape
                         ds (λ () (raise-type-error 'in-array-indexes "Indexes" ds)))])
    (define dims (vector-length ds))
    (define size (array-shape-size ds))
    (cond [(index? size)
           (define: js : Indexes (make-vector dims 0))
           (make-do-sequence
            (λ () (values (λ: ([jjs : (Pair Fixnum Indexes)]) (cdr jjs))
                          (λ: ([jjs : (Pair Fixnum Indexes)])
                            (define js (vector-copy (cdr jjs)))
                            (next-indexes! ds dims js)
                            (cons (unsafe-fx+ (car jjs) 1) js))
                          (cons 0 js)
                          (λ: ([jjs : (Pair Fixnum Indexes)]) ((car jjs) . < . size))
                          #f
                          #f)))]
          [else  (error 'in-array-indexes
                        "array size ~e (for shape ~e) is too large (is not an Index)"
                        size ds)])))

(define-sequence-syntax in-array-indexes-clause
  (λ () #'in-array-indexes)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ ds-expr)]
       (syntax/loc stx
         [(x)
          (:do-in
           ([(ds size dims js)
             (let*: ([ds : User-Indexes  ds-expr]
                     [ds : Indexes  (check-array-shape
                                     ds (λ () (raise-type-error 'in-array-indexes "Indexes" ds)))])
               (define dims (vector-length ds))
               (define size (array-shape-size ds))
               (cond [(index? size)  (define: js : Indexes (make-vector dims 0))
                                     (values ds size dims js)]
                     [else  (error 'in-array-indexes
                                   "array size ~e (for shape ~e) is too large (is not an Index)"
                                   size ds)]))])
           (void)
           ([#{j : Nonnegative-Fixnum} 0])
           (< j size)
           ([(x)  (vector-copy-all js)])
           #true
           #true
           [(begin (next-indexes! ds dims js)
                   (+ j 1))])])]
      [[_ clause]
       (raise-syntax-error 'in-array-indexes "expected (in-array-indexes <Indexes>)"
                           #'clause #'clause)])))

(: in-unsafe-array-indexes (Indexes -> (Sequenceof Indexes)))
(define (in-unsafe-array-indexes ds)
  (in-array-indexes ds))

(define-sequence-syntax in-unsafe-array-indexes-clause
  (λ () #'in-array-indexes)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ ds-expr)]
       (syntax/loc stx
         [(x)
          (:do-in
           ([(ds size dims js)
             (let: ([ds : Indexes  ds-expr])
               (define dims (vector-length ds))
               (define size (array-shape-size ds))
               (cond [(index? size)  (define: js : Indexes (make-vector dims 0))
                                     (values ds size dims js)]
                     [else  (error 'in-array-indexes
                                   "array size ~e (for shape ~e) is too large (is not an Index)"
                                   size ds)]))])
           (void)
           ([#{j : Nonnegative-Fixnum} 0])
           (< j size)
           ([(x)  js])
           #true
           #true
           [(begin (next-indexes! ds dims js)
                   (+ j 1))])])]
      [[_ clause]
       (raise-syntax-error 'in-array-indexes "expected (in-unsafe-array-indexes <Indexes>)"
                           #'clause #'clause)])))

;; ===================================================================================================
;; in-array-axis

(: in-array-axis (All (A) (case-> ((Array A) -> (Sequenceof (Array A)))
                                  ((Array A) Integer -> (Sequenceof (Array A))))))
(define (in-array-axis arr [k 0])
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (cond [(and (0 . <= . k) (k . < . dims))
         (define dk (unsafe-vector-ref ds k))
         (make-do-sequence
          (λ ()
            (values (λ: ([jk : Integer]) (array-axis-ref arr k jk))
                    add1
                    0
                    (λ: ([jk : Integer]) (jk . < . dk))
                    #f
                    #f)))]
        [else
         (error 'in-array-axis (format "expected axis Index < ~e; given ~e" dims k))]))

(: array->array-list (All (A) (case-> ((Array A) -> (Listof (Array A)))
                                      ((Array A) Integer -> (Listof (Array A))))))
(define (array->array-list arr [k 0])
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (cond [(and (0 . <= . k) (k . < . dims))
         (define dk (unsafe-vector-ref ds k))
         (build-list dk (λ: ([jk : Index]) (array-axis-ref arr k jk)))]
        [else
         (error 'array->array-list (format "expected axis Index < ~e; given ~e" dims k))]))

(: array-list->array (All (A) (case-> ((Listof (Array A)) -> (Array A))
                                      ((Listof (Array A)) Integer -> (Array A)))))
(define (array-list->array arrs [k 0])
  (define ds (array-shape-broadcast ((inst map Indexes (Array A)) array-shape arrs)))
  (define dims (vector-length ds))
  (cond [(and (0 . <= . k) (k . <= . dims))
         (let ([arrs  (list->vector (map (λ: ([arr : (Array A)]) (array-broadcast arr ds)) arrs))])
           (define dk (vector-length arrs))
           (define new-ds (unsafe-vector-insert ds k dk))
           (unsafe-build-array
            new-ds (λ: ([js : Indexes])
                     (define jk (unsafe-vector-ref js k))
                     (let ([old-js  (unsafe-vector-remove js k)])
                       ((unsafe-array-proc (unsafe-vector-ref arrs jk)) old-js)))))]
        [else
         (error 'array-list->array (format "expected axis Index <= ~e; given ~e" dims k))]))
