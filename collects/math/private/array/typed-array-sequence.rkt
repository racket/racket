#lang typed/racket/base

(require racket/vector
         "../unsafe.rkt"
         "array-struct.rkt"
         "array-transform.rkt"
         "array-broadcast.rkt"
         "utils.rkt")

(provide (all-defined-out))

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

;; ===================================================================================================
;; Sequence of indexes

(: in-array-indexes (In-Indexes -> (Sequenceof Indexes)))
(define (in-array-indexes ds)
  (let: ([ds : Indexes  (check-array-shape
                         ds (λ () (raise-argument-error 'in-array-indexes "Indexes" ds)))])
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

(: in-unsafe-array-indexes (Indexes -> (Sequenceof Indexes)))
(define (in-unsafe-array-indexes ds)
  (in-array-indexes ds))

;; ===================================================================================================
;; Sequence of axes

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
           (array-default-strict
            (unsafe-build-array
             new-ds (λ: ([js : Indexes])
                      (define jk (unsafe-vector-ref js k))
                      (let ([old-js  (unsafe-vector-remove js k)])
                        ((unsafe-array-proc (unsafe-vector-ref arrs jk)) old-js))))))]
        [else
         (error 'array-list->array (format "expected axis Index <= ~e; given ~e" dims k))]))
