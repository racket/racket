#lang typed/racket/base

(require racket/list
         racket/match
         "../unsafe.rkt"
         "array-struct.rkt"
         "array-transform.rkt"
         "array-constructors.rkt"
         "array-broadcast.rkt"
         "array-ref.rkt"
         "for-each.rkt"
         "utils.rkt")

(provide
 ;; Indexing by array of indexes
 array-indexes-ref
 array-indexes-set!
 ;; Slicing
 (rename-out [-Slice Slice]
             [-Slice-Dots Slice-Dots]
             [-Slice-New-Axis Slice-New-Axis])
 Slice-Spec
 :: slice? slice-start slice-end slice-step slice-apply slice->sequence
 ::... slice-dots?
 ::new slice-new-axis? slice-new-axis-length
 array-slice-ref
 slice-indexes-array
 array-slice-set!)

;; ===================================================================================================
;; Indexing using array of indexes

(: array-indexes-ref (All (A) ((Array A) (Array User-Indexes) -> (View-Array A))))
(define (array-indexes-ref arr idxs)
  (let ([arr   (array-view arr)]
        [idxs  (array-view idxs)])
    (define ds (array-shape idxs))
    (define idxs-proc (unsafe-array-proc idxs))
    (unsafe-view-array ds (λ: ([js : Indexes]) (array-ref arr (idxs-proc js))))))

(: array-indexes-set! (All (A) ((Strict-Array A) (Array User-Indexes) (Array A) -> Void)))
(define (array-indexes-set! arr idxs vals)
  (let*-values ([(idxs vals)  (array-broadcast idxs vals)]
                [(idxs)  (array-view idxs)]
                [(vals)  (array-view vals)])
    (define ds (array-shape idxs))
    (define idxs-proc (unsafe-array-proc idxs))
    (define vals-proc (unsafe-array-proc vals))
    (for-each-array-index ds (λ (js) (array-set! arr (idxs-proc js) (vals-proc js))))))

;; ===================================================================================================
;; Slicing

(define-type End-Index (U Index -1))
(define-predicate end-index? End-Index)

(struct: Slice ([start : (U Index #f)]
                [end : (U End-Index #f)]
                [step : Fixnum])
  #:property prop:custom-write
  (λ (s port _)
    (write-string (format "(:: ~a ~a ~a)" (slice-start s) (slice-end s) (slice-step s)) port)))

(struct: Slice-Dots () #:property prop:custom-write (λ (s port _) (write-string "::..." port)))

(struct: Slice-New-Axis ([length : Index])
  #:property prop:custom-write
  (λ (s port _) (write-string (format "(::new ~a)" (slice-new-axis-length s)) port)))

(define-type -Slice Slice)
(define-type -Slice-Dots Slice-Dots)
(define-type -Slice-New-Axis Slice-New-Axis)
(define-type Slice-Spec-- (U Integer Slice (Sequenceof Integer)))
(define-type Slice-Spec- (U Slice-Spec-- Slice-New-Axis))
(define-type Slice-Spec (U Slice-Spec- Slice-Dots))

(define slice? Slice?)
(define slice-start Slice-start)
(define slice-end Slice-end)
(define slice-step Slice-step)

(define ::... (Slice-Dots))

(define slice-dots? Slice-Dots?)

(: ::new (case-> (-> Slice-New-Axis)
                 (Integer -> Slice-New-Axis)))
(define (::new [dk 1])
  (cond [(index? dk)  (Slice-New-Axis dk)]
        [else  (raise-type-error '::new "Index" dk)]))

(define slice-new-axis? Slice-New-Axis?)
(define slice-new-axis-length Slice-New-Axis-length)

(: :: (case-> (-> Slice)
              ((U Integer #f) -> Slice)
              ((U Integer #f) (U Integer #f) -> Slice)
              ((U Integer #f) (U Integer #f) Integer -> Slice)))
(define ::
  (case-lambda
    [()  (Slice #f #f 1)]
    [(end)  (cond [(or (not end) (index? end))  (Slice 0 end 1)]
                  [else  (raise-type-error 'Slice "Index or #f" end)])]
    [(start end)  (:: start end (if (and start end (end . < . start)) -1 1))]
    [(start end step)
     (cond [(not (or (not start) (index? start)))
            (raise-type-error 'Slice "Index or #f" 0 start end step)]
           [(not (or (not end) (end-index? end)))
            (raise-type-error 'Slice "Index, -1 or #f" 1 start end step)]
           [(not (fixnum? step))
            (raise-type-error 'Slice "Fixnum" 2 start end step)]
           [(and start end (start . < . end) (step . <= . 0))
            (raise-type-error 'Slice "Positive-Fixnum" 2 start end step)]
           [(and start end (start . > . end) (step . >= . 0))
            (raise-type-error 'Slice "Negative-Fixnum" 2 start end step)]
           [else
            (Slice start end step)])]))

(: do-slice-apply (Symbol Slice Integer Index -> (Values Index End-Index)))
(define (do-slice-apply name s k dk)
  (match-define (Slice start end step) s)
  (let-values ([(start end)  (cond [(step . < . 0)
                                    (values (or (slice-start s) (- dk 1))
                                            (or (slice-end s) -1))]
                                   [else
                                    (values (or (slice-start s) 0)
                                            (or (slice-end s) dk))])])
    (cond [(zero? step)  (values 0 0)]
          [(or (start . < . 0) (start . >= . dk))
           (if (k . >= . 0)
               (error name
                      "expected Index < ~e or #f for start index in slice ~e (axis ~e); given ~e"
                      dk s k start)
               (error name "expected Index < ~e or #f for start index; given ~e" dk start))]
          [(or (end . > . dk) (not (end-index? end)))
           (if (k . >= . 0)
               (error name
                      "expected Index <= ~e, -1 or #f for end index in slice ~e (axis ~e); given ~e"
                      dk s k end)
               (error name "expected Index <= ~e, -1 or #f for end index; given ~e" dk end))]
          [else  (values start end)])))

(: slice-apply (Slice Integer -> (Values Index End-Index)))
(define (slice-apply s dk)
  (cond [(index? dk)  (do-slice-apply 'slice-apply s -1 dk)]
        [else  (raise-type-error 'slice-apply "Index" 1 s dk)]))

(: slice->sequence (Slice Integer -> (Sequenceof Integer)))
(define (slice->sequence s dk)
  (define-values (start end) (slice-apply s dk))
  (in-range start end (slice-step s)))

(: slice->vector (Slice Integer Index -> (Vectorof Index)))
(define (slice->vector s k dk)
  (define-values (start end) (do-slice-apply 'array-slice-ref s k dk))
  (define step (slice-step s))
  (cond [(= start end 0)  (vector)]
        [else
         (define size
           (cond [(step . < . 0)  (quotient (+ (- end start) (+ step 1)) step)]
                 [else  (quotient (+ (- end start) (- step 1)) step)]))
         (cond [(index? size)
                (define: jks : (Vectorof Index) (make-vector size 0))
                (let loop ([#{i : Nonnegative-Fixnum} 0] [#{jk : Fixnum} start])
                  (cond [(i . >= . size)  jks]
                        [(or (jk . < . 0) (jk . >= . dk))
                         (error 'slice->vector
                                "expected Index < ~e in slice ~e (axis ~e); given ~e"
                                dk s k jk)]
                        [else
                         (unsafe-vector-set! jks i jk)
                         (loop (+ i 1) (unsafe-fx+ jk step))]))]
               [else
                (error 'array-slice-ref "axis for slice ~e (axis ~e) is too large" s k)])]))

(: slices->array-axis-transform
   (All (A) (Symbol (Array A) (Listof Slice-Spec--) -> (Values (Array A)
                                                               (Vectorof (Vectorof Index))))))
(define (slices->array-axis-transform name arr slices)
  (define n (length slices))
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (define-values (new-arr old-jss)
    (for/fold: ([arr : (Array A)  arr]
                [jss : (Listof (Vectorof Index))  null]
                ) ([s  (in-list (reverse slices))]
                   [k  (in-range (- dims 1) -1 -1)])
      (define dk (unsafe-vector-ref ds k))
      (cond [(integer? s)
             (when (or (s . < . 0) (s . >= . dk))
               (error name "expected Index < ~e in slice ~e (axis ~e)" dk s k))
             (values (array-axis-remove arr k s) jss)]
            [(slice? s)
             (values arr (cons (slice->vector s k dk) jss))]
            [else
             (define js
               (for/fold: ([js : (Listof Index)  null]) ([jk s])
                 (cond [(or (jk . < . 0) (jk . >= . dk))
                        (error name "expected Index < ~e in slice ~e (axis ~e); given ~e"
                               dk s k jk)]
                       [else  (cons jk js)])))
             (values arr (cons ((inst list->vector Index) (reverse js)) jss))])))
  (values new-arr (list->vector old-jss)))

(: expand-dots (Index (Listof Slice-Spec) -> (Listof Slice-Spec-)))
(define (expand-dots dims slices)
  (define n (count (λ (s) (and (not (slice-dots? s)) (not (slice-new-axis? s)))) slices))
  (let loop ([slices slices] [n n])
    (cond [(null? slices)  null]
          [(slice-dots? (car slices))
           (append (make-list (max 0 (- dims n)) (Slice #f #f 1))
                   (loop (cdr slices) dims))]
          [else  (cons (car slices) (loop (cdr slices) n))])))

(: extract-new-axes ((Listof Slice-Spec-) -> (Values (Listof Slice-Spec--)
                                                     (Listof (Pair Integer Index)))))
(define (extract-new-axes slices)
  (define-values (new-slices new-axes _)
    (for/fold: ([new-slices : (Listof Slice-Spec--)  null]
                [new-axes : (Listof (Pair Integer Index))  null]
                [k : Integer  0]
                ) ([s  (in-list slices)])
      (cond [(slice-new-axis? s)
             (values new-slices (cons (cons k (slice-new-axis-length s)) new-axes) k)]
            [else
             (values (cons s new-slices) new-axes (+ k 1))])))
  (values (reverse new-slices) (reverse new-axes)))

(: array-slice-ref (All (A) ((Array A) (Listof Slice-Spec) -> (View-Array A))))
(define (array-slice-ref arr orig-slices)
  (define dims (array-dims arr))
  (let*-values ([(slices)  (expand-dots dims orig-slices)]
                [(slices new-axes)  (extract-new-axes slices)])
    ;; number of indexes should match
    (define num-specs (length slices))
    (unless (= dims num-specs)
      (error 'array-slice-ref "expected list with ~e slices; given ~e in ~e"
             dims num-specs orig-slices))
    (let-values ([(arr jss)  (slices->array-axis-transform 'array-slice-ref arr slices)])
      (for/fold ([arr (unsafe-array-axis-transform arr jss)]) ([na  (in-list new-axes)])
        (match-define (cons k dk) na)
        (array-axis-insert arr k dk)))))

(: slice-indexes-array (User-Indexes (Listof Slice-Spec) -> (View-Array Indexes)))
(define (slice-indexes-array ds slices)
  (array-slice-ref (indexes-array ds) slices))

(: array-slice-set! (All (A) ((Strict-Array A) (Listof Slice-Spec) (Array A) -> Void)))
(define (array-slice-set! arr slices vals)
  (array-indexes-set! arr (slice-indexes-array (array-shape arr) slices) vals))
