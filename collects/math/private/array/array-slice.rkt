#lang typed/racket/base

(require racket/list
         "../unsafe.rkt"
         "array-struct.rkt"
         "array-transform.rkt")

(provide (rename-out [-slice slice]
                     [-slice-dots slice-dots])
         slice? slice-dots? :: ::... array-slice)

(define-type End-Index (U Index -1))
(define-predicate end-index? End-Index)

(struct: slice ([start : (U Index #f)]
                [end : (U End-Index #f)]
                [step : Fixnum])
  #:property prop:custom-write
  (λ (s port _)
    (write-string (format "(:: ~a ~a ~a)" (slice-start s) (slice-end s) (slice-step s))
                  port)))

(struct: slice-dots ()
  #:property prop:custom-write
  (λ (s port _) (write-string "::..." port)))

(define ::... (slice-dots))

(define-type -slice slice)
(define-type -slice-dots slice-dots)

(: :: (case-> (-> slice)
              ((U Integer #f) -> slice)
              ((U Integer #f) (U Integer #f) -> slice)
              ((U Integer #f) (U Integer #f) Integer -> slice)))
(define ::
  (case-lambda
    [()  (slice #f #f 1)]
    [(end)  (cond [(or (not end) (index? end))  (slice 0 end 1)]
                  [else  (raise-type-error 'slice "Index or #f" end)])]
    [(start end)  (:: start end (if (and start end (end . < . start)) -1 1))]
    [(start end step)
     (cond [(not (or (not start) (index? start)))
            (raise-type-error 'slice "Index or #f" 0 start end step)]
           [(not (or (not end) (end-index? end)))
            (raise-type-error 'slice "Index, -1 or #f" 1 start end step)]
           [(not (fixnum? step))
            (raise-type-error 'slice "Fixnum" 2 start end step)]
           [(and start end (start . < . end) (step . <= . 0))
            (raise-type-error 'slice "Positive-Fixnum" 2 start end step)]
           [(and start end (start . > . end) (step . >= . 0))
            (raise-type-error 'slice "Negative-Fixnum" 2 start end step)]
           [else
            (slice start end step)])]))

(: slice->vector (slice Integer Index -> (Vectorof Index)))
(define (slice->vector s k dk)
  (define step (slice-step s))
  (define-values (start end)
    (cond [(step . < . 0)  (values (or (slice-start s) (- dk 1))
                                   (or (slice-end s) -1))]
          [else  (values (or (slice-start s) 0)
                         (or (slice-end s) dk))]))
  (cond [(zero? step)  (vector)]
        [(or (start . < . 0) (start . >= . dk))
         (error 'array-slice
                "expected Index < ~e or #f for start index in slice ~e (axis ~e); given ~e"
                dk s k start)]
        [(or (end . < . -1) (end . > . dk))
         (error 'array-slice
                "expected Index <= ~e, -1 or #f for end index in slice ~e (axis ~e); given ~e"
                dk s k end)]
        [else
         (define size
           (cond [(step . < . 0)  (quotient (+ (- end start) (+ step 1)) step)]
                 [else  (quotient (+ (- end start) (- step 1)) step)]))
         (cond [(index? size)
                (define: js : (Vectorof Index) (make-vector size 0))
                (let loop ([#{i : Nonnegative-Fixnum} 0] [#{jk : Fixnum} start])
                  (cond [(i . >= . size)  js]
                        [(or (jk . < . 0) (jk . >= . dk))
                         (error 'slice->vector
                                "expected Index < ~e in slice ~e (axis ~e); given ~e"
                                dk s k jk)]
                        [else
                         (unsafe-vector-set! js i jk)
                         (loop (+ i 1) (unsafe-fx+ jk step))]))]
               [else
                (error 'array-slice "axis for slice ~e (axis ~e) is too large" s k)])]))

(: slices->array-axis-transform
   (All (A) (Symbol (Array A) (Listof (U Integer slice (Sequenceof Integer)))
                    -> (Values (Array A) (Vectorof (Vectorof Index))))))
(define (slices->array-axis-transform name arr slices)
  (define ds (unsafe-array-shape arr))
  (define dims (vector-length ds))
  (define-values (new-arr old-jss)
    (for/fold: ([arr : (Array A)  arr]
                [jss : (Listof (Vectorof Index))  null]
                ) ([slice  (in-list (reverse slices))]
                   [k  (in-range (- (array-dims arr) 1) -1 -1)])
      (define dk (unsafe-vector-ref ds k))
      (cond [(integer? slice)
             (when (or (slice . < . 0) (slice . >= . dk))
               (error 'array-slice "expected Index < ~e in slice ~e (axis ~e)"
                      dk slice k))
             (values (array-axis-remove arr k slice) jss)]
            [(slice? slice)
             (values arr (cons (slice->vector slice k dk) jss))]
            [else
             (define js
               (for/fold: ([js : (Listof Index)  null]) ([jk  slice])
                 (cond [(or (jk . < . 0) (jk . >= . dk))
                        (error 'array-slice "expected Index < ~e in slice ~e (axis ~e); given ~e"
                               dk slice k jk)]
                       [else  (cons jk js)])))
             (values arr (cons ((inst list->vector Index) (reverse js))
                               jss))])))
  (values new-arr (list->vector old-jss)))

(: expand-dots (Index (Listof (U Integer slice slice-dots (Sequenceof Integer)))
                      -> (Listof (U Integer slice (Sequenceof Integer)))))
(define (expand-dots dims slices)
  (let loop ([slices slices] [n  (count (compose not slice-dots?) slices)])
    (cond [(null? slices)  null]
          [(slice-dots? (car slices))
           (append (make-list (max 0 (- dims n)) (slice #f #f 1))
                   (loop (cdr slices) dims))]
          [else  (cons (car slices) (loop (cdr slices) n))])))

(: array-slice (All (A) ((Array A) (Listof (U Integer slice slice-dots (Sequenceof Integer)))
                                   -> (view-array A))))
(define (array-slice arr slices)
  (define dims (array-dims arr))
  (let ([slices  (expand-dots dims slices)])
    ;; number of indexes should match
    (unless (= dims (length slices))
      (error 'array-slice "expected ~e axis slices; given ~e in ~e"
             dims (length slices) slices))
    (let-values ([(arr jss)  (slices->array-axis-transform 'array-slice arr slices)])
      (unsafe-array-axis-transform arr jss))))
