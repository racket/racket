#lang typed/racket/base

(require "../unsafe.rkt"
         "array-struct.rkt"
         "utils.rkt")

(provide make-array
         axis-index-array
         index-array
         indexes-array
         diagonal-array)

(: make-array (All (A) (User-Indexes A -> (View-Array A))))
(define (make-array ds v)
  (let ([ds  (check-array-shape
              ds (λ () (raise-type-error 'make-array "(Vectorof Index)" 0 ds v)))])
    (unsafe-view-array ds (λ (js) v))))

(: axis-index-array (User-Indexes Integer -> (View-Array Index)))
(define (axis-index-array ds k)
  (let* ([ds  (check-array-shape
               ds (λ () (raise-type-error 'axis-index-array "(Vectorof Index)" 0 ds k)))]
         [dims  (vector-length ds)])
    (cond [(and (0 . <= . k) (k . < . dims))
           (unsafe-view-array ds (λ: ([js : Indexes]) (unsafe-vector-ref js k)))]
          [else  (raise-type-error 'axis-index-array (format "Index < ~a" dims) 1 ds k)])))

(: index-array (User-Indexes -> (View-Array Index)))
(define (index-array ds)
  (let ([ds  (check-array-shape
              ds (λ () (raise-type-error 'index-array "(Vectorof Index)" ds)))])
    (unsafe-view-array ds (λ: ([js : Indexes])
                            (define j (unsafe-array-index->value-index ds js))
                            (with-asserts ([j index?]) j)))))

(: indexes-array (User-Indexes -> (View-Array Indexes)))
(define (indexes-array ds)
  (let ([ds  (check-array-shape
              ds (λ () (raise-type-error 'indexes-array "(Vectorof Index)" ds)))])
    (unsafe-view-array ds (λ: ([js : Indexes]) (vector-copy-all js)))))

(: diagonal-array (All (A) (Integer Integer A A -> (View-Array A))))
(define (diagonal-array dims size on-value off-value)
  (cond [(not (index? dims))  (raise-type-error 'diagonal-array "Index" 0 dims size)]
        [(not (index? size))  (raise-type-error 'diagonal-array "Index" 1 dims size)]
        [else
         (define: ds : Indexes (make-vector dims size))
         ;; specialize for various cases
         (cond [(or (dims . <= . 1) (size . <= . 1))
                (unsafe-view-array ds (λ: ([js : Indexes]) on-value))]
               [(= dims 2)
                (unsafe-view-array
                 ds (λ: ([js : Indexes])
                      (define j0 (unsafe-vector-ref js 0))
                      (define j1 (unsafe-vector-ref js 1))
                      (if (= j0 j1) on-value off-value)))]
               [else
                (unsafe-view-array
                 ds (λ: ([js : Indexes])
                      (define j0 (unsafe-vector-ref js 0))
                      (let: loop : A ([i : Nonnegative-Fixnum  1])
                        (cond [(i . >= . dims)  on-value]
                              [(= (unsafe-vector-ref js i) j0)  (loop (+ i 1))]
                              [else  off-value]))))])]))
