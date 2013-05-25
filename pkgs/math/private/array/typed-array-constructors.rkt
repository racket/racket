#lang typed/racket/base

(require "../unsafe.rkt"
         "array-struct.rkt"
         "utils.rkt")

(provide (all-defined-out))

(: make-array (All (A) (In-Indexes A -> (Array A))))
(define (make-array ds v)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'make-array "(Vectorof Index)" 0 ds v)))])
    (unsafe-build-simple-array ds (λ (js) v))))

(: axis-index-array (In-Indexes Integer -> (Array Index)))
(define (axis-index-array ds k)
  (let* ([ds  (check-array-shape
               ds (λ () (raise-argument-error 'axis-index-array "(Vectorof Index)" 0 ds k)))]
         [dims  (vector-length ds)])
    (cond [(and (0 . <= . k) (k . < . dims))
           (unsafe-build-simple-array ds (λ: ([js : Indexes]) (unsafe-vector-ref js k)))]
          [else  (raise-argument-error 'axis-index-array (format "Index < ~a" dims) 1 ds k)])))

(: index-array (In-Indexes -> (Array Index)))
(define (index-array ds)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'index-array "(Vectorof Index)" ds)))])
    (unsafe-build-simple-array ds (λ: ([js : Indexes])
                                    (assert (unsafe-array-index->value-index ds js) index?)))))

(: indexes-array (In-Indexes -> (Array Indexes)))
(define (indexes-array ds)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'indexes-array "(Vectorof Index)" ds)))])
    (unsafe-build-simple-array ds (λ: ([js : Indexes]) (vector-copy-all js)))))

(: diagonal-array (All (A) (Integer Integer A A -> (Array A))))
(define (diagonal-array dims size on-value off-value)
  (cond [(not (index? dims))  (raise-argument-error 'diagonal-array "Index" 0 dims size)]
        [(not (index? size))  (raise-argument-error 'diagonal-array "Index" 1 dims size)]
        [else
         (define: ds : Indexes (make-vector dims size))
         ;; specialize for various cases
         (cond [(or (dims . <= . 1) (size . <= . 1))
                (unsafe-build-simple-array ds (λ: ([js : Indexes]) on-value))]
               [(= dims 2)
                (unsafe-build-simple-array
                 ds (λ: ([js : Indexes])
                      (define j0 (unsafe-vector-ref js 0))
                      (define j1 (unsafe-vector-ref js 1))
                      (if (= j0 j1) on-value off-value)))]
               [else
                (unsafe-build-simple-array
                 ds (λ: ([js : Indexes])
                      (define j0 (unsafe-vector-ref js 0))
                      (let: loop : A ([i : Nonnegative-Fixnum  1])
                        (cond [(i . >= . dims)  on-value]
                              [(= (unsafe-vector-ref js i) j0)  (loop (+ i 1))]
                              [else  off-value]))))])]))
