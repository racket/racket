#lang typed/racket/base

(require racket/unsafe/ops
         "array-struct.rkt"
         "utils.rkt")

(provide const-array
         index-array
         indexes-array
         diagonal-array)

(: const-array (All (A) ((Listof Integer) A -> (lazy-array A))))
(define (const-array ds v)
  (let ([ds  (array-shape-safe->unsafe
              ds (λ () (raise-type-error 'const-array "(Listof Index) with Index product"
                                         0 ds v)))])
    (unsafe-lazy-array ds (λ (js) v))))

(: index-array ((Listof Integer) Integer -> (lazy-array Integer)))
(define (index-array ds i)
  (let ([ds  (array-shape-safe->unsafe
              ds (λ () (raise-type-error 'index-array "(Listof Index) with Index product"
                                         0 ds i)))])
    (define dims (vector-length ds))
    (cond [(and (0 . <= . i) (i . < . dims))
           (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) (unsafe-vector-ref js i)))]
          [else  (raise-type-error 'index-array (format "Index less than ~a" dims) 1 ds i)])))

(: indexes-array ((Listof Integer) -> (lazy-array (Listof Integer))))
(define (indexes-array ds)
  (let ([ds  (array-shape-safe->unsafe
              ds (λ () (raise-type-error 'indexes-array "(Listof Index) with Index product" ds)))])
    (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) (vector->list js)))))

(: diagonal-array (All (A) (Integer Integer A A -> (lazy-array A))))
(define (diagonal-array dims size on-value off-value)
  (cond [(not (index? dims))  (raise-type-error 'diagonal-array "Index" 0 dims size)]
        [(not (index? size))  (raise-type-error 'diagonal-array "Index" 1 dims size)]
        [else
         (define: ds : (Vectorof Index) (make-vector dims size))
         ;; specialize for various cases
         (cond [(or (dims . <= . 1) (size . <= . 1))
                (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) on-value))]
               [(= dims 2)
                (unsafe-lazy-array
                 ds (λ: ([js : (Vectorof Index)])
                      (define j0 (unsafe-vector-ref js 0))
                      (define j1 (unsafe-vector-ref js 1))
                      (if (= j0 j1) on-value off-value)))]
               [else
                (unsafe-lazy-array
                 ds (λ: ([js : (Vectorof Index)])
                      (define j0 (unsafe-vector-ref js 0))
                      (let: loop : A ([i : Nonnegative-Fixnum  1])
                        (cond [(i . >= . dims)  on-value]
                              [(= (unsafe-vector-ref js i) j0)  (loop (+ i 1))]
                              [else  off-value]))))])]))
