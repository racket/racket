#lang typed/racket/base

(require racket/unsafe/ops
         "../vector/vector-fft.rkt"
         "array-struct.rkt"
         "array-transform.rkt"
         "../../parameters.rkt"
         "../../functions.rkt"
         "for-each.rkt"
         "utils.rkt")

(provide array-axis-fft
         array-fft
         array-axis-inverse-fft
         array-inverse-fft)

;; Fast Fourier Transform

(: array-last-axis-fft ((Array Number) -> (lazy-array Float-Complex)))
(define (array-last-axis-fft arr)
  (define ds (unsafe-array-shape arr))
  (define dims (vector-length ds))
  (define k (- dims 1))
  (cond
    [(not (index? k))  (raise-type-error 'array-last-axis-fft "Array with at least one axis" arr)]
    [else
     (let ([arr  (array-strict arr)])
       (define vs (unsafe-array-data arr))
       (define dk (vector-ref ds k))
       (define: new-vs : (Vectorof Float-Complex) (make-vector (array-size arr) 0.0+0.0i))
       (define new-ds (unsafe-vector-remove ds k))
       (for-each-array+data-index
        new-ds (λ (js j)
                 (define old-j (unsafe-fx* j dk))
                 (vector-fft! vs old-j (unsafe-fx+ old-j dk) new-vs old-j)))
       (array-lazy (unsafe-strict-array ds new-vs)))]))

(: array-axis-fft ((Array Number) Integer -> (lazy-array Float-Complex)))
(define (array-axis-fft arr k)
  (define ds (unsafe-array-shape arr))
  (define dims (vector-length ds))
  (cond [(= dims 0)
         (raise-type-error 'array-axis-fft "Array with at least one axis" 0 arr k)]
        [(or (0 . > . k) (k . >= . dims))
         (raise-type-error 'array-axis-fft (format "Index less than ~a" dims) 1 arr k)]
        [(= k (- dims 1))
         (array-last-axis-fft arr)]
        [else
         (array-axis-swap (array-last-axis-fft (array-axis-swap arr k (- dims 1))) k (- dims 1))]))

(: array-fft ((Array Number) -> (lazy-array Float-Complex)))
(define (array-fft arr)
  (define dims (array-dims arr))
  (cond [(= dims 0)  (raise-type-error 'array-fft "Array with at least one axis" arr)]
        [(not (andmap power-of-two? (array-shape arr)))
         (raise-type-error 'array-fft "Array with power-of-two shape" arr)]
        [else
         (let loop ([#{k : Nonnegative-Fixnum} 1] [arr  (array-axis-fft arr 0)])
           (cond [(k . < . dims)  (loop (+ k 1) (array-axis-fft arr k))]
                 [else  arr]))]))

;; ---------------------------------------------------------------------------------------------------
;; Inverse Fast Fourier Transform

(: array-axis-inverse-fft ((Array Number) Integer -> (lazy-array Float-Complex)))
(define (array-axis-inverse-fft arr k)
  (parameterize ([dft-convention  (dft-inverse-convention)])
    (array-axis-fft arr k)))

(: array-inverse-fft ((Array Number) -> (lazy-array Float-Complex)))
(define (array-inverse-fft arr)
  (parameterize ([dft-convention  (dft-inverse-convention)])
    (array-fft arr)))
