#lang typed/racket/base

(require "../../base.rkt"
         "../../flonum.rkt"
         "../parameters.rkt"
         "../unsafe.rkt"
         "../vector/vector-fft.rkt"
         "fcarray-struct.rkt"
         "array-struct.rkt"
         "array-transform.rkt"
         "for-each.rkt"
         "utils.rkt")

(provide array-axis-fft
         array-fft
         array-axis-inverse-fft
         array-inverse-fft)

;; Fast Fourier Transform

(: fcarray-last-axis-fft (FCArray -> FCArray))
(define (fcarray-last-axis-fft arr)
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (define k (- dims 1))
  (cond
    [(not (index? k))
     (raise-argument-error 'fcarray-last-axis-fft "FCArray with at least one axis" arr)]
    [else
     (define xs (fcarray-real-data arr))
     (define ys (fcarray-imag-data arr))
     (define dk (unsafe-vector-ref ds k))
     (define n (array-size arr))
     (define new-xs (make-flvector n))
     (define new-ys (make-flvector n))
     (for-each-array+data-index
      (unsafe-vector-remove ds k)
      (λ (js j)
        (define old-j (unsafe-fx* j dk))
        (flvector-fft! xs ys old-j (unsafe-fx+ old-j dk) new-xs new-ys old-j)))
     (unsafe-fcarray ds new-xs new-ys)]))

(: array-axis-fft ((Array Number) Integer -> (Array Float-Complex)))
(define (array-axis-fft arr k)
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (cond [(= dims 0)
         (raise-argument-error 'array-axis-fft "Array with at least one axis" 0 arr k)]
        [(or (0 . > . k) (k . >= . dims))
         (raise-argument-error 'array-axis-fft (format "Index less than ~a" dims) 1 arr k)]
        [(= k (- dims 1))
         (fcarray-last-axis-fft (array->fcarray arr))]
        [else
         (parameterize ([array-strictness #f])
           (array-axis-swap (fcarray-last-axis-fft (array->fcarray (array-axis-swap arr k (- dims 1))))
                            k (- dims 1)))]))

(: fcarray-fft (FCArray -> FCArray))
(define (fcarray-fft arr)
  (define dims (array-dims arr))
  (cond [(zero? dims)  (raise-argument-error 'fcarray-fft "FCArray with at least one axis" arr)]
        [(not (andmap power-of-two? (vector->list (array-shape arr))))
         (raise-argument-error 'fcarray-fft "FCArray with power-of-two shape" arr)]
        [else
         (define dims-1 (- dims 1))
         (cond [(zero? dims-1)  (fcarray-last-axis-fft arr)]
               [else
                (let loop ([#{k : Positive-Fixnum} 1] [arr  (array-axis-fft arr 0)])
                  (cond [(k . < . dims-1)  (loop (+ k 1) (array-axis-fft arr k))]
                        [else  (fcarray-last-axis-fft (array->fcarray arr))]))])]))

(: array-fft ((Array Number) -> FCArray))
(define (array-fft arr)
  (define dims (array-dims arr))
  (cond [(= dims 0)  (raise-argument-error 'array-fft "Array with at least one axis" arr)]
        [(not (andmap power-of-two? (vector->list (array-shape arr))))
         (raise-argument-error 'array-fft "Array with power-of-two shape" arr)]
        [else
         (fcarray-fft (array->fcarray arr))]))

;; ---------------------------------------------------------------------------------------------------
;; Inverse Fast Fourier Transform

(: array-axis-inverse-fft ((Array Number) Integer -> (Array Float-Complex)))
(define (array-axis-inverse-fft arr k)
  (parameterize ([dft-convention  (dft-inverse-convention)])
    (array-axis-fft arr k)))

(: array-inverse-fft ((Array Number) -> (Array Float-Complex)))
(define (array-inverse-fft arr)
  (parameterize ([dft-convention  (dft-inverse-convention)])
    (array-fft arr)))
