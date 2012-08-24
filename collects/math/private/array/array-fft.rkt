#lang typed/racket/base

(require "../unsafe.rkt"
         "../exception.rkt"
         "../vector/fcvector.rkt"
         "../vector/fcvector-fft.rkt"
         "array-struct.rkt"
         "array-transform.rkt"
         "fcarray.rkt"
         "../../parameters.rkt"
         "../../functions.rkt"
         "for-each.rkt"
         "utils.rkt")

(provide array-axis-fft
         array-fft
         fcarray-fft
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
     (define vs (fcarray-data arr))
     (define dk (unsafe-vector-ref ds k))
     (define: new-vs : FCVector (make-fcvector (array-size arr)))
     (for-each-array+data-index
      (unsafe-vector-remove ds k)
      (λ (js j)
        (define old-j (unsafe-fx* j dk))
        (fcvector-fft! vs old-j (unsafe-fx+ old-j dk) new-vs old-j)))
     (unsafe-fcarray ds new-vs)]))

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
         (array-axis-swap (fcarray-last-axis-fft (array->fcarray (array-axis-swap arr k (- dims 1))))
                          k (- dims 1))]))

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
