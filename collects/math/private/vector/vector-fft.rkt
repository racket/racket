#lang typed/racket

(require racket/flonum
         racket/fixnum
         "../../parameters.rkt"
         "../../functions.rkt")

(provide vector-fft vector-fft!
         vector-inverse-fft vector-inverse-fft!)

;; Fast Fourier Transform

(: init-d (-> Integer))
(define (init-d)
  (define t (max 1 (max-math-threads)))
  (exact-ceiling (/ (log t) (log 2))))

(: vector-fft (case-> ((Vectorof Number) -> (Vectorof Float-Complex))
                      ((Vectorof Number) Integer -> (Vectorof Float-Complex))
                      ((Vectorof Number) Integer Integer -> (Vectorof Float-Complex))))
(define vector-fft
  (case-lambda
    [(as)    (vector-fft as 0 (vector-length as))]
    [(as n)  (vector-fft as 0 n)]
    [(as start end)
     (define: bs : (Vectorof Float-Complex) (make-vector (- end start) 0.0+0.0i))
     (vector-fft! as start end bs 0)
     bs]))

(: vector-fft! ((Vectorof Number) Integer Integer (Vectorof Float-Complex) Integer -> Void))
(define (vector-fft! as start end bs b-start)
  (define len (vector-length as))
  (define n (- end start))
  (cond
    [(= start end n 0)  (void)]
    [(or (start . < . 0) (start . >= . len))
     (raise-type-error 'vector-fft (format "Index < ~e" len) 1 as start end)]
    [(or (n . < . 0) (n . > . len))
     (raise-type-error 'vector-fft (format "Index <= ~e" len) 2 as start end)]
    [((- (vector-length bs) b-start) . < . n)
     (raise-type-error 'vector-fft! (format "Vector with length >= ~e" (+ n b-start))
                       3 as start end bs b-start)]
    [(not (power-of-two? n))
     (error 'vector-fft "expected power-of-2 length; given length ~e" n)]
    [else
     (define as-r (make-flvector n 0.0))
     (define as-i (make-flvector n 0.0))
     (let loop ([#{j : Nonnegative-Fixnum} 0])
       (when (j . < . n)
         (define a (vector-ref as (+ j start)))
         (flvector-set! as-r j (real->double-flonum (real-part a)))
         (flvector-set! as-i j (real->double-flonum (imag-part a)))
         (loop (+ j 1))))
     (define xs-r (make-flvector n 0.0))
     (define xs-i (make-flvector n 0.0))
     (define a (real->double-flonum (first (dft-convention))))
     (define b (real->double-flonum (second (dft-convention))))
     (vector-fft/depth as-r as-i xs-r xs-i b n 0 (init-d))
     (define c (flexpt (->fl n) (* 0.5 (- 1.0 a))))
     (let loop ([#{j : Nonnegative-Fixnum} 0])
       (when (j . < . n)
         (define r (/ (flvector-ref as-r j) c))
         (define i (/ (flvector-ref as-i j) c))
         (vector-set! bs (+ j b-start) (make-rectangular r i))
         (loop (+ j 1))))]))

(: decimate-in-time! (FlVector FlVector FlVector FlVector Integer Integer -> Void))
(define (decimate-in-time! as-r as-i xs-r xs-i n/2 start)
  (for ([i (in-range n/2)])
    (define si (+ start i))
    (define si2 (+ si i))
    (define si21 (+ si2 1))
    (define sin2 (+ si n/2))
    (flvector-set! xs-r si (flvector-ref as-r si2))
    (flvector-set! xs-i si (flvector-ref as-i si2))
    (flvector-set! xs-r sin2 (flvector-ref as-r si21))
    (flvector-set! xs-i sin2 (flvector-ref as-i si21))))

(: twiddle-factor! (FlVector FlVector Float Integer Integer -> Void))
(define (twiddle-factor! xs-r xs-i b n/2 start)
  (define c (/ (* b pi 0.0+1.0i) (->fl n/2)))
  (for ([k (in-range n/2)])
    (define k-start (+ k start))
    (define res (* (make-rectangular (flvector-ref xs-r k-start)
                                     (flvector-ref xs-i k-start))
                   (exp (* c (->fl k)))))
    (flvector-set! xs-r k-start (real-part res))
    (flvector-set! xs-i k-start (imag-part res))))

(: combine! (FlVector FlVector FlVector FlVector Integer Integer -> Void))
(define (combine! as-r as-i xs-r xs-i n/2 start)
  (for ([k (in-range n/2)])
    (define sk (+ start k))
    (define sk2 (+ sk n/2))
    (define br (flvector-ref xs-r sk))
    (define bi (flvector-ref xs-i sk))
    (define cr (flvector-ref xs-r sk2))
    (define ci (flvector-ref xs-i sk2))
    (flvector-set! as-r sk2 (- br cr))
    (flvector-set! as-i sk2 (- bi ci))
    (flvector-set! as-r sk (+ br cr))
    (flvector-set! as-i sk (+ bi ci))))

(: vector-fft/depth (FlVector FlVector FlVector FlVector Float Integer Integer Integer -> Void))
(define (vector-fft/depth as-r as-i xs-r xs-i b n start d)
  (unless (= n 1)
    (define n/2 (quotient n 2))
    (decimate-in-time! as-r as-i xs-r xs-i n/2 start)
    (cond [(= d 0)  (vector-fft/depth xs-r xs-i as-r as-i b n/2 start 0)
                    (vector-fft/depth xs-r xs-i as-r as-i b n/2 (+ start n/2) 0)
                    (twiddle-factor! xs-r xs-i b n/2 (+ start n/2))]
          [else
           (define bs (future (λ () (vector-fft/depth xs-r xs-i as-r as-i b n/2 start (- d 1)))))
           (define cs (future (λ ()
                                (vector-fft/depth xs-r xs-i as-r as-i b n/2 (+ start n/2) (- d 1))
                                (twiddle-factor! xs-r xs-i b n/2 (+ start n/2)))))
           (touch bs)
           (touch cs)])
    (combine! as-r as-i xs-r xs-i n/2 start)))

;; ---------------------------------------------------------------------------------------------------
;; Inverse Fast Fourier Transform

(: vector-inverse-fft (case-> ((Vectorof Number) -> (Vectorof Float-Complex))
                              ((Vectorof Number) Integer -> (Vectorof Float-Complex))
                              ((Vectorof Number) Integer Integer -> (Vectorof Float-Complex))))
(define vector-inverse-fft
  (case-lambda
    [(as)    (vector-fft as 0 (vector-length as))]
    [(as n)  (vector-fft as 0 n)]
    [(as start end)  (parameterize ([dft-convention  (dft-inverse-convention)])
                       (vector-fft as start end))]))

(: vector-inverse-fft! ((Vectorof Number) Integer Integer (Vectorof Float-Complex) Integer -> Void))
(define (vector-inverse-fft! as start end bs b-start)
  (parameterize ([dft-convention  (dft-inverse-convention)])
    (vector-fft! as start end bs b-start)))
