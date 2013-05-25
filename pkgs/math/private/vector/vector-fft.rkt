#lang typed/racket/base

(require racket/flonum
         racket/fixnum
         racket/list
         racket/future
         "../../base.rkt"
         "../../flonum.rkt"
         "../parameters.rkt"
         "../unsafe.rkt")

(provide vector-fft flvector-fft!
         vector-inverse-fft flvector-inverse-fft!)

;; Fast Fourier Transform

(: init-d (-> Integer))
(define (init-d)
  (define t (max 1 (max-math-threads)))
  (exact-ceiling (/ (log t) (log 2))))

(: vector-fft (case-> ((Vectorof Float-Complex) -> (Vectorof Float-Complex))
                      ((Vectorof Float-Complex) Integer -> (Vectorof Float-Complex))
                      ((Vectorof Float-Complex) Integer Integer -> (Vectorof Float-Complex))))
(define vector-fft
  (case-lambda
    [(as)    (vector-fft as 0 (vector-length as))]
    [(as n)  (vector-fft as 0 n)]
    [(as start end)
     (define n (vector-length as))
     (define as-r (for/flvector: #:length n ([a  (in-vector as)]) (real-part a)))
     (define as-i (for/flvector: #:length n ([a  (in-vector as)]) (imag-part a)))
     (define bs-r (make-flvector n))
     (define bs-i (make-flvector n))
     (flvector-fft! as-r as-i start end bs-r bs-i 0)
     (for/vector: #:length n ([x  (in-flvector bs-r)]
                              [y  (in-flvector bs-i)]) : Float-Complex
       (make-rectangular x y))]))

(: flvector-fft! (FlVector FlVector Integer Integer FlVector FlVector Integer -> Void))
(define (flvector-fft! as-r as-i start end bs-r bs-i b-start)
  (define len (min (flvector-length as-r) (flvector-length as-i)))
  (cond
    [(not (power-of-two? (- end start)))
     (error 'vector-fft "expected power-of-two length; given length ~e" (- end start))]
    [else
     (let ([as-r  (flvector-copy as-r start end)]
           [as-i  (flvector-copy as-i start end)])
       (define n (flvector-length as-r))
       (define xs-r (make-flvector n 0.0))
       (define xs-i (make-flvector n 0.0))
       (define a (real->double-flonum (first (dft-convention))))
       (define b (real->double-flonum (second (dft-convention))))
       (vector-fft/depth as-r as-i xs-r xs-i b n 0 (init-d))
       (define c (flexpt (->fl n) (* 0.5 (- 1.0 a))))
       (let loop ([#{j : Nonnegative-Fixnum} 0])
         (when (j . < . n)
           (define r (/ (unsafe-flvector-ref as-r j) c))
           (define i (/ (unsafe-flvector-ref as-i j) c))
           (let ([j  (+ j b-start)])
             (unsafe-flvector-set! bs-r j r)
             (unsafe-flvector-set! bs-i j i))
           (loop (+ j 1)))))]))

(: decimate-in-time! (FlVector FlVector FlVector FlVector Index Index -> Void))
(define (decimate-in-time! as-r as-i xs-r xs-i n/2 start)
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (when (i . < . n/2)
      (define si (+ start i))
      (define si2 (unsafe-fx+ si i))
      (define si21 (unsafe-fx+ si2 1))
      (define sin2 (unsafe-fx+ si n/2))
      (unsafe-flvector-set! xs-r si (unsafe-flvector-ref as-r si2))
      (unsafe-flvector-set! xs-i si (unsafe-flvector-ref as-i si2))
      (unsafe-flvector-set! xs-r sin2 (unsafe-flvector-ref as-r si21))
      (unsafe-flvector-set! xs-i sin2 (unsafe-flvector-ref as-i si21))
      (loop (+ i 1)))))

(: twiddle-factor! (FlVector FlVector Float Index Index -> Void))
(define (twiddle-factor! xs-r xs-i b n/2 start)
  (define c (/ (* b pi 0.0+1.0i) (->fl n/2)))
  (let loop ([#{k : Nonnegative-Fixnum} 0])
    (when (k . < . n/2)
      (define k-start (+ k start))
      (define res (* (make-rectangular (unsafe-flvector-ref xs-r k-start)
                                       (unsafe-flvector-ref xs-i k-start))
                     (exp (* c (->fl k)))))
      (unsafe-flvector-set! xs-r k-start (real-part res))
      (unsafe-flvector-set! xs-i k-start (imag-part res))
      (loop (+ k 1)))))

(: combine! (FlVector FlVector FlVector FlVector Index Index -> Void))
(define (combine! as-r as-i xs-r xs-i n/2 start)
  (let loop ([#{k : Nonnegative-Fixnum} 0])
    (when (k . < . n/2)
      (define sk (+ start k))
      (define sk2 (unsafe-fx+ sk n/2))
      (define br (unsafe-flvector-ref xs-r sk))
      (define bi (unsafe-flvector-ref xs-i sk))
      (define cr (unsafe-flvector-ref xs-r sk2))
      (define ci (unsafe-flvector-ref xs-i sk2))
      (unsafe-flvector-set! as-r sk2 (- br cr))
      (unsafe-flvector-set! as-i sk2 (- bi ci))
      (unsafe-flvector-set! as-r sk (+ br cr))
      (unsafe-flvector-set! as-i sk (+ bi ci))
      (loop (+ k 1)))))

(: vector-fft/depth (FlVector FlVector FlVector FlVector Float Index Index Integer -> Void))
(define (vector-fft/depth as-r as-i xs-r xs-i b n start d)
  (unless (= n 1)
    (define n/2 (quotient n 2))
    (decimate-in-time! as-r as-i xs-r xs-i n/2 start)
    (cond [(= d 0)  (define start+n/2 (assert (+ start n/2) index?))
                    (vector-fft/depth xs-r xs-i as-r as-i b n/2 start 0)
                    (vector-fft/depth xs-r xs-i as-r as-i b n/2 start+n/2 0)
                    (twiddle-factor! xs-r xs-i b n/2 start+n/2)]
          [else
           (define bs (future (λ () (vector-fft/depth xs-r xs-i as-r as-i b n/2 start (- d 1)))))
           (define cs (future (λ ()
                                (define start+n/2 (assert (+ start n/2) index?))
                                (vector-fft/depth xs-r xs-i as-r as-i b n/2 start+n/2 (- d 1))
                                (twiddle-factor! xs-r xs-i b n/2 start+n/2))))
           (touch bs)
           (touch cs)])
    (combine! as-r as-i xs-r xs-i n/2 start)))

;; ---------------------------------------------------------------------------------------------------
;; Inverse Fast Fourier Transform

(: vector-inverse-fft (case-> ((Vectorof Float-Complex) -> (Vectorof Float-Complex))
                              ((Vectorof Float-Complex) Integer -> (Vectorof Float-Complex))
                              ((Vectorof Float-Complex) Integer Integer -> (Vectorof Float-Complex))))
(define vector-inverse-fft
  (case-lambda
    [(as)    (vector-fft as 0 (vector-length as))]
    [(as n)  (vector-fft as 0 n)]
    [(as start end)  (parameterize ([dft-convention  (dft-inverse-convention)])
                       (vector-fft as start end))]))

(: flvector-inverse-fft! (FlVector FlVector Integer Integer FlVector FlVector Integer -> Void))
(define (flvector-inverse-fft! as-r as-i start end bs-r bs-i b-start)
  (parameterize ([dft-convention  (dft-inverse-convention)])
    (flvector-fft! as-r as-i start end bs-r bs-i b-start)))
