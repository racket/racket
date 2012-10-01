#lang typed/racket/base

(require racket/flonum
         racket/fixnum
         racket/list
         racket/future
         (only-in racket/math exact-ceiling pi)
         "../../base.rkt"
         "../../parameters.rkt"
         "../unsafe.rkt"
         "../exception.rkt"
         "fcvector.rkt")

(provide fcvector-fft fcvector-fft!
         fcvector-inverse-fft fcvector-inverse-fft!)

;; Fast Fourier Transform

(: init-d (-> Integer))
(define (init-d)
  (define t (max 1 (max-math-threads)))
  (exact-ceiling (/ (log t) (log 2))))

(: fcvector-fft (case-> (FCVector -> FCVector)
                        (FCVector Integer -> FCVector)
                        (FCVector Integer Integer -> FCVector)))
(define fcvector-fft
  (case-lambda
    [(as)    (fcvector-fft as 0 (fcvector-length as))]
    [(as n)  (fcvector-fft as 0 n)]
    [(as start end)
     (define: bs : FCVector (make-fcvector (- end start)))
     (fcvector-fft! as start end bs 0)
     bs]))

(: fcvector-fft! (FCVector Integer Integer FCVector Integer -> Void))
(define (fcvector-fft! as start end bs b-start)
  (define len (fcvector-length as))
  (define n (- end start))
  (cond
    [(not (power-of-two? n))
     (error 'fcvector-fft "expected power-of-two length; given length ~e" n)]
    [(or (start . < . 0) (start . >= . len))
     (raise-argument-error 'fcvector-fft (format "Index < ~e" len) 1 as start end)]
    [(or (n . < . 0) (n . > . len))
     (raise-argument-error 'fcvector-fft (format "Index <= ~e" len) 2 as start end)]
    [((- (fcvector-length bs) b-start) . < . n)
     (raise-argument-error 'fcvector-fft! (format "FCVector with length >= ~e" (+ n b-start))
                           3 as start end bs b-start)]
    [else
     (define as-r (make-flvector n 0.0))
     (define as-i (make-flvector n 0.0))
     (let loop ([#{j : Nonnegative-Fixnum} 0])
       (when (j . < . n)
         (define a (unsafe-fcvector-ref as (+ j start)))
         (unsafe-flvector-set! as-r j (real->double-flonum (real-part a)))
         (unsafe-flvector-set! as-i j (real->double-flonum (imag-part a)))
         (loop (+ j 1))))
     (define xs-r (make-flvector n 0.0))
     (define xs-i (make-flvector n 0.0))
     (define a (real->double-flonum (first (dft-convention))))
     (define b (real->double-flonum (second (dft-convention))))
     (fcvector-fft/depth as-r as-i xs-r xs-i b n 0 (init-d))
     (define c (flexpt (->fl n) (* 0.5 (- 1.0 a))))
     (let loop ([#{j : Nonnegative-Fixnum} 0])
       (when (j . < . n)
         (define r (/ (unsafe-flvector-ref as-r j) c))
         (define i (/ (unsafe-flvector-ref as-i j) c))
         (unsafe-fcvector-set! bs (+ j b-start) (make-rectangular r i))
         (loop (+ j 1))))]))

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

(: fcvector-fft/depth (FlVector FlVector FlVector FlVector Float Index Index Integer -> Void))
(define (fcvector-fft/depth as-r as-i xs-r xs-i b n start d)
  (unless (= n 1)
    (define n/2 (quotient n 2))
    (decimate-in-time! as-r as-i xs-r xs-i n/2 start)
    (cond [(= d 0)  (define start+n/2 (+ start n/2))
                    (with-asserts ([start+n/2 index?])
                      (fcvector-fft/depth xs-r xs-i as-r as-i b n/2 start 0)
                      (fcvector-fft/depth xs-r xs-i as-r as-i b n/2 start+n/2 0)
                      (twiddle-factor! xs-r xs-i b n/2 start+n/2))]
          [else
           (define bs (future (λ () (fcvector-fft/depth xs-r xs-i as-r as-i b n/2 start (- d 1)))))
           (define cs (future (λ ()
                                (define start+n/2 (+ start n/2))
                                (with-asserts ([start+n/2 index?])
                                  (fcvector-fft/depth xs-r xs-i as-r as-i b n/2 start+n/2 (- d 1))
                                  (twiddle-factor! xs-r xs-i b n/2 start+n/2)))))
           (touch bs)
           (touch cs)])
    (combine! as-r as-i xs-r xs-i n/2 start)))

;; ---------------------------------------------------------------------------------------------------
;; Inverse Fast Fourier Transform

(: fcvector-inverse-fft (case-> (FCVector -> FCVector)
                                (FCVector Integer -> FCVector)
                                (FCVector Integer Integer -> FCVector)))
(define fcvector-inverse-fft
  (case-lambda
    [(as)    (fcvector-fft as 0 (fcvector-length as))]
    [(as n)  (fcvector-fft as 0 n)]
    [(as start end)  (parameterize ([dft-convention  (dft-inverse-convention)])
                       (fcvector-fft as start end))]))

(: fcvector-inverse-fft! (FCVector Integer Integer FCVector Integer -> Void))
(define (fcvector-inverse-fft! as start end bs b-start)
  (parameterize ([dft-convention  (dft-inverse-convention)])
    (fcvector-fft! as start end bs b-start)))
