#lang typed/racket/base
(require (only-in racket/math pi)
         (only-in racket/future future touch)
         racket/fixnum racket/flonum)

(: decimate-in-time
   (FlVector FlVector
    FlVector FlVector 
    Integer Integer
    -> Void))
(define (decimate-in-time as-r as-i 
                          xs-r xs-i 
                          n/2 start)
  (for ([i (in-range n/2)])
    (define si (+ start i))
    (define si2 (+ si i))
    (define si21 (+ si2 1))
    (define sin2 (+ si n/2))
    (flvector-set! 
     xs-r si (flvector-ref as-r si2))
    (flvector-set! 
     xs-i si (flvector-ref as-i si2))
    (flvector-set! 
     xs-r sin2 (flvector-ref as-r si21))
    (flvector-set!
     xs-i sin2 (flvector-ref as-i si21))))

(: twiddle-factor 
   (FlVector FlVector 
    Integer Integer -> Void))
(define (twiddle-factor cs-r cs-i
                        n/2 start)
  (define c (/ (* pi 0.0+1.0i) (->fl n/2)))
  (for ([k (in-range n/2)])
    (define k-start (+ k start))
    (define res
      (* (make-rectangular
          (flvector-ref cs-r k-start)
          (flvector-ref cs-i k-start))
         (exp (* c (->fl k)))))
    (flvector-set! cs-r k-start 
                   (real-part res))
    (flvector-set! cs-i k-start
                   (imag-part res))))

(: fft/depth
   (FlVector FlVector FlVector FlVector
    Integer Integer Integer
    -> Void))
(define (fft/depth as-r as-i xs-r xs-i
                    n start d)
  (unless (= n 1)
    (define n/2 (quotient n 2))
    (decimate-in-time as-r as-i xs-r
                      xs-i n/2 start)
    (cond
      [(= d 0)
       (fft/depth xs-r xs-i as-r as-i
                   n/2 start 0)
       (fft/depth xs-r xs-i as-r as-i
                   n/2 (+ start n/2) 0)
       (twiddle-factor xs-r xs-i n/2 
                       (+ start n/2))]
      [else
       (define bs
         (future
          (λ ()
            (fft/depth xs-r xs-i as-r 
                        as-i n/2 start
                        (- d 1)))))
       (define cs
         (future
          (λ ()
            (fft/depth xs-r xs-i as-r 
                        as-i n/2 
                        (+ start n/2) (- d 1))
            (twiddle-factor xs-r xs-i n/2
                            (+ start n/2)))))
       (touch bs)
       (touch cs)])
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
      (flvector-set! as-i sk (+ bi ci)))))

(: run-fft : (Listof Float-Complex)
   -> (values (Listof Any) Integer Integer Integer))
(define (run-fft l)
  (define as-r (apply flvector (map real-part l)))
  (define as-i (apply flvector (map imag-part l)))
  (define n (flvector-length as-r))
  (define xs-r (make-flvector n 0.0))
  (define xs-i (make-flvector n 0.0))
  (collect-garbage) (collect-garbage) (collect-garbage)
  (collect-garbage) (collect-garbage) (collect-garbage)
  ((inst time-apply Void FlVector FlVector FlVector FlVector
                    Integer Integer Integer)
   (λ () 
     (let ([f (future (λ () 
                        (fft/depth as-r as-i xs-r xs-i n 0 0)))]) 
       (sleep 0.1) 
       (touch f))) 
   '()))

(define: input : (Listof Float-Complex) 
  (for/list ([t 1048576]) 
    (define in (real->double-flonum (* t (/ (* 2 pi) 500))))
    (make-rectangular
     (flsin (if (= in 0) 0.0 in))
     (flcos (if (= in 0) 0.0 in)))))
(run-fft input)


