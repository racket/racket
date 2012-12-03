#lang typed/racket/base

;; Return random samples from a normal distribution using the Box-Muller transform

(require racket/fixnum
         "../../../flonum.rkt"
         "../../../base.rkt"
         "../../unsafe.rkt")

(provide flnormal-sample)

;; Leaving these in, in case we discover in the future that it's actually important for them
;; to be accurate
#|
(: flsin2pix (Flonum -> Flonum))
;; Computes sin(2*pi*x) accurately in the range [0,1]
(define (flsin2pix x)
  (let*-values ([(x s)  (if (x . fl> . 0.5) (values (fl- x 0.5) -1.0) (values x 1.0))]
                [(x)    (if (x . fl> . 0.25) (fl- 0.5 x) x)])
    (fl* s (flsin (fl* (fl* 2.0 pi) x)))))

(: flcos2pix (Flonum -> Flonum))
;; Computes cos(2*pi*x) accurately in the range [0,1]
(define (flcos2pix x)
  (let*-values ([(x)  (if (x . fl> . 0.5) (fl- 1.0 x) x)]
                [(x s)  (if (x . fl> . 0.25) (values (fl- 0.5 x) 1.0) (values x -1.0))])
    (fl* s (flsin (fl* (fl* 2.0 pi) (fl- x 0.25))))))
|#

(: nonzero-random (-> Flonum))
(define (nonzero-random)
  (let ([u  (random)])
    (if (fl= u 0.0) (nonzero-random) u)))

(: flnormal-sample (Flonum Flonum Integer -> FlVector))
;; The Box-Muller method has an bad reputation, but undeservedly:
;;  1. There's nothing unstable about the floating-point implementation of the transform
;;  2. It has good tail behavior (i.e. it can return very unlikely numbers)
;;  3. With today's FPUs, there's no need to worry about computing `log' and `sin' (sheesh)
;; Points in favor: it's simple and fast
(define (flnormal-sample c s n)
  (cond [(not (index? n))  (raise-argument-error 'flnormal-sample "Natural" 2 c s n)]
        [else
         (define xs (make-flvector n))
         (cond
           [(fx= n 0)  xs]
           [else
            (define n-1 (fx- n 1))
            (let loop ([#{i : Nonnegative-Fixnum} 0])
              (cond [(i . fx< . n-1)
                     (define u1 (nonzero-random))
                     (define u2 (random))
                     (define t (flsqrt (fl* -2.0 (fllog u1))))
                     (define z (fl* (fl* 2.0 pi) u2))
                     (define x (fl* t (flcos z)))
                     (define y (fl* t (flsin z)))
                     (unsafe-flvector-set! xs i (fl+ c (fl* s x)))
                     (unsafe-flvector-set! xs (fx+ i 1) (fl+ c (fl* s y)))
                     (loop (fx+ i 2))]
                    [(i . fx= . n-1)
                     (define u1 (nonzero-random))
                     (define u2 (random))
                     (define x (fl* (flsqrt (fl* -2.0 (fllog u1)))
                                    (flsin (fl* (fl* 2.0 pi) u2))))
                     (unsafe-flvector-set! xs i (fl+ c (fl* s x)))
                     xs]
                    [else
                     xs]))])]))
