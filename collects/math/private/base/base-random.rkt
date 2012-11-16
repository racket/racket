#lang typed/racket/base

(require racket/fixnum)

(provide random-bits random-natural random-integer)

;; Random bits are taken in blocks of this size:
(define block-bits 30)
(define block-size (arithmetic-shift 1 block-bits))

(: random-bits (Integer -> Natural))
(define (random-bits bits)
  (cond [(bits . <= . 0)  (raise-argument-error 'random-bits "Positive-Integer" bits)]
        [else
         (define max-blocks (assert (quotient bits block-bits) index?))
         (define rem-bits (remainder bits block-bits))
         (let: loop : Natural ([blocks : Nonnegative-Fixnum  0]
                               [r : Natural  (random (fxlshift 1 rem-bits))])
           (cond [(blocks . fx< . max-blocks)
                  (loop (fx+ blocks 1)
                        (bitwise-ior (arithmetic-shift r block-bits)
                                     (random block-size)))]
                 [else  r]))]))

(define random-max 4294967087)
(define bias-bits (* 2 block-bits))

(: random-natural (Integer -> Natural))
;; Returns a random integer in the interval [0..n)
(define (random-natural n)
  (cond
    [(n . <= . 0)  (raise-argument-error 'random-natural "Positive-Integer" n)]
    [(n . <= . random-max)  (random n)]
    [else
     ;; Rejection sampling has rejection probability approaching 1/2 in the worst cases; that is,
     ;; when n = 1+2^i for some large-ish integer i
     ;; Adding extra bits shrinks the rejection probability to near zero (it approaches
     ;; (* 1/2 (expt 2 (- bias-bits)))), at the cost of some bias
     ;; The bias starts become detectable after taking (expt 2 bias-bits) samples, which is plenty
     (define bits (+ bias-bits (integer-length (- n 1))))
     (define m (arithmetic-shift 1 bits))
     (let loop ()
       (define r (quotient (* (+ (random-bits bits) 1) n) m))
       (if (r . >= . n) (loop) r))]))

(: random-integer (Integer Integer -> Integer))
(define (random-integer a b)
  (let ([a  (min a b)] [b  (max a b)])
    (+ a (random-natural (- b a)))))
