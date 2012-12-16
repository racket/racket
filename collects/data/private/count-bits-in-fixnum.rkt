#lang racket/base
(require racket/unsafe/ops
         (for-syntax racket/base racket/fixnum racket/vector))
(provide fxpopcount)
;; Count set bits for 30 bit number in 5 steps.
;; for 62 bit number in 6.

(define-for-syntax lut
  #(#x2AAAAAAA
    #x0CCCCCCC
    #x30F0F0F0
    #x3F00FF00
    #x3FFF0000))

(define-for-syntax lut62
  #(#x2AAAAAAAAAAAAAAA
    #x0CCCCCCCCCCCCCCC
    #x30F0F0F0F0F0F0F0
    #x3F00FF00FF00FF00
    #x3FFF0000FFFF0000
    #x3FFFFFFF00000000))

(define-syntax (mk-fxpopcount stx)
  (syntax-case stx ()
    [(_ name)
     ;; Choose at compile time what word length is
     (let* ([lut (if (fixnum? (expt 2 61)) lut62 lut)]
            [flut (vector-map bitwise-not lut)])
       ;; Unroll the addition loop
       #`(define (name n)
           (unless (fixnum? n) (raise-argument-error 'name "fixnum?" 0 n))
           (let* #,(for/list ([m (in-vector lut)]
                              [f (in-vector flut)]
                              [b (in-naturals)])
                     #`[n (unsafe-fx+ (unsafe-fxrshift (unsafe-fxand n #,m)
                                                       #,(fxlshift 1 b))
                                      (unsafe-fxand n #,f))])
             n)))]))

(mk-fxpopcount fxpopcount)

