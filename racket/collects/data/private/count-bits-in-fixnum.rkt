#lang racket/base
(require racket/unsafe/ops
         (for-syntax racket/base racket/fixnum))
(provide fxpopcount
         unsafe-fxpopcount*)
;; Count set bits for 30 bit number in 5 steps.
;; for 62 bit number in 6, for 8 bit numbers in 3

(define-for-syntax lut30
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

(define-for-syntax lut8
  #(#xAA
    #xCC
    #xF0))

(define-syntax (define-fxpopcount stx)
  (syntax-case stx ()
    [(_ name bits)
     #'(define (name n)
         (unless (fixnum? n) (raise-argument-error 'name "fixnum?" 0 n))
         (unsafe-fxpopcount* n bits))]))

(define-syntax (unsafe-fxpopcount* stx)
  (syntax-case stx ()
    [(_ expr bits0)
     ;; Choose at compile time what word length is
     (let* ([bits (syntax-e #'bits0)]
            [bits (or bits 
                      ;; for portable bytecode, use a fixnum size tha
                      ;; always works:
                      30)]
            [lut
             (cond [(<= bits 8) lut8]
                   [(<= bits 30) lut30]
                   [(<= bits 62) lut62]
                   [else (raise-syntax-error "bit width too big" stx #'bits0)])])
       ;; Unroll the addition loop
       #`(let ([n expr])
           (let* #,(for/list ([m (in-vector lut)]
                              [b (in-naturals)])
                     (define f (bitwise-not m))
                     #`[n (unsafe-fx+ (unsafe-fxrshift (unsafe-fxand n #,m)
                                                       #,(fxlshift 1 b))
                                      (unsafe-fxand n #,f))])
             n)))]))

(define-fxpopcount fxpopcount #f)
