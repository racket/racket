#lang typed/scheme

(require
 scheme/fixnum)

;; Test the all the fixnum operations have been wrapped with types

;; We aren't really testing the semantics of the
;; operations. The checks are just to catch anything that is
;; really badly wrong.

(: check (All (a) ((a a -> Boolean) a a -> Boolean)))
;; Simple check function as RackUnit doesn't work in Typed Scheme (yet)
(define (check f a b)
  (if (f a b)
      #t
      (error (format "Check (~a ~a ~a) failed" f a b))))

(check = (fx+ 1 2) 3)
(check = (fx- 2 3) -1)
(check = (fx* 2 4) 8)
(check = (fxquotient 4 2) 2)
(check = (fxremainder 4 3) 1)
(check = (fxmodulo 10 3) 1)
(check = (fxabs -1) 1)
(check = (fxand 2 4) (bitwise-and 2 4))
(check = (fxior 2 4) (bitwise-ior 2 4))
(check = (fxxor 3 5) (bitwise-xor 3 5))
(check = (fxnot 4) (bitwise-not 4))
(check = (fxlshift 4 2) (arithmetic-shift 4 2))
(check = (fxrshift 32 2) (arithmetic-shift 32 -2))

(check equal? (fx= 1 1) #t)
(check equal? (fx< 2 4) #t)
(check equal? (fx> 4 2) #t)
(check equal? (fx<= 2 2) #t)
(check equal? (fx>= 4 4) #t)
(check equal? (fxmin 3 2) 2)
(check equal? (fxmax 3 4) 4)

