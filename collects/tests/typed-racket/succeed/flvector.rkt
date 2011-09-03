#lang typed/scheme

(require
 scheme/flonum
 scheme/unsafe/ops)

;; Test the all the flvector operations have been wrapped with types

;; We aren't really testing the semantics of the
;; operations. The checks are just to catch anything that is
;; really badly wrong.

(: check (All (a) ((a a -> Boolean) a a -> Boolean)))
;; Simple check function as RackUnit doesn't work in Typed Scheme (yet)
(define (check f a b)
  (if (f a b)
      #t
      (error (format "Check (~a ~a ~a) failed" f a b))))

;; Check the FlVector type is exported
(define: v : FlVector (flvector 1. 2. 3.))
(define-struct: Foo ([vec : FlVector]) #:transparent)

(check equal? (flvector 1. 2. 3. 4.) (flvector 1. 2. 3. 4.))
(check equal? (flvector? (flvector 1. 2. 3.)) #t)
(check equal? (make-flvector 3 3.0) (flvector 3. 3. 3.))
(check = (flvector-length (flvector 1. 2. 3.)) 3)
(check = (flvector-ref (flvector 1. 2. 3.) 0) 1.)
(check =
       (let ([v (flvector 1. 2. 3.)])
         (flvector-set! v 0 10.)
         (flvector-ref v 0))
       10.)

;; Unsafe operations

(check = (unsafe-flvector-length (flvector 1. 2. 3.)) 3)
(check = (unsafe-flvector-ref (flvector 1. 2. 3.) 0) 1.)
(check =
       (let ([v (flvector 1. 2. 3.)])
         (unsafe-flvector-set! v 0 10.)
         (unsafe-flvector-ref v 0))
       10.)
