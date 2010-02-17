#lang plai
(define-type A
  [mta]
  [a (b B?)])

(define-type B
  [mtb]
  [b (a A?)])

(define-type T
  [i (f number?)])

(i 4)
(test/exn (make-i #f) "contract")
(test/exn (i-f #f) "contract")

(define-type T1
  [i1 (f (car 1))])

(type-case A (mta)
           [mta () 1]
           [a (x) 2])

(define-type DefrdSub
  [mtSub]
  [aSub (value boolean?)])

(define (lookup ds the-name)
  (type-case DefrdSub ds
             [mtSub () 1]
             [aSub (a-name) 2]))

(define-type t (c))
(test/exn
 (type-case t (list 1) (c () 1))
 "expected")

(define-type t1 (c1 (n number?)))
(test (c1 'not-a-number) (list 5))