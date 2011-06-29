#lang racket/load

(module untyped racket
 (provide (all-defined-out))
 (struct a (v))
 (struct b a (v))
 (struct c (v) #:constructor-name c-maker)
 (struct d c (v) #:constructor-name d-maker)
 (define-struct e (v))
 (define-struct (f e) (v)))

(module typed typed/racket
 (require/typed 'untyped
  (struct a ((v : Integer)))
  (struct (b a) ((v : String)))
  (struct c ((v : Integer)) #:constructor-name c-maker)
  (struct (d c) ((v : String)) #:constructor-name d-maker)
  (struct e ((v : Integer)) #:extra-constructor-name make-e)
  (struct (f e) ((v : String)) #:extra-constructor-name make-f))

 (a 0)
 (b 1 "2")
 (c-maker 3)
 (d-maker 4 "5")
 (make-e 6)
 (make-f 7 "8")
 (e 9)
 (f 10 "11"))

(require 'typed)

