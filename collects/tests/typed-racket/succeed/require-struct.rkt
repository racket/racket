#lang racket/load

(module untyped racket
 (provide (all-defined-out))
 (struct a (v))
 (struct b a (v))
 (struct c (v) #:constructor-name c-maker)
 (struct d c (v) #:constructor-name d-maker)
 (define-struct e (v))
 (define-struct (f e) (v))
 (struct g (v) #:extra-constructor-name make-g)
 (struct h g (v) #:extra-constructor-name make-h))

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


(module typed2 typed/racket/base
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
(require 'typed2)


(module typed3 typed-scheme
 (require/typed 'untyped
  (struct a ((v : Integer)) #:constructor-name a)
  (struct (b a) ((v : String)) #:constructor-name b)
  (struct c ((v : Integer)) #:constructor-name c-maker)
  (struct (d c) ((v : String)) #:constructor-name d-maker)
  (struct e ((v : Integer)) #:extra-constructor-name make-e)
  (struct (f e) ((v : String)) #:extra-constructor-name make-f)
  (struct g ((v : Integer)))
  (struct (h g) ((v : String))))

 (a 0)
 (b 1 "2")
 (c-maker 3)
 (d-maker 4 "5")
 (make-e 6)
 (make-f 7 "8")
 (e 9)
 (f 10 "11")
 (make-g 12)
 (make-h 13 "14")
 (g 15)
 (h 16 "17"))

(require 'typed3)


(module typed4 typed/scheme
 (require/typed 'untyped
  (struct a ((v : Integer)) #:constructor-name a)
  (struct (b a) ((v : String)) #:constructor-name b)
  (struct c ((v : Integer)) #:constructor-name c-maker)
  (struct (d c) ((v : String)) #:constructor-name d-maker)
  (struct e ((v : Integer)) #:extra-constructor-name make-e)
  (struct (f e) ((v : String)) #:extra-constructor-name make-f)
  (struct g ((v : Integer)))
  (struct (h g) ((v : String))))

 (a 0)
 (b 1 "2")
 (c-maker 3)
 (d-maker 4 "5")
 (make-e 6)
 (make-f 7 "8")
 (e 9)
 (f 10 "11")
 (make-g 12)
 (make-h 13 "14")
 (g 15)
 (h 16 "17"))

(require 'typed4)


(module typed5 typed/scheme/base
 (require/typed 'untyped
  (struct a ((v : Integer)) #:constructor-name a)
  (struct (b a) ((v : String)) #:constructor-name b)
  (struct c ((v : Integer)) #:constructor-name c-maker)
  (struct (d c) ((v : String)) #:constructor-name d-maker)
  (struct e ((v : Integer)) #:extra-constructor-name make-e)
  (struct (f e) ((v : String)) #:extra-constructor-name make-f)
  (struct g ((v : Integer)))
  (struct (h g) ((v : String))))

 (a 0)
 (b 1 "2")
 (c-maker 3)
 (d-maker 4 "5")
 (make-e 6)
 (make-f 7 "8")
 (e 9)
 (f 10 "11")
 (make-g 12)
 (make-h 13 "14")
 (g 15)
 (h 16 "17"))

(require 'typed5)


(module typed6 typed/racket
  (require-typed-struct a ((v : Integer)) 'untyped)
  (require-typed-struct (b a) ((v : String)) 'untyped)
  (require-typed-struct c ((v : Integer)) #:constructor-name c-maker 'untyped)
  (require-typed-struct (d c) ((v : String)) #:constructor-name d-maker 'untyped)
  (require-typed-struct e ((v : Integer)) #:extra-constructor-name make-e 'untyped)
  (require-typed-struct (f e) ((v : String)) #:extra-constructor-name make-f 'untyped)

 (a 0)
 (b 1 "2")
 (c-maker 3)
 (d-maker 4 "5")
 (make-e 6)
 (make-f 7 "8")
 (e 9)
 (f 10 "11"))

(require 'typed6)


(module typed7 typed/scheme
  (require-typed-struct a ((v : Integer)) #:constructor-name a 'untyped)
  (require-typed-struct (b a) ((v : String)) #:constructor-name b 'untyped)
  (require-typed-struct c ((v : Integer)) #:constructor-name c-maker 'untyped)
  (require-typed-struct (d c) ((v : String)) #:constructor-name d-maker 'untyped)
  (require-typed-struct e ((v : Integer)) #:extra-constructor-name make-e 'untyped)
  (require-typed-struct (f e) ((v : String)) #:extra-constructor-name make-f 'untyped)
  (require-typed-struct g ((v : Integer)) 'untyped)
  (require-typed-struct (h g) ((v : String)) 'untyped)


 (a 0)
 (b 1 "2")
 (c-maker 3)
 (d-maker 4 "5")
 (make-e 6)
 (make-f 7 "8")
 (e 9)
 (f 10 "11")
 (make-g 12)
 (make-h 13 "14")
 (g 15)
 (h 16 "17"))

(require 'typed7)

(module typed8 typed-scheme
  (require-typed-struct a ((v : Integer)) #:constructor-name a 'untyped)
  (require-typed-struct (b a) ((v : String)) #:constructor-name b 'untyped)
  (require-typed-struct c ((v : Integer)) #:constructor-name c-maker 'untyped)
  (require-typed-struct (d c) ((v : String)) #:constructor-name d-maker 'untyped)
  (require-typed-struct e ((v : Integer)) #:extra-constructor-name make-e 'untyped)
  (require-typed-struct (f e) ((v : String)) #:extra-constructor-name make-f 'untyped)
  (require-typed-struct g ((v : Integer)) 'untyped)
  (require-typed-struct (h g) ((v : String)) 'untyped)


 (a 0)
 (b 1 "2")
 (c-maker 3)
 (d-maker 4 "5")
 (make-e 6)
 (make-f 7 "8")
 (e 9)
 (f 10 "11")
 (make-g 12)
 (make-h 13 "14")
 (g 15)
 (h 16 "17"))

(require 'typed8)

