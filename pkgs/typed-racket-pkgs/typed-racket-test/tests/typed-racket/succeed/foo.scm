#lang scheme/load
(module m mzscheme
  (define x 3)
  (define (y z) (add1 z))
  (provide (all-defined)))

(module bang-tests typed-scheme
  (define: x : Number 1)
  x
  (provide x)
  (set! x 4)
  (when #t 3))


(module trequire typed-scheme
  (require 'bang-tests)
  (define: y : Number x))

(module require-tests typed-scheme
  (provide z)
  (require/typed 'm (x Number))
  (+ x 3)
  (require/typed 'm (y (Number -> Number)))
  (define: z : Number (y (+ x 4))))


(module provide-type typed-scheme
  (define-type-alias top2 Any)

  (define-typed-struct (a) container ([v : a]))

  (container-v (make-container 3))

  (provide top2 container container-v make-container)
  )

(module require-type typed-scheme
  (require 'provide-type)

  (let: ([x : top2 3])
        x)

  (define: (f [x : (container Number)]) : Number
    (container-v x))

  (f (make-container (ann 7 : Number)))

  )
