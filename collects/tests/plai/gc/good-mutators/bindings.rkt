#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 58)

(define x 'intial)
(set! x 'final)
(test/value=? x 'final)


(define y
  (let ([outer-local
         (let ([inner-local 'value-expected])
           inner-local)])
    outer-local))

(test/value=? y 'value-expected)

(define (local-vars)
  (let ([x 23] [y 23])
    x))

(test/value=? (local-vars) 23)

(define (locals-2 x)
  (+ x 5))

(test/value=? (locals-2 23) 28)
