; Ensure that call by value is correctly implemented.
#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 40)

(define global-val 'global)

(define (mut-arg arg)
  (set! arg 'mutated)
  0)

(mut-arg global-val)


(test/value=? global-val 'global)
