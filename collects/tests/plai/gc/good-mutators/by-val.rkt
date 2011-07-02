; Ensure that call by value is correctly implemented.
#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 40)

(define global-val 'global)

(define (mut-arg arg)
  (set! arg 'mutated))

(mut-arg global-val)


(test/value=? global-val 'global)
