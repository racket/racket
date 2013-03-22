#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 40)
(let ([x 1])
  ((λ (x)
     x)
   x))
