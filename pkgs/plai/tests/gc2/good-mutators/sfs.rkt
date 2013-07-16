#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 7)
(let ([x (cons 1 2)])
 (let ([y (cons 3 4)])
   y))
