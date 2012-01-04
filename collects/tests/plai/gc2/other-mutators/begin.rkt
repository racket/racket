#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 7)

(define (go)
  (let ([obj 'z])
    2 3
    (symbol? obj)))

(go)
