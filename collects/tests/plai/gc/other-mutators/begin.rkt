#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 6)

(define (go)
  (let ([obj 'z])
    2 3
    (symbol? obj)))

(go)
