#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 10)

; 3
(define (go)
  ; 2
  (let ([obj 'z])
    9 10
    ; 3
    (symbol? obj)))

; 2
(go)
