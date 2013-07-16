#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 10)
(let ([f (λ (x) x)]) f)
