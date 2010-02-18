#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.ss" 10)
(let ([f (λ (x) x)]) f)