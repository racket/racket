#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.ss" 400)
(let ([f (λ (x) x)]) f)