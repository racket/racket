#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 28)

(halt-on-errors #t)
(test/value=? 12 12)
