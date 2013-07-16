#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 20)

(import-primitives modulo)

(test/value=? (modulo 5 3) 2)
