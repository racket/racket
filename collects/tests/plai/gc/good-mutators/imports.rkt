#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.ss" 20)

(import-primitives modulo)

(test/value=? (modulo 5 3) 2)

