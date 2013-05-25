#lang plai/gc2/mutator
; Is else defined?
(allocator-setup "../good-collectors/good-collector.rkt" 40)

(test/value=? (cond [else 28935723]) 28935723)
