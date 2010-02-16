#lang plai/mutator
; Is else defined?
(allocator-setup "../good-collectors/good-collector.ss" 40)

(test/value=? (cond [else 28935723]) 28935723)