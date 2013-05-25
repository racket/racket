#lang plai/gc2/mutator
; This is `classic' in that it caught many bugs in copying collectors that students wrote for CS173, Fall 2007.
(allocator-setup "../good-collectors/good-collector.rkt" 28)

'trash
'junk
; after GC, alpha beta are copied but the cons references them in the old semispace
(define my-pair (cons 'alpha 'beta))
; we have room for our-pair, but 'refuse forces a semi-space swap that exposes
; the memory corruption (if one exists)
'refuse
(define our-pair (cons my-pair my-pair))
(test/value=? our-pair '((alpha . beta) . (alpha . beta)))
(test/location=? (first our-pair) (rest our-pair))
