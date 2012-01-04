#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 5)

; 3
(define thunker
  (lambda ()
    ; 2
    'alligator
    ; 2
    'bananna
    ; 2
    'frog))
; 5 total

(thunker)
