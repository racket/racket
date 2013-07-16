#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 4)

; 2
(define thunker
  (lambda ()
    ; 2
    'alligator
    ; 2
    'bananna
    ; 2
    'frog))
; 4 total

(thunker)
