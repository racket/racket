#;
(
 TR opt: ternary-equality.rkt 12:0 (= 1 1 1) -- multi fixnum comp
 #t
)

#lang typed/racket/base
#:optimize

;; PR 12479
;; was incorrectly optimized in the same way as fixnum bitwise-and and co
(= 1 1 1)
