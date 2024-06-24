#lang racket/base

;; Make sure assignment to unused is pruned, while
;; assumed to used is preserved

(define unused (printf "unused!\n"))
(define went-away "went away")
(set! unused went-away)

(define (called-later)
  used)
(define used (printf "used!\n"))
(define stayed "stayed")
(set! used stayed)

(called-later)
