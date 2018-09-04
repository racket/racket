#lang racket/base
(require "check.rkt"
         "sandman-struct.rkt")

(provide the-sandman
         current-sandman)

(define the-sandman #f)

;; in atomic mode
(define/who current-sandman
  (case-lambda
    [() the-sandman]
    [(sm)
     (check who sandman? sm)
     (set! the-sandman sm)]))
