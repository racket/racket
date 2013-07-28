#lang racket

;; Test various (begin ...)s at the top-level. In
;; particular, avoid a (begin) regression.

(define ns (make-base-namespace))
(eval '(require typed/racket) ns)
(eval '(begin) ns)
(eval '(begin 1 2) ns)
(eval '(begin (+ 1 2) 5 3 "foo") ns)

