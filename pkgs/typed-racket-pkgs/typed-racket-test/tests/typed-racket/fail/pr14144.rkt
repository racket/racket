#;
(exn-pred #rx"Expected 'foo, but got 'bar")
#lang racket/load

;; Test for PR 14144
;; Make sure that the second definition is checked
;; against the synthesized type for the first definition

(require typed/racket)

(define x 'foo)
(define x 'bar)

