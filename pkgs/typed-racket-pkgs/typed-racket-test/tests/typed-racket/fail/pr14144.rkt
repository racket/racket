#;
(exn-pred #rx"expected: 'foo\n  given: 'bar")
#lang racket/load

;; Test for PR 14144
;; Make sure that the second definition is checked
;; against the synthesized type for the first definition

(require typed/racket)

(define x 'foo)
(define x 'bar)

