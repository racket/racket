#lang racket/base

(require racket/contract)

;; This test checks that names of records using ~ can be dumped successfully
;; The contracted procedure is here because the rumble implementation has a
;; ~ in the name.

(module test info
  ;; Because `dump-memory-stats` writes to stderr:
  (define ignore-stderr #rx".*"))

(define foo
  (contract (-> #:x any/c any/c)
            (λ (#:x x) 0)
            'pos
            'neg))
(struct ~s ( x))

(define v (~s 1))

(dump-memory-stats)

v
