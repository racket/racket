#lang racket/base
(require "base-5-lib.rkt")

;; Same name as in "base-5-lib.rkt":
(define unexported-five "five")
(define s (quote-syntax unexported-five))

(eval-syntax s)
