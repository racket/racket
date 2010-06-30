#lang racket/base
(require "view/view.rkt")
(provide expand/step)

(define (expand/step stx)
  (go stx))
