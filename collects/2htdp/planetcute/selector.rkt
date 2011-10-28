#lang racket/base
(provide selector)
(require racket/draw racket/runtime-path)
(define-runtime-path selector-img "Selector.png")
(define selector (read-bitmap selector-img))
