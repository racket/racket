#lang racket/base
(provide chest-open)
(require racket/draw racket/runtime-path)
(define-runtime-path chest-open-img "chest-open.png")
(define chest-open (read-bitmap chest-open-img))
