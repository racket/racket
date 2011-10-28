#lang racket/base
(provide chest-closed)
(require racket/draw racket/runtime-path)
(define-runtime-path chest-closed-img "Chest Closed.png")
(define chest-closed (read-bitmap chest-closed-img))
