#lang racket/base
(provide chest-lid)
(require racket/draw racket/runtime-path)
(define-runtime-path chest-lid-img "Chest Lid.png")
(define chest-lid (read-bitmap chest-lid-img))
