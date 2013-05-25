#lang racket/base
(provide grass-block)
(require racket/draw racket/runtime-path)
(define-runtime-path grass-block-img "grass-block.png")
(define grass-block (read-bitmap grass-block-img))
