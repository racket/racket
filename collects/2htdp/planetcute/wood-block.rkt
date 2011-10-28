#lang racket/base
(provide wood-block)
(require racket/draw racket/runtime-path)
(define-runtime-path wood-block-img "Wood Block.png")
(define wood-block (read-bitmap wood-block-img))
