#lang racket/base
(provide wall-block)
(require racket/draw racket/runtime-path)
(define-runtime-path wall-block-img "wall-block.png")
(define wall-block (read-bitmap wall-block-img))
