#lang racket/base
(provide wall-block)
(require racket/draw racket/runtime-path)
(define-runtime-path wall-block-img "Wall Block.png")
(define wall-block (read-bitmap wall-block-img))
