#lang racket/base
(provide brown-block)
(require racket/draw racket/runtime-path)
(define-runtime-path brown-block-img "Brown Block.png")
(define brown-block (read-bitmap brown-block-img))
