#lang racket/base
(provide dirt-block)
(require racket/draw racket/runtime-path)
(define-runtime-path dirt-block-img "Dirt Block.png")
(define dirt-block (read-bitmap dirt-block-img))
