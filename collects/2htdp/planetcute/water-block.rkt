#lang racket/base
(provide water-block)
(require racket/draw racket/runtime-path)
(define-runtime-path water-block-img "Water Block.png")
(define water-block (read-bitmap water-block-img))
