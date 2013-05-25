#lang racket/base
(provide stone-block)
(require racket/draw racket/runtime-path)
(define-runtime-path stone-block-img "stone-block.png")
(define stone-block (read-bitmap stone-block-img))
