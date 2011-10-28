#lang racket/base
(provide plain-block)
(require racket/draw racket/runtime-path)
(define-runtime-path plain-block-img "Plain Block.png")
(define plain-block (read-bitmap plain-block-img))
