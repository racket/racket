#lang racket/base
(provide shadow-north-west)
(require racket/draw racket/runtime-path)
(define-runtime-path shadow-north-west-img "shadow-north-west.png")
(define shadow-north-west (read-bitmap shadow-north-west-img))
