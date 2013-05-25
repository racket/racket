#lang racket/base
(provide shadow-west)
(require racket/draw racket/runtime-path)
(define-runtime-path shadow-west-img "shadow-west.png")
(define shadow-west (read-bitmap shadow-west-img))
