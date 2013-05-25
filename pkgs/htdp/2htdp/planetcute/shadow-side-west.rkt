#lang racket/base
(provide shadow-side-west)
(require racket/draw racket/runtime-path)
(define-runtime-path shadow-side-west-img "shadow-side-west.png")
(define shadow-side-west (read-bitmap shadow-side-west-img))
