#lang racket/base
(provide shadow-north)
(require racket/draw racket/runtime-path)
(define-runtime-path shadow-north-img "shadow-north.png")
(define shadow-north (read-bitmap shadow-north-img))
