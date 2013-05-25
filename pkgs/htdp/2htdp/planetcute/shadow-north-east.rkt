#lang racket/base
(provide shadow-north-east)
(require racket/draw racket/runtime-path)
(define-runtime-path shadow-north-east-img "shadow-north-east.png")
(define shadow-north-east (read-bitmap shadow-north-east-img))
