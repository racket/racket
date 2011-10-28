#lang racket/base
(provide shadow-south-west)
(require racket/draw racket/runtime-path)
(define-runtime-path shadow-south-west-img "shadow-south-west.png")
(define shadow-south-west (read-bitmap shadow-south-west-img))
