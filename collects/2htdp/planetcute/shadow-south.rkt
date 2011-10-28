#lang racket/base
(provide shadow-south)
(require racket/draw racket/runtime-path)
(define-runtime-path shadow-south-img "shadow-south.png")
(define shadow-south (read-bitmap shadow-south-img))
