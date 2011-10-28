#lang racket/base
(provide shadow-east)
(require racket/draw racket/runtime-path)
(define-runtime-path shadow-east-img "Shadow East.png")
(define shadow-east (read-bitmap shadow-east-img))
