#lang racket/base
(provide shadow-west)
(require racket/draw racket/runtime-path)
(define-runtime-path shadow-west-img "Shadow West.png")
(define shadow-west (read-bitmap shadow-west-img))
