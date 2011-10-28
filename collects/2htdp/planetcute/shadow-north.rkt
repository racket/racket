#lang racket/base
(provide shadow-north)
(require racket/draw racket/runtime-path)
(define-runtime-path shadow-north-img "Shadow North.png")
(define shadow-north (read-bitmap shadow-north-img))
