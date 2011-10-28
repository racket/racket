#lang racket/base
(provide shadow-north-east)
(require racket/draw racket/runtime-path)
(define-runtime-path shadow-north-east-img "Shadow North East.png")
(define shadow-north-east (read-bitmap shadow-north-east-img))
