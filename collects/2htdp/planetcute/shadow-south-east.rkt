#lang racket/base
(provide shadow-south-east)
(require racket/draw racket/runtime-path)
(define-runtime-path shadow-south-east-img "Shadow South East.png")
(define shadow-south-east (read-bitmap shadow-south-east-img))
