#lang racket/base
(provide heart)
(require racket/draw racket/runtime-path)
(define-runtime-path heart-img "heart.png")
(define heart (read-bitmap heart-img))
