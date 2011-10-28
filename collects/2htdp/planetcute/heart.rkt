#lang racket/base
(provide heart)
(require racket/draw racket/runtime-path)
(define-runtime-path heart-img "Heart.png")
(define heart (read-bitmap heart-img))
