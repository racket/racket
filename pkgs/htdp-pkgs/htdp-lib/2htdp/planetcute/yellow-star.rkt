#lang racket/base
(provide yellow-star)
(require racket/draw racket/runtime-path)
(define-runtime-path yellow-star-img "yellow-star.png")
(define yellow-star (read-bitmap yellow-star-img))
