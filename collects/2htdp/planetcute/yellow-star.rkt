#lang racket/base
(provide yellow-star)
(require racket/draw racket/runtime-path)
(define-runtime-path yellow-star-img "Yellow Star.png")
(define yellow-star (read-bitmap yellow-star-img))
