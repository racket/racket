#lang racket/base
(provide character-cat-girl)
(require racket/draw racket/runtime-path)
(define-runtime-path character-cat-girl-img "character-cat-girl.png")
(define character-cat-girl (read-bitmap character-cat-girl-img))
