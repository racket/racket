#lang racket/base
(provide character-cat-girl)
(require racket/draw racket/runtime-path)
(define-runtime-path character-cat-girl-img "Character Cat Girl.png")
(define character-cat-girl (read-bitmap character-cat-girl-img))
