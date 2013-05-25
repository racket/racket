#lang racket/base
(provide character-horn-girl)
(require racket/draw racket/runtime-path)
(define-runtime-path character-horn-girl-img "character-horn-girl.png")
(define character-horn-girl (read-bitmap character-horn-girl-img))
