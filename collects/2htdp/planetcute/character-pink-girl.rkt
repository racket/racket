#lang racket/base
(provide character-pink-girl)
(require racket/draw racket/runtime-path)
(define-runtime-path character-pink-girl-img "Character Pink Girl.png")
(define character-pink-girl (read-bitmap character-pink-girl-img))
