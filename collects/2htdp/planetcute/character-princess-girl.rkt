#lang racket/base
(provide character-princess-girl)
(require racket/draw racket/runtime-path)
(define-runtime-path character-princess-girl-img "character-princess-girl.png")
(define character-princess-girl (read-bitmap character-princess-girl-img))
