#lang racket/base
(provide character-boy)
(require racket/draw racket/runtime-path)
(define-runtime-path character-boy-img "Character Boy.png")
(define character-boy (read-bitmap character-boy-img))
