#lang racket/base
(provide speechbubble)
(require racket/draw racket/runtime-path)
(define-runtime-path speechbubble-img "Speechbubble.png")
(define speechbubble (read-bitmap speechbubble-img))
