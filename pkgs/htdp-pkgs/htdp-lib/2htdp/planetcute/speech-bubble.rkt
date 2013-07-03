#lang racket/base
(provide speech-bubble)
(require racket/draw racket/runtime-path)
(define-runtime-path speech-bubble-img "speech-bubble.png")
(define speech-bubble (read-bitmap speech-bubble-img))
