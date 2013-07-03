#lang racket/base
(provide wall-block-tall)
(require racket/draw racket/runtime-path)
(define-runtime-path wall-block-tall-img "wall-block-tall.png")
(define wall-block-tall (read-bitmap wall-block-tall-img))
