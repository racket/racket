#lang racket/base
(provide roof-north)
(require racket/draw racket/runtime-path)
(define-runtime-path roof-north-img "Roof North.png")
(define roof-north (read-bitmap roof-north-img))
