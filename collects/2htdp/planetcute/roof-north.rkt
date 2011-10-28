#lang racket/base
(provide roof-north)
(require racket/draw racket/runtime-path)
(define-runtime-path roof-north-img "roof-north.png")
(define roof-north (read-bitmap roof-north-img))
