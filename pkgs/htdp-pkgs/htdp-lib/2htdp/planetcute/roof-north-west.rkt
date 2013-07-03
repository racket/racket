#lang racket/base
(provide roof-north-west)
(require racket/draw racket/runtime-path)
(define-runtime-path roof-north-west-img "roof-north-west.png")
(define roof-north-west (read-bitmap roof-north-west-img))
