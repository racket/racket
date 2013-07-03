#lang racket/base
(provide roof-west)
(require racket/draw racket/runtime-path)
(define-runtime-path roof-west-img "roof-west.png")
(define roof-west (read-bitmap roof-west-img))
