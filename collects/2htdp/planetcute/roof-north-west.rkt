#lang racket/base
(provide roof-north-west)
(require racket/draw racket/runtime-path)
(define-runtime-path roof-north-west-img "Roof North West.png")
(define roof-north-west (read-bitmap roof-north-west-img))
