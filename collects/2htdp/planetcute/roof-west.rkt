#lang racket/base
(provide roof-west)
(require racket/draw racket/runtime-path)
(define-runtime-path roof-west-img "Roof West.png")
(define roof-west (read-bitmap roof-west-img))
