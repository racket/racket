#lang racket/base
(provide roof-east)
(require racket/draw racket/runtime-path)
(define-runtime-path roof-east-img "Roof East.png")
(define roof-east (read-bitmap roof-east-img))
