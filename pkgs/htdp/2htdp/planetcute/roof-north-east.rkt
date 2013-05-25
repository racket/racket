#lang racket/base
(provide roof-north-east)
(require racket/draw racket/runtime-path)
(define-runtime-path roof-north-east-img "roof-north-east.png")
(define roof-north-east (read-bitmap roof-north-east-img))
