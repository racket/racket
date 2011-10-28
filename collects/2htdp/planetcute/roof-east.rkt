#lang racket/base
(provide roof-east)
(require racket/draw racket/runtime-path)
(define-runtime-path roof-east-img "roof-east.png")
(define roof-east (read-bitmap roof-east-img))
