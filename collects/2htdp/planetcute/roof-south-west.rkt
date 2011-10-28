#lang racket/base
(provide roof-south-west)
(require racket/draw racket/runtime-path)
(define-runtime-path roof-south-west-img "roof-south-west.png")
(define roof-south-west (read-bitmap roof-south-west-img))
