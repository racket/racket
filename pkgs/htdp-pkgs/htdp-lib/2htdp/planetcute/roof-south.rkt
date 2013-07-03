#lang racket/base
(provide roof-south)
(require racket/draw racket/runtime-path)
(define-runtime-path roof-south-img "roof-south.png")
(define roof-south (read-bitmap roof-south-img))
