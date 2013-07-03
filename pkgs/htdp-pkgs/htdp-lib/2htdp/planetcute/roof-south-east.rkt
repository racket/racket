#lang racket/base
(provide roof-south-east)
(require racket/draw racket/runtime-path)
(define-runtime-path roof-south-east-img "roof-south-east.png")
(define roof-south-east (read-bitmap roof-south-east-img))
