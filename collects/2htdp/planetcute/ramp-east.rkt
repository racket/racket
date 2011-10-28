#lang racket/base
(provide ramp-east)
(require racket/draw racket/runtime-path)
(define-runtime-path ramp-east-img "Ramp East.png")
(define ramp-east (read-bitmap ramp-east-img))
