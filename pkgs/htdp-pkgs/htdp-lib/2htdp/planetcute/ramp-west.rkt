#lang racket/base
(provide ramp-west)
(require racket/draw racket/runtime-path)
(define-runtime-path ramp-west-img "ramp-west.png")
(define ramp-west (read-bitmap ramp-west-img))
