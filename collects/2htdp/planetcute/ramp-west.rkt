#lang racket/base
(provide ramp-west)
(require racket/draw racket/runtime-path)
(define-runtime-path ramp-west-img "Ramp West.png")
(define ramp-west (read-bitmap ramp-west-img))
