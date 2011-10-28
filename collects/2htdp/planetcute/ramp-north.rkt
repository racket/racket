#lang racket/base
(provide ramp-north)
(require racket/draw racket/runtime-path)
(define-runtime-path ramp-north-img "ramp-north.png")
(define ramp-north (read-bitmap ramp-north-img))
