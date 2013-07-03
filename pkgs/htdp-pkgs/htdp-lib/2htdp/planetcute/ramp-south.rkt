#lang racket/base
(provide ramp-south)
(require racket/draw racket/runtime-path)
(define-runtime-path ramp-south-img "ramp-south.png")
(define ramp-south (read-bitmap ramp-south-img))
