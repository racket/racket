#lang racket/base
(provide ramp-south)
(require racket/draw racket/runtime-path)
(define-runtime-path ramp-south-img "Ramp South.png")
(define ramp-south (read-bitmap ramp-south-img))
