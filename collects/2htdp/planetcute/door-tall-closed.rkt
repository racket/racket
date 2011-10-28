#lang racket/base
(provide door-tall-closed)
(require racket/draw racket/runtime-path)
(define-runtime-path door-tall-closed-img "Door Tall Closed.png")
(define door-tall-closed (read-bitmap door-tall-closed-img))
