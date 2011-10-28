#lang racket/base
(provide door-tall-open)
(require racket/draw racket/runtime-path)
(define-runtime-path door-tall-open-img "Door Tall Open.png")
(define door-tall-open (read-bitmap door-tall-open-img))
