#lang racket/base
(provide rock)
(require racket/draw racket/runtime-path)
(define-runtime-path rock-img "rock.png")
(define rock (read-bitmap rock-img))
