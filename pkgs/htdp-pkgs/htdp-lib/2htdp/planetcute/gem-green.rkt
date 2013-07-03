#lang racket/base
(provide gem-green)
(require racket/draw racket/runtime-path)
(define-runtime-path gem-green-img "gem-green.png")
(define gem-green (read-bitmap gem-green-img))
