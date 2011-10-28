#lang racket/base
(provide gem-blue)
(require racket/draw racket/runtime-path)
(define-runtime-path gem-blue-img "Gem Blue.png")
(define gem-blue (read-bitmap gem-blue-img))
