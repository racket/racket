#lang racket/base
(provide gem-orange)
(require racket/draw racket/runtime-path)
(define-runtime-path gem-orange-img "Gem Orange.png")
(define gem-orange (read-bitmap gem-orange-img))
