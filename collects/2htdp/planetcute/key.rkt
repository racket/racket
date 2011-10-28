#lang racket/base
(provide key)
(require racket/draw racket/runtime-path)
(define-runtime-path key-img "Key.png")
(define key (read-bitmap key-img))
