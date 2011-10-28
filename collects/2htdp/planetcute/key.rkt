#lang racket/base
(provide key)
(require racket/draw racket/runtime-path)
(define-runtime-path key-img "key.png")
(define key (read-bitmap key-img))
