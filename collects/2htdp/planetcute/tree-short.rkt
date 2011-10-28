#lang racket/base
(provide tree-short)
(require racket/draw racket/runtime-path)
(define-runtime-path tree-short-img "Tree Short.png")
(define tree-short (read-bitmap tree-short-img))
