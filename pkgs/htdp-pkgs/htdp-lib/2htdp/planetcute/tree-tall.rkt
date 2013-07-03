#lang racket/base
(provide tree-tall)
(require racket/draw racket/runtime-path)
(define-runtime-path tree-tall-img "tree-tall.png")
(define tree-tall (read-bitmap tree-tall-img))
