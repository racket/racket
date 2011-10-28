#lang racket/base
(provide tree-ugly)
(require racket/draw racket/runtime-path)
(define-runtime-path tree-ugly-img "tree-ugly.png")
(define tree-ugly (read-bitmap tree-ugly-img))
