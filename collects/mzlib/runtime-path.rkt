#lang racket/base

;; deprecated library, see `racket/runtime-path`

(require racket/runtime-path)
(provide define-runtime-path
         define-runtime-paths
         define-runtime-path-list
         define-runtime-module-path-index
         runtime-paths)
