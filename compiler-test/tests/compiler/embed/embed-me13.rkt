#lang racket/base
(require racket/runtime-path)
(define-runtime-module-path-index _mod "embed-me14.rkt")
(dynamic-require _mod #f)
