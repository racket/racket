#lang racket/base
(require racket/runtime-path)
(define-runtime-path quiet "quiet.rktl")
(current-namespace (make-base-namespace))
(eval '(require racket (for-syntax racket)))
(load "quiet.rktl")
