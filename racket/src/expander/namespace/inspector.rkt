#lang racket/base
(require "../common/parameter-like.rkt")

(provide current-module-code-inspector)

;; Parameter to select inspector for functions like `syntax-arm`
(define-parameter-like current-module-code-inspector #f)
