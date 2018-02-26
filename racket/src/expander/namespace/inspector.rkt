#lang racket/base

(provide current-module-code-inspector)

;; Parameter to select inspector for functions like `syntax-arm`
(define current-module-code-inspector (make-parameter #f))
