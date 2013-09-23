#lang racket/base

(require racket/unit "file-sig.rkt" dynext/file)

(provide dynext:file@)

(define-unit-from-context dynext:file@ dynext:file^)
