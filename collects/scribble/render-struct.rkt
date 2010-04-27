#lang scheme/base

(require scheme/serialize)
(provide (struct-out mobile-root))

(define-serializable-struct mobile-root (path) #:mutable)
