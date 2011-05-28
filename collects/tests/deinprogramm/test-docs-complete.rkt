#lang racket/base
(require rackunit/docs-complete)

(check-docs (quote deinprogramm/world))
(check-docs (quote deinprogramm/image))

(check-docs (quote deinprogramm/DMdA-beginner) #:skip #rx"(^#%)|(^\\.\\.)|(^contract$)|(^define-contract$)")
(check-docs (quote deinprogramm/DMdA-vanilla) #:skip #rx"(^#%)|(^\\.\\.)|(^contract$)|(^define-contract$)")
(check-docs (quote deinprogramm/DMdA-advanced) #:skip #rx"(^#%)|(^\\.\\.)|(^contract$)|(^define-contract$)")
(check-docs (quote deinprogramm/DMdA-assignments) #:skip #rx"(^#%)|(^\\.\\.)|(^contract$)|(^define-contract$)")
