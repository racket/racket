#lang racket/base

(require (prefix-in d: unstable/debug))
(provide debugf debugging? dprintf)

(define debugging? (make-parameter #f))
(define-syntax-rule (debugf f . args) (if (debugging?) (d:debugf f . args) (f . args)))
(define (dprintf . args) (when (debugging?) (apply d:dprintf args)))
