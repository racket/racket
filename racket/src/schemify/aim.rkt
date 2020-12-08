#lang racket/base
(provide aim?)

;; macro statically ensures that the second argument is a valid target
(define-syntax aim?
  (syntax-rules (cify interp)
    [(_ e 'cify) (eq? e 'cify)]
    [(_ e 'interp) (eq? e 'interp)]
    [(_ e 'system) (eq? e 'system)]))
