#lang racket/base
(require "../common/inline.rkt")

(provide weaker-inspector)

(define-inline (weaker-inspector a b)
  (cond
    [(eq? a b) a]
    [(not a) #f]
    [(not b) #f]
    [(inspector-superior? a b) b]
    [(inspector-superior? b a) a]
    [else #f]))
