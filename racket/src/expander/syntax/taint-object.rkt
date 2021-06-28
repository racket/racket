#lang racket/base
(require "datum-map.rkt")

(provide taint?
         tainted-for-content
         taint-needs-propagate?
         taint-propagated)

;; A taint is either
;;   * #f - clean
;;   * 'tainted - tainted
;;   * 'tainted/need-propagate - tainted, and taint needs to be propagated to children

(define (taint? v) ; just to distinguish from propagation
  (or (not v) (symbol? v)))

(define (tainted-for-content v)
  (if (datum-has-elements? v)
      'tainted/need-propagate
      'tainted))

(define (taint-needs-propagate? t)
  (eq? t 'tainted/need-propagate))

(define (taint-propagated t)
  (if (eq? t 'tainted/need-propagate)
      'tainted
      t))
