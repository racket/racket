#lang racket/base
(require "../common/set.rkt"
         "datum-map.rkt")

(provide tamper?
         tamper-tainted?
         tamper-armed?
         tamper-clean?
         tamper-tainted-for-content
         tamper-needs-propagate?
         tamper-propagated
         
         serialize-tamper
         deserialize-tamper
         current-arm-inspectors)

;; A tamper status is either
;;   * #f - clean
;;   * 'tainted - tainted
;;   * 'tainted/need-propagate - tainted, and taint needs to be propagated to children
;;   * a set of inspectors - armed with a dye pack that is removable with those inspectors

(define (tamper? v)
  (or (not v) (symbol? v) (set? v)))

(define (tamper-tainted? v)
  (symbol? v))

(define (tamper-armed? v)
  (set? v))

(define (tamper-clean? v)
  (not v))

(define (tamper-tainted-for-content v)
  (if (datum-has-elements? v)
      'tainted/need-propagate
      'tainted))

(define (tamper-needs-propagate? t)
  (eq? t 'tainted/need-propagate))

(define (tamper-propagated t)
  (if (eq? t 'tainted/need-propagate)
      'tainted
      t))

;; ----------------------------------------

(define (serialize-tamper t)
  ;; We can't serialize inspectors; any set of inspectors is replaced
  ;; with the current inspector at deserialization time (which
  ;; matches declaration time for a module)
  (if (tamper-armed? t) 'armed t))

;; Set during deserialize to select a code inspector:
(define current-arm-inspectors (make-parameter (seteq)))

(define (deserialize-tamper t)
  (if (eq? t 'armed) (current-arm-inspectors) t))
