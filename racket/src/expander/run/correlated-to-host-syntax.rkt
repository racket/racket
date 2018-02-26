#lang racket/base
(require "../syntax/datum-map.rkt"
         "../host/correlate.rkt")

;; Convert from `compile-linklet`-compatible representation to a
;; `compile`-compatible representation.

(provide correlated->host-syntax)

(define (correlated->host-syntax v)
  (datum-map v
             (lambda (tail? v)
               (cond
                [(correlated? v)
                 (define e (correlated->host-syntax (correlated-e v)))
                 (define s (datum->syntax #f
                                          e 
                                          (vector (correlated-source v)
                                                  (correlated-line v)
                                                  (correlated-column v)
                                                  (correlated-position v)
                                                  (correlated-span v))))
                 (define keys (correlated-property-symbol-keys v))
                 (for/fold ([s s]) ([key (in-list keys)])
                  (syntax-property s key (correlated-property v key)))]
                [else v]))))
