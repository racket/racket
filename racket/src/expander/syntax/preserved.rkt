#lang racket/base
(require "datum-map.rkt"
         "../common/prefab.rkt")

(provide preserved-property-value?
         preserved-property-value
         plain-property-value
         
         check-value-to-preserve)

(struct preserved-property-value (content))

(define (plain-property-value v)
  (if (preserved-property-value? v)
      (preserved-property-value-content v)
      v))

(define (deserialize-preserved-property-value v)
  (preserved-property-value v))

(define (check-value-to-preserve v syntax?)
  (define (check-preserve tail? v)
    (unless (or (null? v) (boolean? v) (symbol? v) (number? v)
                (char? v) (string? v) (bytes? v) (regexp? v)
                (syntax? v)
                (pair? v) (vector? v) (box? v) (hash? v)
                (immutable-prefab-struct-key v))
      (raise-arguments-error 'write
                             "disallowed value in preserved syntax property"
                             "value" v))
    v)
  (datum-map v check-preserve check-preserve disallow-cycles))

(define disallow-cycles
  (hash 'cycle-fail
        (lambda (v)
          (raise-arguments-error 'write
                                 "disallowed cycle in preserved syntax property"
                                 "at" v))))
