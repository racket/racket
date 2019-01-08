#lang racket/base
(require "syntax.rkt"
         "preserved.rkt"
         "../common/contract.rkt")

(provide syntax-property
         syntax-property-preserved?
         syntax-property-symbol-keys
         syntax-property-remove)

;; ----------------------------------------

(define/who syntax-property
  (case-lambda
    [(s key)
     (check who syntax? s)
     (define v (hash-ref (syntax-props s) key #f))
     (plain-property-value v)]
    [(s key val)
     (check who syntax? s)
     (define pval (if (eq? key 'paren-shape)
                      (preserved-property-value val)
                      val))
     (struct-copy syntax s
                  [props (hash-set (syntax-props s) key pval)])]
    [(s key val preserved?)
     (check who syntax? s)
     (when preserved?
       (unless (and (symbol? key) (symbol-interned? key))
         (raise-arguments-error who
                                "key for a preserved property must be an interned symbol"
                                "given key" key
                                "given value" val)))
     (define pval (if preserved?
                      (preserved-property-value val)
                      val))
     (struct-copy syntax s
                  [props (hash-set (syntax-props s) key pval)])]))

(define/who (syntax-property-preserved? s key)
  (check who syntax? s)
  (unless (and (symbol? key) (symbol-interned? key))
    (raise-argument-error who "(and/c symbol? symbol-interned?)" key))
  (preserved-property-value? (hash-ref (syntax-props s) key #f)))

(define/who (syntax-property-symbol-keys s)
  (unless (syntax? s)
    (raise-argument-error who "syntax" s))
  (for/list ([(k v) (in-immutable-hash (syntax-props s))]
             #:when (and (symbol? k) (symbol-interned? k)))
    k))

(define/who (syntax-property-remove s key)
  (check who syntax? s)
  (if (hash-ref (syntax-props s) key #f)
      (struct-copy syntax s
                   [props (hash-remove (syntax-props s) key)])
      s))
