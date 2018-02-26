#lang racket/base
(require racket/private/primitive-table)

;; Get host notion of syntax for `compile-linklet`.

;; This module uses `primitive-table` from '#%linklet instead of from
;; "linklet.rkt". When bootstrapping, the underlying values are
;; different.

(define-syntax-rule (bounce id ...)
  (begin
    (provide id ...)
    (import-from-primitive-table #%kernel id ...)))

(bounce datum->syntax syntax->datum syntax-property-symbol-keys
        syntax-property syntax-span syntax-position syntax-column
        syntax-line syntax-source syntax-e syntax?)
