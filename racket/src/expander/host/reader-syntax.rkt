#lang racket/base
(require racket/private/primitive-table)

;; Get host notion of syntax for `datum->syntax`, etc. Bounce the
;; references to these operations through `primitive-table`, so that
;; the bootstrapping process doesn't complain about using them.

;; Note that if the host has a `compile-linklet`, these syntax objects
;; may not be compatible with it. See "correlate-syntax.rkt" for
;; `compile-linklet`-compatible variants.

(define-syntax-rule (bounce id ...)
  (begin
    (provide id ...)
    (import-from-primitive-table #%kernel id ...)))

(bounce datum->syntax syntax->datum syntax-property-symbol-keys
        syntax-property syntax-span syntax-position syntax-column
        syntax-line syntax-source syntax-e syntax?)
