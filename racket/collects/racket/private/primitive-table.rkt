#lang racket/base
(require (only-in '#%linklet primitive-table))

;; By using only `primitive-table` directly, that's the only function
;; needed for bootstrapping situations (that might even replace the
;; linklet implementation otherwise).

(provide import-from-primitive-table)

(define-syntax import-from-primitive-table
  (syntax-rules ()
    [(_ (table-name ...) [id import-id])
     ;; Linklet-flattening tools can recognize this specific pattern
     ;; to substitute a static reference for a dynamic lookup
     (define import-id (hash-ref (or (primitive-table 'table-name) ...) 'id #f))]
    [(_ table-name [id import-id])
     (import-from-primitive-table (table-name) [id import-id])]
    [(_ tables id)
     (import-from-primitive-table tables [id id])]
    [(_ tables bind ...)
     (begin
       (import-from-primitive-table tables bind)
       ...)]))
