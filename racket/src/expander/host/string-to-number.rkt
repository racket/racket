#lang racket/base
(require racket/private/primitive-table)

;; Get host implementation of `string->number` for very basic number
;; parsing. Going through `primitive-table` prevents the reference
;; from being tied back to out implementation here when flattening the
;; expander+reader.

(provide string->number)

(import-from-primitive-table #%kernel string->number)
