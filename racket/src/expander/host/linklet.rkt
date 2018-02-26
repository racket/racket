#lang racket/base
(require racket/private/primitive-table
         "../run/linklet-operation.rkt")

;; The `racket/private/primitive-table` module uses only
;; `primitive-table` directly, so that's the only function needed for
;; bootstrapping --- and generally so we can replace the linklet
;; implementation for bootstrapping. See also "../run/linklet.rkt".

(define-syntax-rule (bounce id ...)
  (begin
    (provide id ...)
    (import-from-primitive-table
     ;; As a hook for bootstrapping, first check for a replacement of
     ;; the primitive '#%linklet module:
     (#%bootstrap-linklet #%linklet)
     id
     ...)))

(linklet-operations=> bounce)

(void
 (unless variable-reference-constant?
   (error "broken '#%linklet primitive table; maybe you need to use \"bootstrap-run.rkt\"")))
