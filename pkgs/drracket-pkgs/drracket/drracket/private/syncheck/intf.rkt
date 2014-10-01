#lang racket/base
(provide syncheck-text<%>) 
(require racket/class 
         drracket/private/syncheck/syncheck-intf
         "local-member-names.rkt")

(define syncheck-text<%>
  (interface (syncheck-annotations<%>)
    syncheck:init-arrows
    syncheck:clear-arrows
    syncheck:arrows-visible?
    syncheck:get-bindings-table
    syncheck:jump-to-next-bound-occurrence
    syncheck:jump-to-binding-occurrence
    syncheck:jump-to-definition
    syncheck:rename-identifier
    syncheck:tack/untack-arrows))