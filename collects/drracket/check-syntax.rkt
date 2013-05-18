#lang at-exp racket/base
(require racket/contract
         racket/class
         "private/syncheck/traversals.rkt"
         "private/syncheck/intf.rkt"
         "private/syncheck/local-member-names.rkt")

(provide/contract
 [make-traversal 
  (-> namespace?
      (or/c path-string? #f)
      (values (->* (syntax?) ((-> any/c void?)) void?)
              (-> void?)))]
 
 [current-max-to-send-at-once
  (parameter/c (or/c +inf.0 (and/c exact-integer? (>=/c 2))))]
 [syncheck-annotations<%>
  interface?]
 [current-annotations 
  (parameter/c (or/c #f (is-a?/c syncheck-annotations<%>)))]
 [annotations-mixin 
  (and/c mixin-contract
         (-> any/c (implementation?/c syncheck-annotations<%>)))])

;; methods in syncheck-annotations<%>
(provide 
 syncheck:find-source-object
 syncheck:add-background-color
 syncheck:add-require-open-menu
 syncheck:add-docs-menu
 syncheck:add-id-set
 syncheck:add-rename-menu
 syncheck:add-arrow
 syncheck:add-arrow/name-dup
 syncheck:add-tail-arrow
 syncheck:add-mouse-over-status
 syncheck:add-jump-to-definition
 syncheck:color-range)



