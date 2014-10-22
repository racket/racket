#lang racket/base
(require racket/class)
(provide (all-defined-out))

(define-local-member-name
  syncheck:find-source-object
  syncheck:add-background-color
  syncheck:add-docs-menu
  syncheck:color-range
  syncheck:add-require-open-menu
  syncheck:add-id-set
  syncheck:add-arrow
  syncheck:add-arrow/name-dup
  syncheck:add-rename-menu
  syncheck:add-tail-arrow
  syncheck:add-mouse-over-status
  syncheck:add-jump-to-definition)
