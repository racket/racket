#lang racket/base
(require racket/class)
(provide (all-defined-out))

(define-local-member-name
  syncheck:init-arrows
  syncheck:clear-arrows
  syncheck:clear-coloring
  syncheck:arrows-visible?

  syncheck:find-source-object
  syncheck:add-background-color
  syncheck:add-docs-menu
  syncheck:color-range
  syncheck:add-require-open-menu
  syncheck:add-rename-menu
  syncheck:add-arrow
  syncheck:add-tail-arrow
  syncheck:add-mouse-over-status
  syncheck:add-jump-to-definition
  
  syncheck:sort-bindings-table
  syncheck:jump-to-next-bound-occurrence
  syncheck:jump-to-binding-occurrence
  syncheck:jump-to-definition
  
  syncheck:clear-highlighting
  syncheck:apply-style/remember
  ;syncheck:error-report-visible? ;; test suite uses this one.
  ;syncheck:get-bindings-table    ;; test suite uses this one.
  syncheck:clear-error-message
  
  hide-error-report
  get-error-report-text
  get-error-report-visible?
  
  turn-off-error-report
  turn-on-error-report
  
  update-button-visibility/settings
  
  set-syncheck-mode
  get-syncheck-mode
  update-menu-status)
