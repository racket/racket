#lang racket/base
(require racket/class
         drracket/private/syncheck/syncheck-local-member-names)
(provide (all-defined-out)
         (all-from-out drracket/private/syncheck/syncheck-local-member-names))

(define-local-member-name
  syncheck:init-arrows
  syncheck:clear-arrows
  syncheck:clear-coloring
  syncheck:arrows-visible?

  syncheck:jump-to-next-bound-occurrence
  syncheck:jump-to-binding-occurrence
  syncheck:jump-to-definition
  syncheck:rename-identifier
  syncheck:tack/untack-arrows
  
  syncheck:clear-highlighting
  syncheck:apply-style/remember
  ;syncheck:error-report-visible? ;; test suite uses this one.
  ;syncheck:get-bindings-table    ;; test suite uses this one.
  syncheck:clear-error-message
  
  syncheck:find-definition-target
  
  jump-to
  
  hide-error-report
  get-error-report-text
  get-error-report-visible?
  
  turn-off-error-report
  turn-on-error-report
  
  update-button-visibility/settings
  
  set-syncheck-mode
  get-syncheck-mode
  update-menu-status)
