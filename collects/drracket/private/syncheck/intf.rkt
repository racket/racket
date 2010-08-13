#lang racket/base
(require racket/class
         racket/promise
         setup/xref
         scribble/xref)

(define-local-member-name
  syncheck:init-arrows
  syncheck:clear-arrows
  syncheck:arrows-visible?
  syncheck:add-menu
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

(define syncheck-text<%>
  (interface ()
    syncheck:init-arrows
    syncheck:clear-arrows
    syncheck:arrows-visible?
    syncheck:add-menu
    syncheck:add-arrow
    syncheck:add-tail-arrow
    syncheck:add-mouse-over-status
    syncheck:add-jump-to-definition
    syncheck:sort-bindings-table
    syncheck:get-bindings-table
    syncheck:jump-to-next-bound-occurrence
    syncheck:jump-to-binding-occurrence
    syncheck:jump-to-definition))

;; use this to communicate the frame being
;; syntax checked w/out having to add new
;; parameters to all of the functions
(define currently-processing-definitions-text (make-parameter #f))

(define xref (if (getenv "PLTDRXREFDELAY")
                 (begin
                   (printf "PLTDRXREFDELAY: using plain delay\n")
                   (delay (begin
                            (printf "PLTDRXREFDELAY: loading xref\n")
                            (begin0
                              (load-collections-xref)
                              (printf "PLTDRXREFDELAY: loaded xref\n")))))
                 (delay/idle (load-collections-xref))))
(define (get-xref) (force xref))

(provide syncheck-text<%>
         currently-processing-definitions-text
         get-xref
         
         ;; methods
         syncheck:init-arrows
         syncheck:clear-arrows
         syncheck:arrows-visible?
         syncheck:add-menu
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