#lang racket/base
(require racket/class
         racket/promise
         setup/xref)

(define-local-member-name
  syncheck:init-arrows
  syncheck:clear-arrows
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

(define syncheck-annotations<%>
  (interface ()
    syncheck:find-source-object
    syncheck:add-background-color
    syncheck:add-require-open-menu
    syncheck:add-docs-menu
    syncheck:add-rename-menu
    syncheck:add-arrow
    syncheck:add-tail-arrow
    syncheck:add-mouse-over-status
    syncheck:add-jump-to-definition
    syncheck:color-range))

(define syncheck-text<%>
  (interface (syncheck-annotations<%>)
    syncheck:init-arrows
    syncheck:clear-arrows
    syncheck:arrows-visible?
    syncheck:sort-bindings-table
    syncheck:get-bindings-table
    syncheck:jump-to-next-bound-occurrence
    syncheck:jump-to-binding-occurrence
    syncheck:jump-to-definition))

;; use this to communicate the frame being
;; syntax checked w/out having to add new
;; parameters to all of the functions
(define current-annotations (make-parameter #f))

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

(define annotations-mixin
  (mixin () (syncheck-annotations<%>)
    (define/public (syncheck:find-source-object stx) #f)
    (define/public (syncheck:add-background-color source start end color) (void))
    (define/public (syncheck:add-require-open-menu source color start end key) (void))
    (define/public (syncheck:add-rename-menu text start-pos end-pos key id-as-sym id-sets rename-ht get-ids) (void))
    (define/public (syncheck:add-docs-menu text start-pos end-pos key the-label path tag) (void))
    (define/public (syncheck:add-arrow start-text start-pos-left start-pos-right
                                       end-text end-pos-left end-pos-right
                                       actual? level)
      (void))
    (define/public (syncheck:add-tail-arrow from-text from-pos to-text to-pos) (void))
    (define/public (syncheck:add-mouse-over-status text pos-left pos-right str) (void))
    (define/public (syncheck:add-jump-to-definition text start end id filename) (void))
    (define/public (syncheck:color-range source start finish style-name mode) (void))
    (super-new)))

(provide syncheck-text<%>
         syncheck-annotations<%>
         current-annotations
         annotations-mixin
         get-xref
         
         ;; methods
         syncheck:init-arrows
         syncheck:clear-arrows
         syncheck:arrows-visible?
         syncheck:add-arrow
         syncheck:add-tail-arrow
         syncheck:add-mouse-over-status
         syncheck:add-jump-to-definition
         syncheck:add-docs-menu
         syncheck:add-require-open-menu
         syncheck:add-rename-menu
         syncheck:add-background-color
         syncheck:sort-bindings-table
         syncheck:jump-to-next-bound-occurrence
         syncheck:jump-to-binding-occurrence
         syncheck:jump-to-definition
         
         syncheck:clear-highlighting
         syncheck:apply-style/remember
         ;syncheck:error-report-visible? ;; test suite uses this one.
         ;syncheck:get-bindings-table    ;; test suite uses this one.
         syncheck:clear-error-message
         syncheck:color-range
         syncheck:find-source-object
         
         hide-error-report
         get-error-report-text
         get-error-report-visible?
         
         turn-off-error-report
         turn-on-error-report
         
         update-button-visibility/settings
         
         set-syncheck-mode
         get-syncheck-mode
         update-menu-status)
