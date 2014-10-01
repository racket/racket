#lang racket/base
(require racket/class
         "syncheck-local-member-names.rkt")

(define syncheck-annotations<%>
  (interface ()
    syncheck:find-source-object
    syncheck:add-background-color
    syncheck:add-require-open-menu
    syncheck:add-docs-menu
    syncheck:add-id-set
    syncheck:add-arrow
    syncheck:add-arrow/name-dup
    syncheck:add-tail-arrow
    syncheck:add-mouse-over-status
    syncheck:add-jump-to-definition
    syncheck:add-definition-target
    syncheck:color-range
    
    syncheck:add-rename-menu))

;; use this to communicate the frame being
;; syntax checked w/out having to add new
;; parameters to all of the functions
(define current-annotations (make-parameter #f))

(define annotations-mixin
  (mixin () (syncheck-annotations<%>)
    (define/public (syncheck:find-source-object stx) #f)
    (define/public (syncheck:add-background-color source start end color) (void))
    (define/public (syncheck:add-require-open-menu source start end key) (void))
    (define/public (syncheck:add-id-set all-ids new-name-intereferes?) (void))
    (define/public (syncheck:add-rename-menu sym all-ids new-name-intereferes?) (void))
    (define/public (syncheck:add-docs-menu text start-pos end-pos
                                           key
                                           the-label
                                           path
                                           definition-tag
                                           tag)
      (void))
    (define/public (syncheck:add-arrow start-text start-pos-left start-pos-right
                                       end-text end-pos-left end-pos-right
                                       actual? level)
      (void))
    (define/public (syncheck:add-arrow/name-dup start-text start-pos-left start-pos-right
                                                end-text end-pos-left end-pos-right
                                                actual? level require-arrow? name-dup?)
      (syncheck:add-arrow start-text start-pos-left start-pos-right
                          end-text end-pos-left end-pos-right
                          actual? level))
    (define/public (syncheck:add-tail-arrow from-text from-pos to-text to-pos) (void))
    (define/public (syncheck:add-mouse-over-status text pos-left pos-right str) (void))
    (define/public (syncheck:add-jump-to-definition text start end id filename submods) (void))
    (define/public (syncheck:add-definition-target source pos-left pos-right id mods) (void))
    (define/public (syncheck:color-range source start finish style-name) (void))
    (super-new)))

(provide syncheck-annotations<%>
         current-annotations
         annotations-mixin)
