#lang racket/base

(require (for-syntax racket/base)
         racket/unit
         racket/class
         racket/surrogate
         "sig.rkt")

(provide mode@ surrogate-methods)

(define-syntax (surrogate-methods stx)
  (syntax-case stx ()
    [(_ m)
     #'(m (augment (void) on-change ())
          (override on-char (event))
          (override on-default-char (event))
          (override on-default-event (event))
          (augment (void) on-display-size ())
          (augment (void) on-edit-sequence ())
          (override on-event (event))
          (override on-focus (on?))
          (augment (void) on-load-file (filename format))
          (override on-local-char (event))
          (override on-local-event (event))
          (override on-new-box (type))
          (override on-new-image-snip (filename kind relative-path? inline?))
          (override on-paint (before? dc left top right bottom dx dy draw-caret))
          (augment (void) on-save-file (filename format))
          (augment (void) on-snip-modified (snip modified?))
          
          (augment (void) on-change-style (start len))
          (augment (void) on-delete (start len))
          (augment (void) on-insert (start len))
          (override on-new-string-snip ())
          (override on-new-tab-snip ())
          (augment (void) on-set-size-constraint ())
          
          (augment (void) after-change-style (start len))
          (augment (void) after-delete (start len))
          (augment (void) after-insert (start len))
          (augment (void) after-set-position ())
          (augment (void) after-set-size-constraint ())
          (augment (void) after-edit-sequence ())
          (augment (void) after-load-file (success?))
          (augment (void) after-save-file (success?))
          
          (augment #t can-change-style? (start len))
          (augment #t can-delete? (start len))
          (augment #t can-insert? (start len))
          (augment #t can-set-size-constraint? ())
          (override can-do-edit-operation? (op) (op recursive?))
          (augment #t can-load-file? (filename format))
          (augment #t can-save-file? (filename format))
          
          (override put-file (directory default-name)))]))

(define-unit mode@
  (import)
  (export framework:mode^)
  
  (define-values (host-text-mixin host-text<%> surrogate-text% surrogate-text<%>)
    (surrogate-methods surrogate)))
