#lang racket/base
(require racket/class 
         racket/gui/base
         string-constants/string-constant
         images/compile-time
         (for-syntax racket/base images/icons/tool images/icons/style))
(provide syncheck-drracket-button
         syncheck-bitmap
         syncheck-small-bitmap
         syncheck:button-callback)

(define-local-member-name syncheck:button-callback)

(define syncheck-bitmap
  (compiled-bitmap (check-syntax-icon #:height (toolbar-icon-height))))
(define syncheck-small-bitmap
  (compiled-bitmap (small-check-syntax-icon #:height (toolbar-icon-height))))

(define syncheck-drracket-button
  (list 
   (string-constant check-syntax)
   syncheck-bitmap
   (λ (drs-frame) (send drs-frame syncheck:button-callback))))

