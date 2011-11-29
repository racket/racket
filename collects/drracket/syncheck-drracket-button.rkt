#lang racket/base
(require racket/class 
         racket/gui/base
         string-constants/string-constant
         icons)
(provide syncheck-drracket-button
         syncheck-bitmap
         syncheck-small-bitmap
         syncheck:button-callback)

(define-local-member-name syncheck:button-callback)

(define syncheck-bitmap (check-syntax-icon (toolbar-icon-height)))
(define syncheck-small-bitmap (check-syntax-small-icon (toolbar-icon-height)))

(define syncheck-drracket-button
  (list 
   (string-constant check-syntax)
   syncheck-bitmap
   (λ (drs-frame) (send drs-frame syncheck:button-callback))))

