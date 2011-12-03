#lang racket/base
(require racket/class 
         racket/gui/base
         string-constants/string-constant
         icons slideshow/pict)
(provide syncheck-drracket-button
         syncheck-bitmap
         syncheck-small-bitmap
         syncheck:button-callback)

(define-local-member-name syncheck:button-callback)

(define syncheck-bitmap
  (pict->bitmap (hb-append (magnifying-glass-left-icon-pict (* 7/8 (toolbar-icon-height)))
                           (check-icon-pict 'green (toolbar-icon-height)))))

(define syncheck-small-bitmap
  (pict->bitmap (rb-superimpose (hc-append (check-icon-pict 'green (toolbar-icon-height))
                                           (blank (* 1/4 (toolbar-icon-height))))
                                (magnifying-glass-icon-pict (* 3/4 (toolbar-icon-height))))))

(define syncheck-drracket-button
  (list 
   (string-constant check-syntax)
   syncheck-bitmap
   (λ (drs-frame) (send drs-frame syncheck:button-callback))))

