#lang scheme/base
(require scheme/class 
         scheme/gui/base
         string-constants/string-constant)
(provide syncheck-drscheme-button
         syncheck-bitmap
         syncheck:button-callback)

(define-local-member-name syncheck:button-callback)

(define syncheck-bitmap (make-object bitmap% (build-path (collection-path "icons") "syncheck.png") 'png/mask))

(define syncheck-drscheme-button
  (list 
   (string-constant check-syntax)
   syncheck-bitmap
   (λ (drs-frame) (send drs-frame syncheck:button-callback))))

