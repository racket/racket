#lang scheme/base
(require scheme/class string-constants/string-constant
         (prefix-in x: "private/mred-extensions.ss"))
(provide stepper-button-callback stepper-drscheme-button)
(define-local-member-name stepper-button-callback)

(define stepper-drscheme-button
  (list (list 
         (string-constant stepper-button-label)
         x:foot-img/horizontal
         (λ (drs-frame) (send drs-frame stepper-button-callback)))))