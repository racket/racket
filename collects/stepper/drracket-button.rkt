#lang scheme/base
(require scheme/class string-constants/string-constant
         (prefix-in x: "private/mred-extensions.ss"))
(provide stepper-button-callback stepper-drracket-button)
(define-local-member-name stepper-button-callback)

(define stepper-drracket-button
  (list 
   (string-constant stepper-button-label)
   x:foot-img/horizontal
   (λ (drs-frame) (send drs-frame stepper-button-callback))))
