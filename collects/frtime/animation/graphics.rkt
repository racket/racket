#lang scheme
(require scheme/unit
         mred/mred-sig
         mred
         "graphics-sig.rkt"
         "graphics-unit.rkt")
(provide-signature-elements graphics:posn^ graphics:posn-less^)

(define-values/invoke-unit/infer graphics@)
