#lang scheme
(require scheme/unit
         mred/mred-sig
         "graphics-sig.ss"
         "graphics-posn-less-unit.ss")
(provide graphics@)

(define-unit posn@ (import) (export graphics:posn^)
  (define-struct posn (x y) #:mutable))

(define-compound-unit/infer graphics@
  (import mred^)
  (export graphics:posn^ graphics:posn-less^) 
  (link posn@ graphics-posn-less@))
