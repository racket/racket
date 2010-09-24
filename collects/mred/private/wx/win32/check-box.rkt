#lang racket/base
(require racket/class
         "../../syntax.rkt"
         "button.rkt"
         "item.rkt"
         "const.rkt")

(provide check-box%)

(defclass check-box% base-button%
  (inherit auto-size)

  (super-new)

  (define/override (get-flags) (bitwise-ior BS_AUTOCHECKBOX))

  (define/override (auto-size-button label)
    (auto-size label 0 0 20 0))

  (def/public-unimplemented set-value)
  (def/public-unimplemented get-value))
