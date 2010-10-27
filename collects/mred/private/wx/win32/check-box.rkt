#lang racket/base
(require racket/class
         "../../syntax.rkt"
         "button.rkt"
         "item.rkt"
         "utils.rkt"
         "const.rkt")

(provide 
 (protect-out check-box%))

(define BM_GETCHECK #x00F0)
(define BM_SETCHECK #x00F1)

(defclass check-box% base-button%
  (inherit auto-size
           get-hwnd)

  (super-new)

  (define/override (get-flags) (bitwise-ior BS_AUTOCHECKBOX))

  (define/override (get-button-background)
    (GetSysColor COLOR_BTNFACE))

  (define/override (auto-size-button font label)
    (auto-size font label 0 0 20 0))

  (define/public (set-value v)
    (void (SendMessageW (get-hwnd) BM_SETCHECK (if v 1 0) 0)))

  (define/public (get-value)
    (positive? (bitwise-and #x3 (SendMessageW (get-hwnd) BM_GETCHECK 0 0)))))
