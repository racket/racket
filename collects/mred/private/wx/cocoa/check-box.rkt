#lang scheme/base
(require ffi/objc
         scheme/foreign
         scheme/class
          "../../syntax.rkt"
         "button.rkt"
         "types.rkt"
         "const.rkt")
(unsafe!)
(objc-unsafe!)

(provide check-box%)

;; ----------------------------------------

(defclass check-box% core-button%
  (inherit get-cocoa)
  (super-new [button-type NSSwitchButton]
             [event-type 'check-box])

  (define/public (set-value v)
    (tellv (get-cocoa) setState: #:type _NSInteger (if v 1 0)))
  (define/public (get-value)
    (positive? (tell #:type _NSInteger (get-cocoa) state))))

