#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         racket/class
          "../../syntax.rkt"
         "button.rkt"
         "types.rkt"
         "const.rkt")

(provide 
 (protect-out check-box%))

;; ----------------------------------------

(defclass check-box% core-button%
  (inherit get-cocoa)
  (super-new [button-type NSSwitchButton]
             [event-type 'check-box])

  (define/public (set-value v)
    (tellv (get-cocoa) setState: #:type _NSInteger (if v 1 0)))
  (define/public (get-value)
    (positive? (tell #:type _NSInteger (get-cocoa) state))))

