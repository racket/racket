#lang scheme/base
(require scheme/class
         scheme/foreign
         ffi/objc
          "../../syntax.rkt"
         "window.rkt"
         "const.rkt"
         "types.rkt")
(unsafe!)
(objc-unsafe!)

(provide item%
         install-control-font)

(import-class NSFont)
(define sys-font (tell NSFont
                       systemFontOfSize: #:type _CGFloat 13))

(define (install-control-font cocoa font)
  (tellv cocoa setFont: sys-font))

(defclass item% window%
  (inherit get-cocoa)

  (init-field callback)

  (define/public (get-cocoa-control) (get-cocoa))

  (define/override (enable on?)
    (tellv (get-cocoa-control) setEnabled: #:type _BOOL on?))
  (define/override (is-window-enabled?)
    (tell #:type _BOOL (get-cocoa-control) isEnabled))

  (define/override (gets-focus?)
    (tell #:type _BOOL (get-cocoa-control) canBecomeKeyView))

  (define/public (command e)
    (callback this e))

  (def/public-unimplemented set-label)
  (def/public-unimplemented get-label)
  (super-new)

  (define/public (init-font cocoa font)
    (install-control-font cocoa font)))
