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

(provide item%)

(import-class NSFont)
(define sys-font (tell NSFont
                       systemFontOfSize: #:type _CGFloat 13))

(defclass item% window%
  (inherit get-cocoa)

  (define/public (get-cocoa-control) (get-cocoa))

  (define/override (enable on?)
    (tellv (get-cocoa) setEnabled: #:type _BOOL on?))
  (define/override (is-window-enabled?)
    (tell #:type _BOOL (get-cocoa-control) isEnabled))

  (define/override (gets-focus?)
    (tell #:type _BOOL (get-cocoa) canBecomeKeyView))

  (def/public-unimplemented set-label)
  (def/public-unimplemented get-label)
  (def/public-unimplemented command)
  (super-new)

  (define/public (init-font cocoa font)
    (tellv cocoa setFont: sys-font)))
