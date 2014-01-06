#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         "../../syntax.rkt"
         "../../lock.rkt"
         "window.rkt"
         "const.rkt"
         "types.rkt"
         "font.rkt")

(provide 
 (protect-out item%
              install-control-font
              sys-font-size
              strip-mnemonic))

(import-class NSFont)

(define sys-font-size 13)
(define sys-font 
  (atomically
   (let ([f (tell NSFont systemFontOfSize: #:type _CGFloat sys-font-size)])
     (tellv f retain)
     f)))

(define (install-control-font cocoa font)
  (if font
      (tellv cocoa setFont: (font->NSFont font))
      (tellv cocoa setFont: sys-font)))

(define (strip-mnemonic s)
  (regexp-replace #rx"[&](.)" s "\\1"))

(defclass item% window%
  (inherit get-cocoa
           is-window-enabled?)

  (init-field callback)

  (define/public (get-cocoa-control) (get-cocoa))

  (define/override (enable-window on?)
    (let ([on? (and on? (is-window-enabled?))])
      (tellv (get-cocoa-control) setEnabled: #:type _BOOL on?)))

  (define/override (gets-focus?)
    (tell #:type _BOOL (get-cocoa-control) canBecomeKeyView))

  (define/public (command e)
    (callback this e))

  (def/public-unimplemented set-label)
  (def/public-unimplemented get-label)
  (super-new)

  (define/public (init-font cocoa font)
    (install-control-font cocoa font)))
