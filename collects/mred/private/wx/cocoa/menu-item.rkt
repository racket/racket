#lang scheme/base
(require scheme/class
         scheme/foreign
         ffi/objc
         "../../syntax.rkt"
         "utils.rkt"
         "types.rkt")
(unsafe!)
(objc-unsafe!)

(provide menu-item%)

(import-class NSMenuItem)

(defclass menu-item% object%
  (define/public (id) this)

  (define/public (install menu label)
    (let ([item (tell (tell NSMenuItem alloc) 
                      initWithTitle: #:type _NSString (regexp-replace #rx"\t.*" label "")
                      action: #:type _SEL #f
                      keyEquivalent: #:type _NSString "")])
      (tellv menu addItem: item)
      (tellv item release)))

  (super-new))
