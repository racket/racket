#lang scheme/base
(require scheme/class
         scheme/foreign
         ffi/objc
          "../../syntax.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "image.rkt")
(unsafe!)
(objc-unsafe!)

(provide message%)

;; ----------------------------------------

(import-class NSTextField NSImageView)

(defclass message% item%
  (init parent label
        x y
        style font)
  (inherit get-cocoa)
  
  (super-new [parent parent]
             [cocoa (let* ([label (cond
                                   [(string? label) label]
                                   [(symbol? label) (format "<~a>" label)]
                                   [(send label ok?) label]
                                   [else "<bad>"])]
                           [cocoa
                            (if (string? label)
                                (as-objc-allocation
                                 (tell (tell NSTextField alloc) init))
                                (as-objc-allocation
                                 (tell (tell NSImageView alloc) init)))])
                      (cond
                       [(string? label)
                        (tellv cocoa setSelectable: #:type _BOOL #f)
                        (tellv cocoa setEditable: #:type _BOOL #f)
                        (tellv cocoa setBordered: #:type _BOOL #f)
                        (tellv cocoa setDrawsBackground: #:type _BOOL #f)
                        (tellv cocoa setTitleWithMnemonic: #:type _NSString label)
                        (tellv cocoa sizeToFit)]
                       [else
                        (tellv cocoa setImage: (bitmap->image label))
                        (tellv cocoa setFrame: #:type _NSRect 
                               (make-NSRect (make-NSPoint 0 0)
                                            (make-NSSize (send label get-width)
                                                         (send label get-height))))])
                      cocoa)]
             [no-show? (memq 'deleted style)])

  (define/override (set-label label)
    (tellv (get-cocoa) setTitleWithMnemonic: #:type _NSString label))

  (define/override (gets-focus?) #f)

  (def/public-unimplemented get-font))
