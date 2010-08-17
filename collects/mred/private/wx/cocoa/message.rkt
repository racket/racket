#lang scheme/base
(require scheme/class
         scheme/foreign
         ffi/objc
         racket/draw/bitmap
          "../../syntax.rkt"
          "window.rkt"
          "item.rkt"
          "utils.rkt"
          "types.rkt"
          "image.rkt")
(unsafe!)
(objc-unsafe!)

(provide message%)

;; ----------------------------------------

(import-class NSTextField NSImageView NSWorkspace)

(define _OSType _uint32)

(define-cocoa NSFileTypeForHFSTypeCode (_fun _OSType -> _id))

(define (get-app-icon)
  (tell (tell NSWorkspace sharedWorkspace)
        iconForFile:
        (tell (tell (tell NSWorkspace sharedWorkspace)
                    activeApplication)
              objectForKey:
              #:type _NSString
              "NSApplicationPath")))

(define-objc-class MyTextField NSTextField
  #:mixins (FocusResponder KeyMouseResponder CursorDisplayer)
  [wxb])

(define-objc-class MyImageView NSImageView
  #:mixins (FocusResponder KeyMouseResponder CursorDisplayer)
  [wxb])

(defclass message% item%
  (init parent label
        x y
        style font)
  (inherit get-cocoa init-font)
  
  (super-new [parent parent]
             [cocoa (let* ([label (cond
                                   [(string? label) label]
                                   [(symbol? label)
                                    (let ([icon
                                           (if (eq? label 'app)
                                               (get-app-icon)
                                               (let ([id (integer-bytes->integer
                                                          (case label
                                                            [(caution) #"caut"]
                                                            [(stop) #"stop"])
                                                          #f
                                                          #t)])
                                                 (tell (tell NSWorkspace sharedWorkspace)
                                                       iconForFileType:
                                                       (NSFileTypeForHFSTypeCode id))))])
                                      (tellv icon setSize: #:type _NSSize (make-NSSize 64 64))
                                      icon)]
                                   [(send label ok?) label]
                                   [else "<bad>"])]
                           [cocoa
                            (if (string? label)
                                (as-objc-allocation
                                 (tell (tell MyTextField alloc) init))
                                (as-objc-allocation
                                 (tell (tell MyImageView alloc) init)))])
                      (cond
                       [(string? label)
                        (init-font cocoa font)
                        (tellv cocoa setSelectable: #:type _BOOL #f)
                        (tellv cocoa setEditable: #:type _BOOL #f)
                        (tellv cocoa setBordered: #:type _BOOL #f)
                        (tellv cocoa setDrawsBackground: #:type _BOOL #f)
                        (tellv cocoa setTitleWithMnemonic: #:type _NSString label)
                        (tellv cocoa sizeToFit)]
                       [else
                        (tellv cocoa setImage: (if (label . is-a? . bitmap%)
                                                   (bitmap->image label)
                                                   label))
                        (tellv cocoa setFrame: #:type _NSRect 
                               (make-NSRect (make-NSPoint 0 0)
                                            (if (label . is-a? . bitmap%)
                                                (make-NSSize (send label get-width)
                                                             (send label get-height))
                                                (tell #:type _NSSize label size))))])
                      cocoa)]
             [callback void]
             [no-show? (memq 'deleted style)])

  (define/override (set-label label)
    (tellv (get-cocoa) setTitleWithMnemonic: #:type _NSString label))

  (define/override (gets-focus?) #f)

  (def/public-unimplemented get-font))
