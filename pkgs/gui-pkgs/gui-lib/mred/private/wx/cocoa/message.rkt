#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         racket/draw/private/bitmap
          "../../syntax.rkt"
          "../../lock.rkt"
          "window.rkt"
          "item.rkt"
          "utils.rkt"
          "types.rkt"
          "image.rkt")

(provide 
 (protect-out message%))

;; ----------------------------------------

(import-class NSTextField NSImageView NSWorkspace NSRunningApplication)

(define _OSType _uint32)

(define-cocoa NSFileTypeForHFSTypeCode (_fun _OSType -> _id))

(define (get-app-icon)
  (tell (tell NSRunningApplication currentApplication) icon))

(define (make-icon label)
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
    (tellv icon retain) 
    (tellv icon setSize: #:type _NSSize (make-NSSize 64 64))
    (unless (eq? label 'app)
      ;; Add badge:
      (let ([app-icon (get-icon 'app)])
        (tellv icon lockFocus)
        (tellv app-icon drawInRect: #:type _NSRect (make-NSRect (make-NSPoint 32 0)
                                                                (make-NSSize 32 32))
               fromRect: #:type _NSRect (make-NSRect (make-NSPoint 0 0)
                                                     (make-NSSize 64 64))
               operation: #:type _int 2 ; NSCompositeSourceOver 
               fraction: #:type _CGFloat 1.0)
        (tellv icon unlockFocus)))
    icon))

(define icons (make-hash))
(define (get-icon label)
  (or (hash-ref icons label #f)
      (let ([icon (atomically (make-icon label))])
        (hash-set! icons label icon)
        icon)))

;; ----------------------------------------

(define-objc-class RacketTextField NSTextField
  #:mixins (KeyMouseResponder CursorDisplayer)
  [wxb])

(define-objc-class RacketImageView NSImageView
  #:mixins (KeyMouseResponder CursorDisplayer)
  [wxb])

(defclass message% item%
  (init parent label
        x y
        style font)
  (inherit get-cocoa init-font)
  
  (super-new [parent parent]
             [cocoa (let* ([label (cond
                                   [(string? label) label]
                                   [(symbol? label) (get-icon label)]
                                   [else label])]
                           [cocoa
                            (if (string? label)
                                (as-objc-allocation
                                 (tell (tell RacketTextField alloc) init))
                                (as-objc-allocation
                                 (tell (tell RacketImageView alloc) init)))])
                      (cond
                       [(string? label)
                        (init-font cocoa font)
                        (tellv cocoa setSelectable: #:type _BOOL #f)
                        (tellv cocoa setEditable: #:type _BOOL #f)
                        (tellv cocoa setBordered: #:type _BOOL #f)
                        (tellv cocoa setDrawsBackground: #:type _BOOL #f)
                        (tellv cocoa setStringValue: #:type _NSString (strip-mnemonic label))
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
    (cond
     [(string? label)
      (tellv (get-cocoa) setStringValue: #:type _NSString (strip-mnemonic label))]
     [else
      (tellv (get-cocoa) setImage: (bitmap->image label))]))

  (define/override (gets-focus?) #f)

  (define/public (set-preferred-size)
    (tellv (get-cocoa) sizeToFit)
    #t)

  (def/public-unimplemented get-font))

