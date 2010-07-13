#lang scheme/base
(require ffi/objc
         scheme/foreign
         scheme/class
          "../../syntax.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "window.rkt"
         "../common/event.rkt"
         "image.rkt")
(unsafe!)
(objc-unsafe!)

(provide button%
         MyButton)

;; ----------------------------------------

(import-class NSButton NSView NSImageView)

(define-objc-class MyButton NSButton
  #:mixins (FocusResponder)
  [wx]
  (-a _void (clicked: [_id sender])
      (queue-window-event wx (lambda () (send wx clicked)))))

(defclass button% item%
  (init parent cb label x y w h style font
        [button-type #f])
  (init-field [event-type 'button])
  (inherit get-cocoa get-cocoa-window init-font)

  (define button-cocoa
    (let ([cocoa 
           (as-objc-allocation
            (tell (tell MyButton alloc) 
                  initWithFrame: #:type _NSRect (make-NSRect (make-NSPoint x y)
                                                             (make-NSSize w h))))])
      (when button-type
        (tellv cocoa setButtonType: #:type _int button-type))
      (unless button-type
        (tellv cocoa setBezelStyle: #:type _int (if (not (string? label))
                                                    NSRegularSquareBezelStyle
                                                    NSRoundedBezelStyle)))
      (cond
       [(string? label)
        (tellv cocoa setTitleWithMnemonic: #:type _NSString label)]
       [(send label ok?)
        (if button-type
            (tellv cocoa setTitle: #:type _NSString "")
            (tellv cocoa setImage: (bitmap->image label)))]
       [else
        (tellv cocoa setTitle: #:type _NSString "<bad>")])
      (init-font cocoa font)
      (tellv cocoa sizeToFit)
      cocoa))

  (define cocoa (if (and button-type
                         (not (string? label))
                         (send label ok?))
                    ;; Check-box image: need an view to join a button and an image view:
                    ;; (Could we use the NSImageButtonCell from the radio-box implementation
                    ;;  instead?)
                    (let* ([frame (tell #:type _NSRect button-cocoa frame)]
                           [new-width (+ (NSSize-width (NSRect-size frame))
                                         (send label get-width))]
                           [new-height (max (NSSize-height (NSRect-size frame))
                                            (send label get-height))])
                      (let ([cocoa (tell (tell NSView alloc)
                                         initWithFrame: #:type _NSRect
                                         (make-NSRect (NSRect-origin frame)
                                                      (make-NSSize new-width
                                                                   new-height)))]
                            [image-cocoa (as-objc-allocation
                                          (tell (tell NSImageView alloc) init))])
                        (tellv cocoa addSubview: button-cocoa)
                        (tellv cocoa addSubview: image-cocoa)
                        (tellv image-cocoa setImage: (bitmap->image label))
                        (tellv image-cocoa setFrame: #:type _NSRect 
                               (make-NSRect (make-NSPoint (NSSize-width (NSRect-size frame))
                                                          (quotient (- new-height
                                                                       (send label get-height))
                                                                    2))
                                            (make-NSSize (send label get-width)
                                                         (send label get-height))))
                        (tellv button-cocoa setFrame: #:type _NSRect
                               (make-NSRect (make-NSPoint 0 0)
                                            (make-NSSize new-width new-height)))
                        (set-ivar! button-cocoa wx this)
                        cocoa))
                    button-cocoa))

  (super-new [parent parent]
             [cocoa cocoa]
             [no-show? (memq 'deleted style)])

  (when (memq 'border style)
    (tellv (get-cocoa-window) setDefaultButtonCell: (tell button-cocoa cell)))

  (tellv button-cocoa setTarget: button-cocoa)
  (tellv button-cocoa setAction: #:type _SEL (selector clicked:))

  (define/override (get-cocoa-control) button-cocoa)

  (define/override (set-label label)
    (cond
     [(string? label)
      (tellv cocoa setTitleWithMnemonic: #:type _NSString label)]
     [else
      (tellv cocoa setImage: (bitmap->image label))]))
  
  (define callback cb)
  (define/public (clicked)
    (callback this (new control-event%
                        [event-type event-type]
                        [time-stamp (current-milliseconds)])))
  
  (def/public-unimplemented set-border))
