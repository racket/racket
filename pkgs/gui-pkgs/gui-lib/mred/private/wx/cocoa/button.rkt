#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         racket/class
          "../../syntax.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "window.rkt"
         "../common/event.rkt"
         "image.rkt")

(provide 
 (protect-out button%
              core-button%
              RacketButton))

;; ----------------------------------------

(import-class NSButton NSView NSImageView)

(define MIN-BUTTON-WIDTH 72)
(define BUTTON-EXTRA-WIDTH 12)

(define NSSmallControlSize 1)
(define NSMiniControlSize 2)

(define-objc-class RacketButton NSButton
  #:mixins (FocusResponder KeyMouseResponder CursorDisplayer)
  [wxb]
  (-a _void (clicked: [_id sender])
      (queue-window*-event wxb (lambda (wx) (send wx clicked)))))

(define NSImageLeft 2)
(define NSImageRight 3)
(define NSImageBelow 4)
(define NSImageAbove 5)

(defclass core-button% item%
  (init parent cb label x y w h style font
        [button-type #f])
  (init-field [event-type 'button])
  (inherit get-cocoa get-cocoa-window init-font
           register-as-child get-wx-window)

  (define button-cocoa
    (let ([cocoa 
           (as-objc-allocation
            (tell (tell RacketButton alloc) 
                  initWithFrame: #:type _NSRect (make-NSRect (make-init-point x y)
                                                             (make-NSSize w h))))])
      (when button-type
        (tellv cocoa setButtonType: #:type _int button-type))
      (unless button-type
        (tellv cocoa setBezelStyle: #:type _int (if (or (not (string? label))
                                                        (regexp-match? #rx"\n" label))
                                                    NSRegularSquareBezelStyle
                                                    NSRoundedBezelStyle)))
      (cond
       [(string? label)
        (tellv cocoa setTitle: #:type _NSString (strip-mnemonic label))]
       [else
        (if button-type
            (tellv cocoa setTitle: #:type _NSString "")
            (begin
              (when (pair? label)
                (tellv cocoa setTitle: #:type _NSString (cadr label))
                (tellv cocoa setImagePosition: #:type _NSInteger 
                       (case (caddr label)
                         [(left) NSImageLeft]
                         [(right) NSImageRight]
                         [(top) NSImageAbove]
                         [(bottom) NSImageBelow])))
              (tellv cocoa setImage: (bitmap->image (if (pair? label) (car label) label)))))])
      (init-font cocoa font)
      (tellv cocoa sizeToFit)
      (when (and (eq? event-type 'button)
                 (or (string? label)
                     (pair? label)))
        (when font
          (let ([n (send font get-point-size)])
            ;; If the font is small, adjust the control size:
            (when (n . < . sys-font-size)
              (tellv (tell cocoa cell) 
                     setControlSize: #:type _int 
                     (if (n . < . (- sys-font-size 2))
                         NSMiniControlSize
                         NSSmallControlSize))
              (tellv cocoa sizeToFit))
            ;; If the font is big, use a scalable control shape:
            (when (n . > . (+ sys-font-size 2))
              (tellv cocoa setBezelStyle: #:type _int NSRegularSquareBezelStyle)
              (tellv cocoa sizeToFit))))
        (let ([frame (tell #:type _NSRect cocoa frame)])
          (tellv cocoa setFrame: #:type _NSRect 
                 (make-NSRect (NSRect-origin frame)
                              (make-NSSize (+ BUTTON-EXTRA-WIDTH
                                              (max MIN-BUTTON-WIDTH
                                                   (NSSize-width (NSRect-size frame))))
                                           (NSSize-height (NSRect-size frame)))))))
      cocoa))

  (when (pair? label)
    ;; It looks better to add extra padding around the button:
    (let ([f (tell #:type _NSRect button-cocoa frame)])
      (tellv button-cocoa setFrame: #:type _NSRect 
             (make-NSRect
              (NSRect-origin f)
              (make-NSSize (+ (NSSize-width (NSRect-size f)) 2)
                           (+ (NSSize-height (NSRect-size f)) 4))))))
  
  (define-values (cocoa image-cocoa)
    (if (and button-type
             (not (string? label)))
        ;; Check-box image: need a view to join a button and an image view:
        ;; (Could we use the NSImageButtonCell from the radio-box implementation
        ;;  instead?)
        (let* ([frame (tell #:type _NSRect button-cocoa frame)]
               [new-width (+ (NSSize-width (NSRect-size frame))
                             (send label get-width))]
               [new-height (max (NSSize-height (NSRect-size frame))
                                (send label get-height))])
          (let ([cocoa (as-objc-allocation
                        (tell (tell NSView alloc)
                              initWithFrame: #:type _NSRect
                              (make-NSRect (NSRect-origin frame)
                                           (make-NSSize new-width
                                                        new-height))))]
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
            (set-ivar! button-cocoa wxb (->wxb this))
            (values cocoa image-cocoa)))
        (values button-cocoa #f)))

  (super-new [parent parent]
             [cocoa cocoa]
             [no-show? (memq 'deleted style)]
             [callback cb])

  (define default-button? (memq 'border style))
  (define/override (show-children)
    (when default-button?
      (send (get-wx-window) add-possible-default this)))
  (define/override (hide-children)
    (when default-button?
      (send (get-wx-window) remove-possible-default this)))

  (define/override (enable-window on?)
    (super enable-window on?)
    (when default-button?
      (send (get-wx-window) queue-default-button-check)))

  (define/public (be-default) ; called by frame, only when the button is shown
    ;; return #t to indicate succes, #f to let some other button take over
    (and (tell #:type _BOOL button-cocoa isEnabled)
         (begin
           (tellv (get-cocoa-window) setDefaultButtonCell: (tell button-cocoa cell))
           #t)))

  (tellv button-cocoa setTarget: button-cocoa)
  (tellv button-cocoa setAction: #:type _SEL (selector clicked:))

  (define/override (get-cocoa-control) button-cocoa)

  (define/override (maybe-register-as-child parent on?)
    (register-as-child parent on?))
    
  (define/override (set-label label)
    (cond
     [(string? label)
      (tellv cocoa setTitle: #:type _NSString (strip-mnemonic label))]
     [else
      (tellv (or image-cocoa cocoa) setImage: (bitmap->image label))]))
  
  (define callback cb)
  (define/public (clicked)
    (callback this (new control-event%
                        [event-type event-type]
                        [time-stamp (current-milliseconds)])))
  
  (def/public-unimplemented set-border))

(define button%
  (class core-button% (super-new)))

