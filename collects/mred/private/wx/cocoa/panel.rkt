#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         "../../syntax.rkt"
         "types.rkt"
         "utils.rkt"
         "cg.rkt"
         "window.rkt")

(provide 
 (protect-out panel%
              panel-mixin

              FrameView))

(import-class NSView NSGraphicsContext)

(define-objc-class RacketPanelView NSView
  #:mixins (KeyMouseTextResponder CursorDisplayer)
  [wxb])

(define-objc-class FrameView NSView 
  []
  (-a _void (drawRect: [_NSRect r])
      (let ([ctx (tell NSGraphicsContext currentContext)])
        (tellv ctx saveGraphicsState)
        (let ([cg (tell #:type _CGContextRef ctx graphicsPort)]
              [r (tell #:type _NSRect self bounds)])
          (CGContextSetRGBFillColor cg 0 0 0 1.0)
          (CGContextAddRect cg r)
          (CGContextStrokePath cg))
        (tellv ctx restoreGraphicsState))))

(define (panel-mixin %)
  (class %
    (inherit register-as-child on-new-child
             is-window-enabled? get-cocoa)

    (define lbl-pos 'horizontal)
    (define children null)

    (super-new)
    
    (define/public (get-label-position) lbl-pos)
    (define/public (set-label-position pos) (set! lbl-pos pos))

    (define/public (adopt-child p)
      ;; in atomic mode
      (send p set-parent this))

    (define/override (fix-dc)
      (super fix-dc)
      (for ([child (in-list children)])
        (send child fix-dc)))

    (define/override (hide-children)
      (for ([child (in-list children)])
        (send child hide-children)))

    (define/override (show-children)
      (for ([child (in-list children)])
        (send child show-children)))

    (define/override (fixup-locations-children)
      (for ([child (in-list children)])
        (send child fixup-locations-children)))
    
    (define/override (paint-children)
      (for ([child (in-list children)])
        (send child paint-children)))

    (define/override (children-accept-drag on?)
      (for ([child (in-list children)])
        (send child child-accept-drag on?)))

    (define/override (enable-window on?)
      (super enable-window on?)
      (let ([on? (and on? (is-window-enabled?))])
        (for ([child (in-list children)])
          (send child enable-window on?))))
    
    (define/override (set-size x y w h)
      (super set-size x y w h)
      (fix-dc))
    
    (define/override (maybe-register-as-child parent on?)
      (register-as-child parent on?))
    
    (define/override (register-child child on?)
      (let ([now-on? (and (memq child children) #t)])
        (unless (eq? on? now-on?)
          (set! children 
                (if on?
                    (cons child children)
                    (remq child children)))
          (on-new-child child on?))))

    (define/override (show on?)
      (super show on?)
      (fix-dc))

    (define/override (refresh-all-children)
      (for ([child (in-list children)])
        (send child refresh)))
    
    (define/public (set-item-cursor x y) (void))))

(defclass panel% (panel-mixin window%)
  (inherit get-cocoa)
  (init parent
        x y w h
        style
        label)

  (define has-border? (memq 'border style))

  (super-new [parent parent]
             [cocoa
              (as-objc-allocation
               (tell (tell (if has-border? FrameView RacketPanelView) alloc)
                     initWithFrame: #:type _NSRect (make-NSRect (make-init-point x y)
                                                                (make-NSSize (max (if has-border? 3 1) w) 
                                                                             (max (if has-border? 3 1) h)))))]
             [no-show? (memq 'deleted style)])

  (define content-cocoa
    (and has-border?
         (let* ([c (get-cocoa)]
                [f (tell #:type _NSRect c frame)])
           (as-objc-allocation
            (tell (tell RacketPanelView alloc)
                  initWithFrame: #:type _NSRect (make-NSRect (make-init-point 1 1)
                                                             (let ([s (NSRect-size f)])
                                                               (make-NSSize (max 1 (- (NSSize-width s) 2))
                                                                            (max 1 (- (NSSize-height s) 2))))))))))
  (when has-border?
    (let ([cocoa (get-cocoa)])
      (tell #:type _void cocoa addSubview: content-cocoa)
      (set-ivar! content-cocoa wxb (->wxb this))))

  (define/override (get-cocoa-content)
    (if has-border?
        content-cocoa
        (super get-cocoa-content)))

  (define/override (set-size x y w h)
    (super set-size x y w h)
    (when has-border?
      (tellv content-cocoa setFrame: #:type _NSRect (make-NSRect (make-NSPoint 1 1)
                                                                (make-NSSize (max 1 (- w 2)) (max 1 (- h 2))))))))


