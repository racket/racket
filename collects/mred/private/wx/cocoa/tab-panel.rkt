#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         racket/runtime-path
         "../../syntax.rkt"
         "types.rkt"
         "utils.rkt"
         "window.rkt"
         "panel.rkt"
         "queue.rkt"
         "../common/event.rkt"
         "../common/procs.rkt"
         (for-syntax racket/base))

(provide 
 (protect-out tab-panel%))

(define-runtime-path psm-tab-bar-dir
  '(so "PSMTabBarControl.framework"))

;; Load PSMTabBarControl:
(void (ffi-lib (build-path psm-tab-bar-dir "PSMTabBarControl")))
(define NSNoTabsNoBorder 6)

(define NSDefaultControlTint 0)
(define NSClearControlTint 7)

(import-class NSView NSTabView NSTabViewItem PSMTabBarControl)
(import-protocol NSTabViewDelegate)

(define-objc-class MyTabView NSTabView
  #:mixins (FocusResponder KeyMouseResponder CursorDisplayer)
  [wxb]
  (-a _void (tabView: [_id cocoa] didSelectTabViewItem: [_id item-cocoa])
      (queue-window*-event wxb (lambda (wx) (send wx do-callback)))))

(define-objc-class MyPSMTabBarControl PSMTabBarControl
  #:mixins (FocusResponder KeyMouseResponder CursorDisplayer)
  [wxb]
  (-a _void (tabView: [_id cocoa] didSelectTabViewItem: [_id item-cocoa])
      (super-tell #:type _void tabView: cocoa didSelectTabViewItem: item-cocoa)
      (queue-window*-event wxb (lambda (wx) (send wx do-callback)))))

(defclass tab-panel% (panel-mixin window%)
  (init parent
        x y w h
        style
        labels)
  (inherit get-cocoa register-as-child
           is-window-enabled?
           block-mouse-events)

  (define tabv-cocoa (as-objc-allocation
                      (tell (tell MyTabView alloc) init)))
  (define cocoa (if (not (memq 'border style))
                    (as-objc-allocation
                     (tell (tell NSView alloc) init))
                    tabv-cocoa))

  (define control-cocoa
    (and (not (memq 'border style))
         (let ([i (as-objc-allocation
                   (tell (tell MyPSMTabBarControl alloc)
                         initWithFrame: #:type _NSRect (make-NSRect (make-NSPoint 0 0)
                                                                    (make-NSSize 200 22))))])
           (tellv cocoa addSubview: i)
           (tellv cocoa addSubview: tabv-cocoa)
           (tellv tabv-cocoa setDelegate: i)
           (tellv tabv-cocoa setTabViewType: #:type _int NSNoTabsNoBorder)
           (tellv i setTabView: tabv-cocoa)
           (tellv i setStyleNamed: #:type _NSString "Aqua")
           ;;(tellv i setSizeCellsToFit: #:type _BOOL #t)
           (tellv i setDisableTabClose: #:type _BOOL #t)
           i)))

  (define item-cocoas
    (for/list ([lbl (in-list labels)])
      (let ([item (as-objc-allocation
                   (tell (tell NSTabViewItem alloc) initWithIdentifier: #f))])
        (tellv item setLabel: #:type _NSString (label->plain-label lbl))
        (tellv tabv-cocoa addTabViewItem: item)
        item)))
  (if control-cocoa
      (tellv cocoa setFrame: #:type _NSRect (make-NSRect (make-init-point x y) 
                                                         (make-NSSize 50 22)))
      (let ([sz (tell #:type _NSSize tabv-cocoa minimumSize)])
        (tellv tabv-cocoa setFrame: #:type _NSRect (make-NSRect (make-init-point x y) sz))
        (tellv tabv-cocoa setDelegate: tabv-cocoa)))
  
  (define content-cocoa 
    (as-objc-allocation
     (tell (tell NSView alloc)
           initWithFrame: #:type _NSRect (tell #:type _NSRect tabv-cocoa contentRect))))
  (tellv tabv-cocoa addSubview: content-cocoa)

  (define/override (get-cocoa-content) content-cocoa)
  (define/override (get-cocoa-cursor-content) tabv-cocoa)
  (define/override (set-size x y w h)
    (super set-size x y w h)
    (when control-cocoa
      (let ([r (tell #:type _NSRect cocoa frame)])
        (tellv control-cocoa setFrame: #:type _NSRect (make-NSRect (make-NSPoint
                                                                    0
                                                                    (- (NSSize-height (NSRect-size r)) 22))
                                                                   (make-NSSize
                                                                    (NSSize-width (NSRect-size r))
                                                                    22)))
        (tellv tabv-cocoa setFrame: #:type _NSRect (make-NSRect (make-NSPoint 0 0)
                                                                (make-NSSize
                                                                 (NSSize-width (NSRect-size r))
                                                                 (- (NSSize-height (NSRect-size r)) 22))))))
    (tellv content-cocoa setFrame: #:type _NSRect (tell #:type _NSRect tabv-cocoa contentRect)))

  (define/public (set-label i str)
    (tellv (list-ref item-cocoas i) setLabel: #:type _NSString (label->plain-label str)))
  
  (define/public (set-selection i)
    (tellv tabv-cocoa selectTabViewItem: (list-ref item-cocoas i)))
  (define/public (get-selection)
    (item->index (tell tabv-cocoa selectedTabViewItem)))

  (define (item->index tv)
    (for/or ([c (in-list item-cocoas)]
             [i (in-naturals)])
      (and (ptr-equal? c tv) i)))

  (public [append* append])
  (define (append* lbl)
    (let ([item (as-objc-allocation
                 (tell (tell NSTabViewItem alloc) initWithIdentifier: #f))])
      (tellv item setLabel: #:type _NSString (label->plain-label lbl))
      (tellv tabv-cocoa addTabViewItem: item)
      (set! item-cocoas (append item-cocoas (list item)))))

  (define/public (delete i)
    (let ([item-cocoa (list-ref item-cocoas i)])
      (tellv tabv-cocoa removeTabViewItem: item-cocoa)
      (set! item-cocoas (remq item-cocoa item-cocoas))))

  (define/public (set choices)
    (for ([item-cocoa (in-list item-cocoas)])
      (tellv tabv-cocoa removeTabViewItem: item-cocoa))
    (set! item-cocoas null)
    (for ([lbl (in-list choices)])
      (append* lbl)))

  (define callback void)
  (define/public (set-callback cb) (set! callback cb))
  (define/public (do-callback)
    (callback this (new control-event%
                        [event-type 'tab-panel]
                        [time-stamp (current-milliseconds)])))

  (super-new [parent parent]
             [cocoa cocoa]
             [no-show? (memq 'deleted style)])
  
  (when control-cocoa
    (set-ivar! control-cocoa wxb (->wxb this)))

  (define/override (enable-window on?)
    (super enable-window on?)
    (let ([on? (and on? (is-window-enabled?))])
      (block-mouse-events (not on?))
      (tellv tabv-cocoa setControlTint: #:type _int
             (if on? NSDefaultControlTint NSClearControlTint))
      (when control-cocoa
        (tellv control-cocoa setEnabled: #:type _BOOL on?))))

  (define/override (maybe-register-as-child parent on?)
    (register-as-child parent on?)))

