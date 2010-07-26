#lang scheme/base
(require scheme/class
         scheme/foreign
         ffi/objc
          "../../syntax.rkt"
         "item.rkt"
         "button.rkt"
         "types.rkt"
         "const.rkt"
         "utils.rkt"
         "window.rkt"
         "../common/event.rkt"
         "image.rkt")
(unsafe!)
(objc-unsafe!)

(provide radio-box%)

;; ----------------------------------------

(import-class NSMatrix NSButtonCell)

(define NSRadioModeMatrix 0)

(define-objc-class MyMatrix NSMatrix
  #:mixins (FocusResponder)
  [wx]
  (-a _void (clicked: [_id sender])
      (queue-window-event wx (lambda () (send wx clicked)))))

(define-objc-class MyImageButtonCell NSButtonCell
  [img]
  [-a _NSSize (cellSize)
      (let ([s (super-tell #:type _NSSize cellSize)])
        (if img
            (let ([s2 (tell #:type _NSSize img size)])
              (make-NSSize (+ (NSSize-width s) (NSSize-width s2))
                           (max (NSSize-height s) (NSSize-height s2))))
            s))]
  [-a _void (drawInteriorWithFrame: [_NSRect f] inView: [_id view])
      (super-tell #:type _void drawInteriorWithFrame: #:type _NSRect f inView: view)
      (when img
        (let ([size (tell #:type _NSSize img size)])
          (tellv img 
                 drawInRect: #:type _NSRect (make-NSRect
                                             (make-NSPoint 
                                              (+ (NSPoint-x (NSRect-origin f))
                                                 (- (NSSize-width (NSRect-size f))
                                                    (NSSize-width size)))
                                              (+ (NSPoint-y (NSRect-origin f))
                                                 (quotient (- (NSSize-height (NSRect-size f))
                                                              (NSSize-height size))
                                                           2)))
                                             size)
                 fromRect: #:type _NSRect (make-NSRect (make-NSPoint 0 0) size)
                 operation: #:type _int 1
                 fraction: #:type _CGFloat 1.0)))])

(defclass radio-box% item%
  (init parent cb label
        x y w h
        labels
        val
        style
        font)
  (inherit get-cocoa set-focus)

  (define horiz? (and (memq 'horizontal style) #t))

  (super-new [parent parent]
             [cocoa
              (let ([cocoa
                     (as-objc-allocation
                      (tell (tell MyMatrix alloc) 
                            initWithFrame: #:type _NSRect (make-NSRect (make-NSPoint x y)
                                                                       (make-NSSize w h))
                            mode: #:type _int NSRadioModeMatrix
                            cellClass: (if (andmap string? labels)
                                           NSButtonCell
                                           MyImageButtonCell)
                            numberOfRows: #:type _NSInteger (if horiz? 1 (length labels))
                            numberOfColumns: #:type _NSInteger (if horiz? (length labels) 1)))])
                (for ([label (in-list labels)]
                      [i (in-naturals)])
                  (let ([button (tell cocoa 
                                      cellAtRow: #:type _NSInteger (if horiz? 0 i)
                                      column: #:type _NSInteger (if horiz? i 0))])
                    (if (and (not (string? label))
                             (send label ok?))
                        (begin
                          (tellv button setTitle: #:type _NSString "")
                          (set-ivar! button img (bitmap->image label)))
                        (tellv button setTitleWithMnemonic: #:type _NSString (if (string? label)
                                                                                 label
                                                                                 "<bad>")))
                    (tellv button setButtonType: #:type _int NSRadioButton)))
                (tellv cocoa sizeToFit)
                (tellv cocoa setTarget: cocoa)
                (tellv cocoa setAction: #:type _SEL (selector clicked:))
                cocoa)]
             [callback cb]
             [no-show? (memq 'deleted style)])

  (define count (length labels))

  (define callback cb)
  (define/public (clicked)
    (callback this (new control-event%
                        [event-type 'radio-box]
                        [time-stamp (current-milliseconds)])))

  (define/public (button-focus i)
    (if (= i -1)
        0
        (set-focus)))

  (define/public (set-selection i)
    (tellv (get-cocoa) selectCellAtRow: #:type _NSInteger (if horiz? 0 i)
           column: #:type _NSInteger (if horiz? i 0)))
  (define/public (get-selection)
    (if horiz?
        (tell #:type _NSInteger (get-cocoa) selectedColumn)
        (tell #:type _NSInteger (get-cocoa) selectedRow)))
  (define/public (number) count))
