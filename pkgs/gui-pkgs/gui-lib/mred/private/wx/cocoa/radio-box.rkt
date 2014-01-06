#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
          "../../syntax.rkt"
         "item.rkt"
         "button.rkt"
         "types.rkt"
         "const.rkt"
         "utils.rkt"
         "window.rkt"
         "../common/event.rkt"
         "image.rkt")

(provide 
 (protect-out radio-box%))

;; ----------------------------------------

(import-class NSMatrix NSButtonCell)

(define NSRadioModeMatrix 0)
(define NSListModeMatrix 2)

(define-objc-class RacketMatrix NSMatrix
  #:mixins (FocusResponder KeyMouseResponder CursorDisplayer)
  [wxb]
  (-a _void (clicked: [_id sender])
      ;; In case we were in 0-item mode, switch to Radio mode to
      ;; ensure that only one button is selected:
      (tellv self setAllowsEmptySelection: #:type _BOOL #f)
      (queue-window*-event wxb (lambda (wx) (send wx clicked)))))

(define-objc-class RacketImageButtonCell NSButtonCell
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
  (inherit get-cocoa set-focus init-font register-as-child)

  (define horiz? (and (memq 'horizontal style) #t))

  (super-new [parent parent]
             [cocoa
              (let ([cocoa
                     (as-objc-allocation
                      (tell (tell RacketMatrix alloc) 
                            initWithFrame: #:type _NSRect (make-NSRect (make-init-point x y)
                                                                       (make-NSSize w h))
                            mode: #:type _int NSRadioModeMatrix
                            cellClass: (if (andmap string? labels)
                                           NSButtonCell
                                           RacketImageButtonCell)
                            numberOfRows: #:type _NSInteger (if horiz? 1 (length labels))
                            numberOfColumns: #:type _NSInteger (if horiz? (length labels) 1)))])
                (tellv cocoa setIntercellSpacing: #:type _NSSize (make-NSSize 2 2))
                (for ([label (in-list labels)]
                      [i (in-naturals)])
                  (let ([button (tell cocoa 
                                      cellAtRow: #:type _NSInteger (if horiz? 0 i)
                                      column: #:type _NSInteger (if horiz? i 0))])
                    (if (not (string? label))
                        (begin
                          (tellv button setTitle: #:type _NSString "")
                          (set-ivar! button img (bitmap->image label)))
                        (begin
                          (init-font button font)
                          (tellv button setTitle: #:type _NSString (if (string? label)
                                                                       (strip-mnemonic label)
                                                                       "<bad>"))))
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
        (if horiz?
            (tell #:type _NSInteger (get-cocoa) selectedColumn)
            (tell #:type _NSInteger (get-cocoa) selectedRow))
        (let ([which (get-selection)])
          (set-focus)
          (tellv (get-cocoa) 
                 selectCellAtRow: #:type _NSInteger (if horiz? 0 i) 
                 column: #:type _NSInteger (if horiz? i 0))
          (unless (equal? which (get-selection))
            (queue-window-event this (lambda () (clicked)))))))

  (define/private (get-button i)
    (tell (get-cocoa)
          cellAtRow: #:type _NSUInteger (if horiz? 0 i)
          column: #:type _NSUInteger (if horiz? i 0)))

  (define/public (enable-button i on?)
    (tellv (get-button i) setEnabled: #:type _BOOL on?))

  (define/public (set-selection i)
    (if (= i -1)
        (begin
          (tellv (get-cocoa) setAllowsEmptySelection: #:type _BOOL #t)
          (tellv (get-cocoa) deselectAllCells))
        (begin
          (tellv (get-cocoa) selectCellAtRow: #:type _NSInteger (if horiz? 0 i)
                 column: #:type _NSInteger (if horiz? i 0))
          (tellv (get-cocoa) setAllowsEmptySelection: #:type _BOOL #f))))
  (define/public (get-selection)
    (let ([c (tell (get-cocoa) selectedCell)]
          [pos (if horiz?
                   (tell #:type _NSInteger (get-cocoa) selectedColumn)
                   (tell #:type _NSInteger (get-cocoa) selectedRow))])
      (if (and c
               (positive? (tell #:type _NSInteger c state)))
          pos
          -1)))
  (define/public (number) count)

  (define/override (maybe-register-as-child parent on?)
    (register-as-child parent on?)))
