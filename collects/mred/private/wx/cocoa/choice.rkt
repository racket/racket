#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
          "../../syntax.rkt"
         "item.rkt"
         "types.rkt"
         "const.rkt"
         "utils.rkt"
         "window.rkt"
         "../common/event.rkt")

(provide 
 (protect-out choice%))

;; ----------------------------------------

(import-class NSPopUpButton)

(define-objc-class RacketPopUpButton NSPopUpButton 
  #:mixins (FocusResponder KeyMouseResponder CursorDisplayer)
  [wxb]
  (-a _void (clicked: [_id sender])
      (queue-window*-event wxb (lambda (wx) (send wx clicked)))))

(defclass choice% item%
  (init parent cb label
        x y w h
        choices style font)
  (inherit get-cocoa init-font register-as-child)

  (super-new [parent parent]
             [cocoa 
              (let ([cocoa 
                     (as-objc-allocation
                      (tell (tell RacketPopUpButton alloc) 
                            initWithFrame: #:type _NSRect (make-NSRect (make-init-point x y)
                                                                       (make-NSSize w h))
                            pullsDown: #:type _BOOL #f))])
                (for ([lbl (in-list choices)]
                      [i (in-naturals)])
                  (tellv cocoa 
                         insertItemWithTitle: #:type _NSString lbl 
                         atIndex: #:type _NSInteger i))
                (init-font cocoa font)
                (tellv cocoa sizeToFit)
                (tellv cocoa setTarget: cocoa)
                (tellv cocoa setAction: #:type _SEL (selector clicked:))
                cocoa)]
             [callback cb]
             [no-show? (memq 'deleted style)])

  (define callback cb)
  (define/public (clicked)
    (callback this (new control-event%
                        [event-type 'choice]
                        [time-stamp (current-milliseconds)])))
  
  (define/public (set-selection i)
    (tellv (get-cocoa) selectItemAtIndex: #:type _NSInteger i))
  (define/public (get-selection)
    (tell #:type _NSInteger (get-cocoa) indexOfSelectedItem))
  (define/public (number)
    (tell #:type _NSInteger (get-cocoa) numberOfItems))
  (define/public (clear)
    (tellv (get-cocoa) removeAllItems))
  (define/public (append lbl)
    (tellv (get-cocoa)
           insertItemWithTitle: #:type _NSString lbl 
           atIndex: #:type _NSInteger (number)))
  (define/public (delete i)
    (tellv (get-cocoa) removeItemAtIndex: #:type _NSInteger i))

  (define/override (maybe-register-as-child parent on?)
    (register-as-child parent on?)))
