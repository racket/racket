#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
          "../../syntax.rkt"
          "types.rkt"
          "utils.rkt"
          "window.rkt"
          "panel.rkt")

(provide 
 (protect-out group-panel%))

(import-class NSBox)

(define-objc-class RacketBox NSBox
  #:mixins (FocusResponder KeyMouseResponder CursorDisplayer)
  [wxb])

(defclass group-panel% (panel-mixin window%)
  (init parent
        x y w h
        style
        label)
  (inherit get-cocoa)

  (super-new [parent parent]
             [cocoa
              (let ([cocoa (as-objc-allocation
                            (tell (tell RacketBox alloc) init))])
                (when label
                  (tellv cocoa setTitle: #:type _NSString label)
                  (tellv cocoa sizeToFit))
                cocoa)]
             [no-show? (memq 'deleted style)])

  (define/override (get-cocoa-content) 
    (tell (get-cocoa) contentView))
  (define/override (get-cocoa-cursor-content) 
    (get-cocoa))

  (define/public (set-label l)
    (tellv (get-cocoa) setTitle: #:type _NSString l)))
