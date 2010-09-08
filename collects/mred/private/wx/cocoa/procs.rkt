#lang racket/base
(require "../../syntax.rkt"
         racket/class
         racket/draw
         ffi/unsafe
         ffi/unsafe/objc
         "utils.rkt"
         "types.rkt"
         "frame.rkt"
         "../../lock.rkt"
         "../common/handlers.rkt")

(provide
 application-file-handler
 application-quit-handler
 application-about-handler
 application-pref-handler
 get-color-from-user
 get-font-from-user
 get-panel-background
 play-sound
 check-for-break
 find-graphical-system-path
 register-collecting-blit
 unregister-collecting-blit
 shortcut-visible-in-label?
 in-atomic-region
 set-menu-tester
 set-dialogs
 set-executer
 send-event
 file-creator-and-type
 run-printout
 get-double-click-time
 set-combo-box-font
 draw-tab
 draw-tab-base
 key-symbol-to-integer
 get-control-font-size
 cancel-quit
 fill-private-color
 flush-display
 write-resource
 get-resource
 display-origin
 display-size
 bell
 hide-cursor
 get-display-depth
 is-color-display?
 file-selector
 id-to-menu-item
 get-the-x-selection
 get-the-clipboard
 show-print-setup
 can-show-print-setup?
 get-highlight-background-color
 get-highlight-text-color)

(import-class NSScreen NSCursor)


(define-unimplemented get-color-from-user)
(define-unimplemented get-font-from-user)
(define (get-panel-background) (make-object color% "gray"))
(define-unimplemented play-sound)
(define-unimplemented check-for-break)
(define-unimplemented find-graphical-system-path)
(define (register-collecting-blit . args) (void))
(define (unregister-collecting-blit . args) (void))
(define (shortcut-visible-in-label? [x #f]) #f)
(define-unimplemented in-atomic-region)
(define (set-menu-tester proc)
  (void))
(define (set-dialogs . args)
  (void))
(define (set-executer proc)
  (void))
(define-unimplemented send-event)
(define-unimplemented file-creator-and-type)
(define (begin-refresh-sequence) (void))
(define (end-refresh-sequence) (void))
(define-unimplemented run-printout)
(define (get-double-click-time)
  500)
(define (set-combo-box-font f) (void))
(define-unimplemented draw-tab)
(define-unimplemented draw-tab-base)
(define-unimplemented key-symbol-to-integer)
(define (get-control-font-size) 13)
(define (cancel-quit) (void))
(define-unimplemented fill-private-color)
(define (flush-display) (void))
(define-unimplemented write-resource)
(define-unimplemented get-resource)

(define (display-origin xb yb all?)
  (set-box! xb 0)
  (set-box! yb 0))
(define (display-size xb yb v)
  (let ([f (tell #:type _NSRect (tell NSScreen mainScreen) frame)])
    (set-box! xb (->long (NSSize-width (NSRect-size f))))
    (set-box! yb (->long (NSSize-height (NSRect-size f))))))

(define-appkit NSBeep (_fun -> _void))  
(define (bell) (NSBeep))

(define (hide-cursor) 
  (tellv NSCursor setHiddenUntilMouseMoves: #:type _BOOL #t))

(define (get-display-depth) 32)
(define-unimplemented is-color-display?)
(define-unimplemented file-selector)
(define (id-to-menu-item id) id)
(define-unimplemented get-the-x-selection)
(define-unimplemented get-the-clipboard)
(define-unimplemented show-print-setup)
(define (can-show-print-setup?) #t)

;; ------------------------------------------------------------
;; Text & highlight color

(import-class NSColor)

(define-cocoa NSCalibratedRGBColorSpace _id)

(define (get-highlight-background-color)
  (let ([hi (tell (tell NSColor selectedTextBackgroundColor) 
                  colorUsingColorSpaceName: NSCalibratedRGBColorSpace)]
        [as-color (lambda (v)
                    (inexact->exact (floor (* 255.0 v))))])
    (make-object color%
                 (as-color
                  (tell #:type _CGFloat hi redComponent))
                 (as-color
                  (tell #:type _CGFloat hi greenComponent))
                 (as-color
                  (tell #:type _CGFloat hi blueComponent)))))

(define (get-highlight-text-color)
  #f)
