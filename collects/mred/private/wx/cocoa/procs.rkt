#lang racket/base
(require "../../syntax.rkt"
         racket/class
         racket/draw
         ffi/unsafe
         ffi/unsafe/objc
         "types.rkt"
         "../../lock.rkt"
         "../common/handlers.rkt")

(provide
 special-control-key
 special-option-key
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
 get-top-level-windows
 register-collecting-blit
 unregister-collecting-blit
 shortcut-visible-in-label?
 in-atomic-region
 set-menu-tester
 location->window
 set-dialogs
 set-executer
 send-event
 file-creator-and-type
 begin-refresh-sequence
 end-refresh-sequence
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
 end-busy-cursor
 is-busy?
 begin-busy-cursor
 get-display-depth
 is-color-display?
 file-selector
 id-to-menu-item
 get-the-x-selection
 get-the-clipboard
 show-print-setup
 can-show-print-setup?)

(import-class NSScreen NSCursor)


(define-unimplemented special-control-key)
(define-unimplemented special-option-key)
(define-unimplemented get-color-from-user)
(define-unimplemented get-font-from-user)
(define (get-panel-background) (make-object color% "gray"))
(define-unimplemented play-sound)
(define-unimplemented check-for-break)
(define-unimplemented find-graphical-system-path)
(define-unimplemented get-top-level-windows)
(define (register-collecting-blit . args) (void))
(define (unregister-collecting-blit . args) (void))
(define (shortcut-visible-in-label? [x #f]) #f)
(define-unimplemented in-atomic-region)
(define (set-menu-tester proc)
  (void))
(define-unimplemented location->window)
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
(define (get-control-font-size) 14)
(define-unimplemented cancel-quit)
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
  
(define-unimplemented bell)
(define (hide-cursor) 
  (tellv NSCursor setHiddenUntilMouseMoves: #:type _BOOL #t))

(define busy-count 0)
(define (end-busy-cursor) (as-entry (lambda () (set! busy-count (add1 busy-count)))))
(define (is-busy?) (positive? busy-count))
(define (begin-busy-cursor) (as-entry (lambda () (set! busy-count (sub1 busy-count)))))

(define (get-display-depth) 32)
(define-unimplemented is-color-display?)
(define-unimplemented file-selector)
(define-unimplemented id-to-menu-item)
(define-unimplemented get-the-x-selection)
(define-unimplemented get-the-clipboard)
(define-unimplemented show-print-setup)
(define (can-show-print-setup?) #t)
