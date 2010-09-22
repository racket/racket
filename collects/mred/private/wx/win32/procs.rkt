#lang racket/base
(require racket/class
	 "../../syntax.rkt"
	 "theme.rkt"
	 racket/draw)

(provide
 special-control-key
 special-option-key
 get-color-from-user
 get-font-from-user
 get-panel-background
 play-sound
 find-graphical-system-path
 register-collecting-blit
 unregister-collecting-blit
 shortcut-visible-in-label?
 location->window
 send-event
 file-creator-and-type
 run-printout
 get-double-click-time
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
 get-highlight-text-color
 make-screen-bitmap
 check-for-break)

(define-unimplemented special-control-key)
(define-unimplemented special-option-key)
(define-unimplemented get-color-from-user)
(define-unimplemented get-font-from-user)
(define (get-panel-background) (make-object color% "gray"))
(define-unimplemented play-sound)
(define-unimplemented find-graphical-system-path)
(define-unimplemented register-collecting-blit)
(define-unimplemented unregister-collecting-blit)
(define (shortcut-visible-in-label? [? #f]) #t)
(define-unimplemented location->window)
(define-unimplemented send-event)
(define-unimplemented file-creator-and-type)
(define-unimplemented run-printout)
(define (get-double-click-time) 500)
(define (get-control-font-size) (get-theme-font-size))
(define-unimplemented cancel-quit)
(define-unimplemented fill-private-color)
(define-unimplemented flush-display)
(define-unimplemented write-resource)
(define-unimplemented get-resource)
(define (display-origin xb yb ?)
  (set-box! xb 0)
  (set-box! yb 0))
(define (display-size xb yb ?)
  (set-box! xb 1024)
  (set-box! yb 768))
(define-unimplemented bell)

(define (hide-cursor) (void))

(define-unimplemented end-busy-cursor)
(define-unimplemented is-busy?)
(define-unimplemented begin-busy-cursor)
(define-unimplemented get-display-depth)
(define-unimplemented is-color-display?)
(define-unimplemented file-selector)
(define-unimplemented id-to-menu-item)
(define-unimplemented get-the-x-selection)
(define-unimplemented get-the-clipboard)
(define-unimplemented show-print-setup)
(define-unimplemented can-show-print-setup?)

(define (get-highlight-background-color)
  (make-object color% 0 0 0))
(define (get-highlight-text-color)
  (make-object color% 255 255 255))

(define-unimplemented make-screen-bitmap)

(define (check-for-break) #f)
