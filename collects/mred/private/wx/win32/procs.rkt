#lang racket/base
(require ffi/unsafe
         racket/class
	 "../../syntax.rkt"
	 "theme.rkt"
         "types.rkt"
         "utils.rkt"
         "const.rkt"
         "menu-item.rkt"
         "frame.rkt"
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
 get-control-font-size-in-pixels?
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
(define (register-collecting-blit . args) (void))
(define-unimplemented unregister-collecting-blit)
(define (shortcut-visible-in-label? [? #f]) #t)
(define-unimplemented location->window)
(define-unimplemented send-event)
(define-unimplemented file-creator-and-type)
(define-unimplemented run-printout)
(define (get-double-click-time) 500)
(define (get-control-font-size) (get-theme-font-size))
(define (get-control-font-size-in-pixels?) #t)
(define-unimplemented cancel-quit)
(define-unimplemented fill-private-color)
(define (flush-display) (void))
(define-unimplemented write-resource)
(define-unimplemented get-resource)
(define-unimplemented bell)

(define (hide-cursor) (void))

(define-unimplemented end-busy-cursor)
(define-unimplemented is-busy?)
(define-unimplemented begin-busy-cursor)
(define (get-display-depth) 32)
(define-unimplemented is-color-display?)
(define-unimplemented file-selector)
(define-unimplemented get-the-x-selection)
(define-unimplemented get-the-clipboard)
(define-unimplemented show-print-setup)
(define (can-show-print-setup?) #f)

(define (get-highlight-background-color)
  (let ([c (GetSysColor COLOR_HIGHLIGHT)])
    (make-object color% (GetRValue c) (GetGValue c) (GetBValue c))))
(define (get-highlight-text-color)
  (let ([c (GetSysColor COLOR_HIGHLIGHTTEXT)])
    (make-object color% (GetRValue c) (GetGValue c) (GetBValue c))))

(define-unimplemented make-screen-bitmap)

(define (check-for-break) #f)
