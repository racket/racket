#lang racket/base
(require ffi/unsafe
         "../../syntax.rkt"
         "../../lock.rkt"
         racket/class
         racket/draw
         "types.rkt"
         "utils.rkt"
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


(define-unimplemented special-control-key)
(define-unimplemented special-option-key)
(define-unimplemented get-color-from-user)
(define-unimplemented get-font-from-user)
(define (get-panel-background) (make-object color% "gray"))
(define-unimplemented play-sound)
(define-unimplemented check-for-break)
(define-unimplemented find-graphical-system-path)
(define (register-collecting-blit . args) (void))
(define (unregister-collecting-blit . args) (void))
(define (shortcut-visible-in-label? [mbar? #f]) #t)
(define-unimplemented in-atomic-region)
(define (set-menu-tester proc) (void))
(define-unimplemented location->window)
(define (set-dialogs . args) (void))
(define (set-executer e) (void))
(define-unimplemented send-event)
(define-unimplemented file-creator-and-type)
(define (begin-refresh-sequence) (void))
(define (end-refresh-sequence) (void))
(define-unimplemented run-printout)
(define (get-double-click-time) 250)
(define (set-combo-box-font f) (void))
(define-unimplemented draw-tab)
(define-unimplemented draw-tab-base)
(define-unimplemented key-symbol-to-integer)
(define (get-control-font-size) 10) ;; FIXME
(define-unimplemented cancel-quit)
(define-unimplemented fill-private-color)

(define _GdkDisplay (_cpointer 'GdkDisplay))
(define-gdk gdk_display_flush (_fun _GdkDisplay -> _void))
(define-gdk gdk_display_get_default (_fun -> _GdkDisplay))
(define (flush-display) (gdk_display_flush (gdk_display_get_default)))

(define-unimplemented write-resource)
(define-unimplemented get-resource)

(define _GdkScreen (_cpointer 'GdkScreen))
(define-gdk gdk_screen_get_default (_fun -> _GdkScreen))
(define-gdk gdk_screen_get_width (_fun _GdkScreen -> _int))
(define-gdk gdk_screen_get_height (_fun _GdkScreen -> _int))

(define (display-origin x y all?) (set-box! x 0) (set-box! y 0))
(define (display-size w h all?)
  (let ([s (gdk_screen_get_default)])
    (set-box! w (gdk_screen_get_width s))
    (set-box! h (gdk_screen_get_height s))))
(define (get-display-depth) 32)

(define-unimplemented bell)
(define (hide-cursor) (void))

(define busy-count 0)
(define (end-busy-cursor) (as-entry (lambda () (set! busy-count (add1 busy-count)))))
(define (is-busy?) (positive? busy-count))
(define (begin-busy-cursor) (as-entry (lambda () (set! busy-count (sub1 busy-count)))))

(define-unimplemented is-color-display?)
(define-unimplemented file-selector)
(define (id-to-menu-item i) i)
(define-unimplemented get-the-x-selection)
(define-unimplemented get-the-clipboard)
(define-unimplemented show-print-setup)
(define (can-show-print-setup?) #f)
