#lang racket/base
(require ffi/unsafe
         "../../syntax.rkt"
         "../../lock.rkt"
         racket/class
         racket/draw
         "filedialog.rkt"
         "colordialog.rkt"
         "types.rkt"
         "utils.rkt"
         "style.rkt"
         "widget.rkt"
         "window.rkt"
         "dc.rkt"
         "printer-dc.rkt"
         "gl-context.rkt"
         "../common/printer.rkt"
         "../common/default-procs.rkt"
         "../common/handlers.rkt")

(provide
 special-control-key
 special-option-key
 get-color-from-user
 color-from-user-platform-mode
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
 key-symbol-to-integer
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
 show-print-setup
 can-show-print-setup?
 get-highlight-background-color
 get-highlight-text-color
 make-screen-bitmap
 make-gl-bitmap
 check-for-break)

(define-unimplemented get-font-from-user)
(define-unimplemented play-sound)
(define-unimplemented find-graphical-system-path)
(define-unimplemented location->window)
(define-unimplemented send-event)
(define-unimplemented key-symbol-to-integer)
(define-unimplemented cancel-quit)
(define-unimplemented write-resource)
(define-unimplemented get-resource)

(define (color-from-user-platform-mode) 'dialog)

(define (register-collecting-blit canvas x y w h on off on-x on-y off-x off-y)
  (send canvas register-collecting-blit x y w h on off on-x on-y off-x off-y))
(define (unregister-collecting-blit canvas)
  (send canvas unregister-collecting-blits))
(define (shortcut-visible-in-label? [mbar? #f]) #t)

(define run-printout (make-run-printout printer-dc%))

(define (get-double-click-time) 250)
(define (get-control-font-size) 10) ;; FIXME
(define (get-control-font-size-in-pixels?) #f) ;; FIXME

(define-gdk gdk_screen_get_width (_fun _GdkScreen -> _int))
(define-gdk gdk_screen_get_height (_fun _GdkScreen -> _int))

(define (display-origin x y all?) (set-box! x 0) (set-box! y 0))
(define (display-size w h all?)
  (let ([s (gdk_screen_get_default)])
    (set-box! w (gdk_screen_get_width s))
    (set-box! h (gdk_screen_get_height s))))
(define (get-display-depth) 32)

(define-gdk gdk_display_beep (_fun _GdkDisplay -> _void))
(define (bell) (gdk_display_beep (gdk_display_get_default)))

(define (hide-cursor) (void))

(define (is-color-display?) #t)

(define (id-to-menu-item i) i)
(define (can-show-print-setup?) #t)

(define (get-highlight-background-color)
  (let-values ([(r g b) (get-selected-background-color)])
    (make-object color% r g b)))

(define (get-highlight-text-color)
  (let-values ([(r g b) (get-selected-text-color)])
    (if (and (zero? r) (zero? g) (zero? b))
	#f
	(make-object color% r g b))))

(define/top (make-screen-bitmap [exact-positive-integer? w]
                                [exact-positive-integer? h])
  (if (eq? 'unix (system-type))
      (make-object x11-bitmap% w h #f)
      (make-object bitmap% w h #f #t)))

(define/top (make-gl-bitmap [exact-positive-integer? w]
                            [exact-positive-integer? h]
                            [gl-config% c])
  (let ([bm (make-object x11-bitmap% w h #f)])
    (create-and-install-gl-context bm c)
    bm))

(define (check-for-break) #f)
