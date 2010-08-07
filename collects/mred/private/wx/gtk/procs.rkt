#lang racket/base
(require ffi/unsafe
         "../../syntax.rkt"
         "../../lock.rkt"
         racket/class
         racket/draw
         racket/match
         "types.rkt"
         "utils.rkt"
         "widget.rkt"
         "../common/handlers.rkt"
         "../common/queue.rkt")

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

(define _GtkFileChooserDialog _GtkWidget)
(define _GtkFileChooser (_cpointer 'GtkFileChooser))
(define _GtkFileChooserAction 
  (_enum (list 'open 'save 'select-folder 'create-folder)))

(define _GtkResponse
  (_enum 
   '(none = -1
     reject = -2
     accept = -3
     delete-event = -4
     ok = -5
     cancel = -6
     close = -7
     yes = -8
     no = -9
     apply = -10
     help = -11)
   _fixint))
;; FIXME: really there are varargs here, but we don't need them for
;; our purposes
(define-gtk gtk_file_chooser_dialog_new 
  (_fun _string (_or-null _GtkWindow) 
        _GtkFileChooserAction
        _string _GtkResponse
        _string _GtkResponse
        (_or-null _pointer)
        -> _GtkFileChooserDialog))
;; FIXME - should really be _GtkDialog but no subtyping
(define-gtk gtk_dialog_run (_fun _GtkFileChooserDialog -> _int))
;; FIXME ;; these should really be _GtkFileChooser but no subtyping
(define-gtk gtk_file_chooser_get_filename 
  (_fun _GtkFileChooserDialog -> _gpath/free))
(define-gtk gtk_file_chooser_get_filenames
  (_fun _GtkFileChooserDialog -> (_GSList _gpath/free)))
(define-gtk gtk_file_chooser_set_current_name
  (_fun _GtkFileChooserDialog _path -> _void))
(define-gtk gtk_file_chooser_set_current_folder
  (_fun _GtkFileChooserDialog _path -> _void))
(define-gtk gtk_file_chooser_set_select_multiple
  (_fun _GtkFileChooserDialog _gboolean -> _void))

(define _GtkFileFilter (_cpointer 'GtkFileFilter))
(define-gtk gtk_file_filter_new (_fun -> _GtkFileFilter))
(define-gtk gtk_file_filter_set_name
  (_fun _GtkFileFilter _string -> _void))
(define-gtk gtk_file_filter_add_pattern
  (_fun _GtkFileFilter _string -> _void))

(define-gtk gtk_file_chooser_add_filter
  (_fun _GtkFileChooserDialog _GtkFileFilter -> _void))

(define (file-selector message directory filename 
                       extension ;; always ignored
                       filters style parent)
  (define type (car style)) ;; the rest of `style' is irrelevant on Gtk
  (define dlg (gtk_file_chooser_dialog_new 
               message (and parent (send parent get-gtk))
               (case type
                 [(dir) 'select-directory]
                 [(put) 'save]
                 [else 'open])
               "gtk-cancel" 'cancel
               ;; no stock names for "Select"
               (case type
                 [(dir) "Choose"]
                 [(put) "gtk-save"]
                 [(get) "gtk-open"]
                 [(multi) "Choose"])
               'accept
               #f))
  (when (eq? 'multi type)
    (gtk_file_chooser_set_select_multiple dlg #t))
  (when filename
    (gtk_file_chooser_set_current_name dlg filename))
  (when directory
    (gtk_file_chooser_set_current_folder dlg directory))
  (for ([f (in-list filters)])
    (match f
      [(list name glob)
       (let ([ff (gtk_file_filter_new)])
         (gtk_file_filter_set_name ff name)
         (gtk_file_filter_add_pattern ff glob)
         (gtk_file_chooser_add_filter dlg ff))]))
  (define ans (and (= -3 (show-dialog dlg))
                   (if (eq? type 'multi)           
                       (gtk_file_chooser_get_filenames dlg)
                       (gtk_file_chooser_get_filename dlg))))
  (gtk_widget_destroy dlg)
  ans)

(define response-sema (make-semaphore))
(define response-val #f)

(define-signal-handler connect-response "response"
  (_fun _GtkWidget _int -> _void)
  (lambda (gtk id)
    (set! response-val id)
    (semaphore-post response-sema)))

(define (show-dialog dlg-gtk)
  (connect-response dlg-gtk)
  (gtk_widget_show dlg-gtk)
  (yield response-sema)
  (gtk_widget_hide dlg-gtk)
  response-val)

(define (id-to-menu-item i) i)
(define-unimplemented get-the-x-selection)
(define-unimplemented get-the-clipboard)
(define-unimplemented show-print-setup)
(define (can-show-print-setup?) #f)
