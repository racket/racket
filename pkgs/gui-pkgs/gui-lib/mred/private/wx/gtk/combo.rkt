#lang racket/base
(require ffi/unsafe
         racket/class
          "../../syntax.rkt"
         "types.rkt"
         "utils.rkt"
         "window.rkt")

;; Hacks for working with GtkComboBox[Entry]

(provide 
 (protect-out extract-combo-button
	      re-extract-combo-button
              connect-combo-key-and-mouse))

;; ----------------------------------------

(define-gtk gtk_container_foreach (_fun _GtkWidget (_fun _GtkWidget -> _void) _pointer -> _void))
(define-gtk gtk_container_forall (_fun _GtkWidget (_fun _GtkWidget -> _void) _pointer -> _void))

(define-gtk gtk_widget_get_name (_fun _GtkWidget -> _string))

(define-gobj g_signal_parse_name (_fun _string
                                       _GType
                                       (id : (_ptr o _uint))
                                       (_ptr o _GQuark)
                                       _gboolean
                                       -> (r : _gboolean)
                                       -> (and r id)))

(define-gobj g_type_from_name (_fun _string -> _GType))

(define _GSignalMatchType _int)
(define _GQuark _uint32)
(define _GClosure _int)
(define-gobj g_signal_handler_find (_fun _GtkWidget 
                                         _GSignalMatchType 
                                         _uint ; signal_id
                                         _GQuark ; detail
                                         _GClosure ; closure
                                         _pointer ; func
                                         _pointer ; data
                                         -> _ulong))
(define-gobj g_signal_handler_disconnect (_fun _GtkWidget _uint -> _void))
(define-gobj g_signal_handler_block (_fun _GtkWidget _uint -> _void))
(define-gobj g_signal_handler_unblock (_fun _GtkWidget _uint -> _void))

(define-gobj g_signal_emit (_fun _GtkWidget
                                 _uint
                                 _GQuark
                                 _pointer
                                 (r : (_ptr o _gboolean))
                                 -> _void
                                 -> r))

(define G_SIGNAL_MATCH_ID 1)

(define button-press-id #f)

(define unblocked? #f)
(define-signal-handler connect-reorder-button-press "button-press-event"
  (_fun _GtkWidget _GdkEventButton-pointer _long -> _gboolean)
  (lambda (gtk event other-id)
    (if unblocked?
        #f
        (let ([v (do-button-event gtk event #f #f)])
          (or v
              (begin
                (g_signal_handler_unblock gtk other-id)
		(set! unblocked? #t)
                (let ([r (g_signal_emit gtk
                                        button-press-id
                                        0
                                        event)])
                  (g_signal_handler_block gtk other-id)
		  (set! unblocked? #f)
                  r)))))))

;; Dependence on the implemenation of GtkComboBox:
;; Keyboard focus and other actions are based on a private button widget
;; inside a GtkComboBox, so we extract it.
(define (extract-combo-button gtk)
  (let ([all null]
        [ext null])
    (gtk_container_forall gtk (lambda (c) (set! all (cons c all))) #f)
    (gtk_container_foreach gtk (lambda (c) (set! ext (cons c ext))) #f)
    (for-each (lambda (e)
                (set! all (filter (lambda (a)
				    (not (ptr-equal? a e)))
                                  all)))
              ext)
    (define combo-gtk
      (cond
       [(= 1 (length all))
	;; most common case:
	(car all)]
       [(and (= 2 (length all))
	     (equal? '("GtkFrame" "GtkToggleButton")
		     (map gtk_widget_get_name all)))
	(define inner null)
	(gtk_container_forall (car all) (lambda (c) (set! inner (cons c inner))) #f)
	(and (= 1 (length inner))
	     (car inner))]
       [else #f]))
    (unless combo-gtk
      (error "unrecognized Gtk combobox implementation"))
    combo-gtk))

(define (re-extract-combo-button gtk combo-button-gtk win)
  (define c-gtk (extract-combo-button gtk))
  (cond
   [(ptr-equal? c-gtk combo-button-gtk)
    ;; combo button hasn't changed:
    combo-button-gtk]
   [else
    (send win register-extra-gtk gtk c-gtk)
    c-gtk]))

;; More dependence on the implemenation of GtkComboBox:
;; The menu-popup action is implemented by seeting a button-press-event
;; signal handler on `button-gtk'. Since Gtk calls signal handlers in the
;; order that they're registered, our button-press-event handler doesn't
;; get called first, so it can't cancel the button press due to modality
;; or an `on-subwindow-event' result. We effectively reorder the callbacks
;; by finding the old one, blocking it, and then unblocking during a
;; redispatch.  
(define (connect-combo-key-and-mouse button-gtk)
  (unless button-press-id
    (set! button-press-id
          (g_signal_parse_name "button-press-event" (g_type_from_name "GtkWidget") #f)))
  (let ([hand-id
         (and button-press-id
              (let ([hand-id (g_signal_handler_find button-gtk
                                                    G_SIGNAL_MATCH_ID
                                                    button-press-id
                                                    0
                                                    0
                                                    #f
                                                    #f)])
                (if (zero? hand-id)
                    #f
                    (begin
                      (g_signal_handler_block button-gtk hand-id)
                      hand-id))))])
    (connect-key-and-mouse button-gtk (and hand-id #t))
    (when hand-id
      (connect-reorder-button-press button-gtk (cast hand-id _long _pointer)))))
