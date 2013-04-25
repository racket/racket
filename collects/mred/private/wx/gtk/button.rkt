#lang racket/base
(require ffi/unsafe
         racket/class
          "../../syntax.rkt"
          "../../lock.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "window.rkt"
         "const.rkt"
         "pixbuf.rkt"
         "message.rkt"
         "../common/event.rkt")

(provide 
 (protect-out button%
              button-core%))

;; ----------------------------------------

(define-gtk gtk_button_new_with_mnemonic (_fun _string -> _GtkWidget))
(define-gtk gtk_button_new (_fun -> _GtkWidget))
(define-gtk gtk_window_set_default (_fun _GtkWidget (_or-null _GtkWidget) -> _void))
(define-gtk gtk_button_set_label (_fun _GtkWidget _string -> _void))
(define-gtk gtk_button_set_image (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_button_set_image_position (_fun _GtkWidget _int -> _void))

(define GTK_POS_LEFT 0)
(define GTK_POS_RIGHT 1)
(define GTK_POS_TOP 2)
(define GTK_POS_BOTTOM 3)

(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_bin_get_child (_fun _GtkWidget -> _GtkWidget))

(define _GtkSettings (_cpointer 'GtkSettings))
(define-gtk gtk_settings_get_default (_fun -> _GtkSettings))
(define-gobj g_object_set/boolean 
  (_fun _GtkSettings _string _gboolean (_pointer = #f) -> _void)
  #:c-id g_object_set)
(define (force-button-images-on gtk)
  ;; Globally turning on button images isn't really the right thing.
  ;; Is there a way to enable image just for the widget `gtk'?
  (g_object_set/boolean (gtk_settings_get_default) "gtk-button-images" #t))

(define-signal-handler connect-clicked "clicked"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx queue-clicked)))))

(defclass button-core% item%
  (init parent cb label x y w h style font
        [gtk_new_with_mnemonic gtk_button_new_with_mnemonic]
        [gtk_new gtk_button_new])
  (init-field [event-type 'button])
  (inherit get-gtk get-client-gtk set-auto-size is-window-enabled?
           get-window-gtk)

  (super-new [parent parent]
             [gtk (cond
                   [(or (string? label) (not label))
                    (as-gtk-allocation
                     (gtk_new_with_mnemonic (or (mnemonic-string label) "")))]
                   [else
                    (let ([pixbuf (bitmap->pixbuf (if (pair? label) 
                                                      (car label)
                                                      label))])
                      (atomically
                       (let ([gtk (if (pair? label)
                                      (as-gtk-allocation (gtk_new_with_mnemonic (cadr label)))
                                      (as-gtk-allocation (gtk_new)))]
                             [image-gtk (gtk_image_new_from_pixbuf pixbuf)])
                         (release-pixbuf pixbuf)
                         (if (pair? label)
                             (begin
			       (force-button-images-on gtk)
			       (gtk_button_set_image gtk image-gtk)
                               (gtk_button_set_image_position 
                                gtk
                                (case (caddr label)
                                  [(left) GTK_POS_LEFT]
                                  [(right) GTK_POS_RIGHT]
                                  [(top) GTK_POS_TOP]
                                  [(bottom) GTK_POS_BOTTOM])))
                             (begin
                               (gtk_container_add gtk image-gtk)
                               (gtk_widget_show image-gtk)))
                         gtk)))])]
             [callback cb]
             [font font]
             [no-show? (memq 'deleted style)])
  (define gtk (get-gtk))

  (define both-labels? (pair? label))
  
  (when (eq? event-type 'button)
    (set-gtk-object-flags! gtk (bitwise-ior (get-gtk-object-flags gtk)
                                            GTK_CAN_DEFAULT)))

  (set-auto-size)
  
  (connect-clicked gtk)

  (when (memq 'border style) (set-border #t))

  (define callback cb)
  (define/public (clicked)
    (when (is-window-enabled?)
      (callback this (new control-event%
                          [event-type event-type]
                          [time-stamp (current-milliseconds)]))))
  (define/public (queue-clicked)
    ;; Called from event-handling thread
    (queue-window-event this (lambda () (clicked))))

  (define/override (get-label-gtk)
    (gtk_bin_get_child (get-client-gtk)))

  (define the-font font)
  (define/override (set-label s)
    (cond
     [(string? s)
      (gtk_button_set_label gtk (mnemonic-string s))
      (when the-font (install-control-font (get-label-gtk) the-font))]
     [else
      (let ([pixbuf (bitmap->pixbuf s)])
        (atomically
         (let ([image-gtk (gtk_image_new_from_pixbuf pixbuf)])
           (release-pixbuf pixbuf)
           (if both-labels?
               (gtk_button_set_image gtk image-gtk)
               (begin
                 (gtk_container_remove gtk (gtk_bin_get_child gtk))
                 (gtk_container_add gtk image-gtk)
                 (gtk_widget_show image-gtk))))))]))

  (define/public (set-border on?)
    (gtk_window_set_default (get-window-gtk) (if on? gtk #f))))

(defclass button% button-core%
  (super-new))

