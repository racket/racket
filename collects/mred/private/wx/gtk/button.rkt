#lang scheme/base
(require scheme/foreign
         scheme/class
          "../../syntax.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "window.rkt"
         "const.rkt"
         "pixbuf.rkt"
         "message.rkt"
         "../common/event.rkt")
(unsafe!)

(provide button%
         button-core%)

;; ----------------------------------------

(define-gtk gtk_button_new_with_mnemonic (_fun _string -> _GtkWidget))
(define-gtk gtk_button_new (_fun -> _GtkWidget))
(define-gtk gtk_window_set_default (_fun _GtkWidget (_or-null _GtkWidget) -> _void))
(define-gtk gtk_button_set_label (_fun _GtkWidget _string -> _void))

(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_bin_get_child (_fun _GtkWidget -> _GtkWidget))

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
  (inherit get-gtk set-auto-size is-window-enabled?
           get-window-gtk)

  (super-new [parent parent]
             [gtk (cond
                   [(or (string? label) (not label))
                    (gtk_new_with_mnemonic (or (mnemonic-string label) ""))]
                   [(send label ok?)
                    (let ([gtk (gtk_new)]
                          [image-gtk (gtk_image_new_from_pixbuf 
                                      (bitmap->pixbuf label))])
                      (gtk_container_add gtk image-gtk)
                      (gtk_widget_show image-gtk)
                      gtk)]
                   [else
                    (gtk_new_with_mnemonic "<bad>")])]
             [callback cb]
             [no-show? (memq 'deleted style)])
  (define gtk (get-gtk))
  
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

  (define/override (set-label s)
    (cond
     [(string? s)
      (gtk_button_set_label gtk (mnemonic-string s))]
     [else
      (let ([image-gtk (gtk_image_new_from_pixbuf 
                        (bitmap->pixbuf s))])
        (gtk_container_remove gtk (gtk_bin_get_child gtk))
        (gtk_container_add gtk image-gtk)
        (gtk_widget_show image-gtk))]))

  (define/public (set-border on?)
    (gtk_window_set_default (get-window-gtk) (if on? gtk #f))))

(defclass button% button-core%
  (super-new))

