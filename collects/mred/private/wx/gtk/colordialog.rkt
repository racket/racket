#lang racket/base
(require ffi/unsafe
         racket/class
         racket/draw/private/color
         "types.rkt"
         "utils.rkt"
         "stddialog.rkt")

(provide 
 (protect-out get-color-from-user
	      color-dialog-works?))

(define-gtk gtk_color_selection_dialog_new (_fun _string -> _GtkWidget))

(define-gtk gtk_color_selection_dialog_get_color_selection (_fun _GtkWidget -> _GtkWidget)
  #:fail (lambda () #f))
(define-gtk gtk_color_selection_get_current_color (_fun _GtkWidget (c : (_ptr o _GdkColor)) -> _void -> c))
(define-gtk gtk_color_selection_set_current_color (_fun _GtkWidget _GdkColor-pointer -> _void))

(define (color-dialog-works?)
  (and gtk_color_selection_dialog_get_color_selection #t))

(define (get-color-from-user message parent color)
  (let ([d (as-gtk-window-allocation
            (gtk_color_selection_dialog_new (or message "Choose Color")))]
        [to-gdk (lambda (c) (arithmetic-shift c 8))])
    (when color
      (gtk_color_selection_set_current_color
       (gtk_color_selection_dialog_get_color_selection d)
       (make-GdkColor
        0
        (to-gdk (color-red color))
        (to-gdk (color-green color))
        (to-gdk (color-blue color)))))
    (and (eq? (show-dialog d) 'ok)
         (let ([c (gtk_color_selection_get_current_color
                   (gtk_color_selection_dialog_get_color_selection d))])
           (make-object color% 
                        (arithmetic-shift (GdkColor-red c) -8)
                        (arithmetic-shift (GdkColor-green c) -8)
                        (arithmetic-shift (GdkColor-blue c) -8))))))
