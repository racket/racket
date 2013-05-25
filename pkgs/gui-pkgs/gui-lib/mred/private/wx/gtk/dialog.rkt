#lang racket/base
(require racket/class
         ffi/unsafe
          "../../syntax.rkt"
          "../common/queue.rkt"
          "../common/dialog.rkt"
          "../../lock.rkt"
          "types.rkt"
          "utils.rkt"
          "frame.rkt")

(provide 
 (protect-out dialog%))

(define GTK_WIN_POS_CENTER 1)
(define GTK_WIN_POS_CENTER_ON_PARENT 4)

(define GDK_WINDOW_TYPE_HINT_DIALOG 1)

(define-gtk gtk_window_set_position (_fun _GtkWidget _int -> _void))
(define-gtk gtk_window_set_transient_for (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_window_set_type_hint (_fun _GtkWidget _int -> _void))

(define dialog% 
  (class (dialog-mixin frame%)
    (inherit get-gtk
             get-parent)

    (super-new [is-dialog? #t])

    (gtk_window_set_type_hint (get-gtk) GDK_WINDOW_TYPE_HINT_DIALOG)

    (let ([p (get-parent)])
      (when p
        (gtk_window_set_transient_for (get-gtk) (send p get-gtk))))
    
    (define/override (center dir wrt)
      (if (eq? dir 'both)
          (gtk_window_set_position (get-gtk) 
                                   (if (get-parent)
                                       GTK_WIN_POS_CENTER_ON_PARENT
                                       GTK_WIN_POS_CENTER))
          (super center dir wrt)))))
