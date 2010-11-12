#lang racket/base
(require ffi/unsafe
         racket/class
          "../../syntax.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "window.rkt"
         "const.rkt")

(provide 
 (protect-out gauge%))

;; ----------------------------------------

(define-gtk gtk_progress_bar_new (_fun _pointer -> _GtkWidget))
(define-gtk gtk_progress_bar_set_fraction (_fun _GtkWidget _double* -> _void))
(define-gtk gtk_progress_bar_set_orientation (_fun _GtkWidget _int -> _void))

(define GTK_PROGRESS_BOTTOM_TO_TOP 2)

(defclass gauge% item%
  (init parent
        label
        rng
        x y w h
        style
        font)
  (inherit get-gtk set-auto-size)

  (super-new [parent parent]
             [gtk (as-gtk-allocation (gtk_progress_bar_new #f))]
             [no-show? (memq 'deleted style)])
  (define gtk (get-gtk))

  (when (memq 'vertical style)
    (gtk_progress_bar_set_orientation gtk GTK_PROGRESS_BOTTOM_TO_TOP))

  (set-auto-size)

  (define range rng)
  (define value 0)

  (define/private (reset)
    (gtk_progress_bar_set_fraction gtk
                                   (if (zero? range)
                                       0.0
                                       (/ value range))))

  (define/public (get-range)
    range)
  (define/public (set-range r)
    (set! range r)
    (set! value (min value r))
    (reset))

  (define/public (set-value v)
    (set! value v)
    (reset))
  (define/public (get-value)
    value))
