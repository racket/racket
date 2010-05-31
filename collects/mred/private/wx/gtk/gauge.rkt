#lang scheme/base
(require scheme/foreign
         scheme/class
          "../../syntax.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "window.rkt"
         "const.rkt")
(unsafe!)

(provide gauge%)

;; ----------------------------------------

(define-gtk gtk_progress_bar_new (_fun _pointer -> _GtkWidget))
(define-gtk gtk_progress_bar_set_fraction (_fun _GtkWidget _double* -> _void))

(defclass gauge% item%
  (init parent
        label
        rng
        x y w h
        style
        font)
  (inherit get-gtk set-auto-size)

  (super-new [parent parent]
             [gtk (gtk_progress_bar_new #f)]
             [no-show? (memq 'deleted style)])
  (define gtk (get-gtk))

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
