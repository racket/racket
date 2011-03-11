#lang racket/base
(require ffi/unsafe
         racket/class
          "../../syntax.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "window.rkt"
         "const.rkt"
         "../common/event.rkt"
         "../../lock.rkt")

(provide 
 (protect-out slider%))

;; ----------------------------------------

(define-gtk gtk_hscale_new (_fun _pointer -> _GtkWidget))
(define-gtk gtk_vscale_new (_fun _pointer -> _GtkWidget))
(define-gtk gtk_range_set_range (_fun _GtkWidget _double* _double* -> _void))
(define-gtk gtk_range_set_increments (_fun _GtkWidget _double* _double* -> _void))
(define-gtk gtk_range_set_value (_fun _GtkWidget _double* -> _void))
(define-gtk gtk_range_get_value (_fun _GtkWidget -> _double))
(define-gtk gtk_scale_set_digits (_fun _GtkWidget _int -> _void))
(define-gtk gtk_scale_set_draw_value (_fun _GtkWidget _gboolean -> _void))

(define-signal-handler connect-changed "value-changed"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx queue-changed)))))

(defclass slider% item%
  (init parent cb
        label
        val lo hi
        x y w
        style
        font)
  (inherit get-gtk set-auto-size)

  (super-new [parent parent]
             [gtk (as-gtk-allocation
                   (if (memq 'vertical style)
                       (gtk_vscale_new #f)
                       (gtk_hscale_new #f)))]
             [callback cb]
             [no-show? (memq 'deleted style)])
  (define gtk (get-gtk))

  (gtk_scale_set_digits gtk 0)
  (gtk_range_set_range gtk lo hi)
  (gtk_range_set_increments gtk 1.0 1.0)
  (gtk_range_set_value gtk val)

  (when (memq 'plain style)
    (gtk_scale_set_draw_value gtk #f))

  (set-auto-size)

  (connect-changed gtk)
  
  (define callback cb)
  (define ignore-click? #f)
  (define/public (queue-changed)
    ;; Called in event-dispatch thread
    (gtk_range_set_value gtk (floor (gtk_range_get_value gtk)))
    (unless ignore-click?
      (queue-window-event 
       this
       (lambda ()
         (callback this (new control-event%
                             [event-type 'slider]
                             [time-stamp (current-milliseconds)]))))))

  (define/public (set-value v)
    (atomically
     (set! ignore-click? #t)
     (gtk_range_set_value gtk v)
     (set! ignore-click? #f)))
  (define/public (get-value)
    (inexact->exact (floor (gtk_range_get_value gtk)))))
