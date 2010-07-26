#lang scheme/base
(require scheme/foreign
         scheme/class
          "../../syntax.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "window.rkt"
         "const.rkt"
         "../common/event.rkt")
(unsafe!)

(provide slider%)

;; ----------------------------------------

(define-gtk gtk_hscale_new (_fun _pointer -> _GtkWidget))
(define-gtk gtk_vscale_new (_fun _pointer -> _GtkWidget))
(define-gtk gtk_range_set_range (_fun _GtkWidget _double* _double* -> _void))
(define-gtk gtk_range_set_increments (_fun _GtkWidget _double* _double* -> _void))
(define-gtk gtk_range_set_value (_fun _GtkWidget _double* -> _void))
(define-gtk gtk_range_get_value (_fun _GtkWidget -> _double))

(define-signal-handler connect-changed "value-changed"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (send wx queue-changed))))

(defclass slider% item%
  (init parent cb
        label
        val lo hi
        x y w
        style
        font)
  (inherit get-gtk set-auto-size)

  (super-new [parent parent]
             [gtk (if (memq 'vertical style)
                      (gtk_vscale_new #f)
                      (gtk_hscale_new #f))]
             [callback cb]
             [no-show? (memq 'deleted style)])
  (define gtk (get-gtk))

  (gtk_range_set_range gtk lo hi)
  (gtk_range_set_increments gtk 1.0 1.0)
  (gtk_range_set_value gtk val)

  (set-auto-size)

  (connect-changed gtk)
  
  (define callback cb)
  (define/public (queue-changed)
    ;; Called in event-dispatch thread
    (gtk_range_set_value gtk (floor (gtk_range_get_value gtk)))
    (queue-window-event 
     this
     (lambda ()
       (callback this (new control-event%
                           [event-type 'slider]
                           [time-stamp (current-milliseconds)])))))

  (define/public (set-value v)
    (gtk_range_set_value gtk v))
  (define/public (get-value)
    (inexact->exact (floor (gtk_range_get_value gtk)))))
