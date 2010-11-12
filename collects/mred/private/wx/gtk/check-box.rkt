#lang racket/base
(require ffi/unsafe
         racket/class
          "../../syntax.rkt"
         "button.rkt"
         "utils.rkt"
         "types.rkt"
         "../../lock.rkt")

(provide 
 (protect-out check-box%))

;; ----------------------------------------

(define-gtk gtk_check_button_new_with_mnemonic (_fun _string -> _GtkWidget))
(define-gtk gtk_check_button_new (_fun -> _GtkWidget))
(define-gtk gtk_toggle_button_get_active (_fun _GtkWidget -> _gboolean))
(define-gtk gtk_toggle_button_set_active (_fun _GtkWidget _gboolean -> _void))

(defclass check-box% button-core%
  (super-new [gtk_new_with_mnemonic gtk_check_button_new_with_mnemonic]
             [gtk_new gtk_check_button_new]
             [event-type 'check-box])
  (inherit get-gtk)

  (define/public (set-value v)
    (atomically
     (set! no-clicked? #t)
     (gtk_toggle_button_set_active (get-gtk) v)
     (set! no-clicked? #f)))

  (define no-clicked? #f)
  (define/override (queue-clicked)
    (unless no-clicked? (super queue-clicked)))
       
  (define/public (get-value)
    (gtk_toggle_button_get_active (get-gtk))))
