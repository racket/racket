#lang racket/base
(require ffi/unsafe
         racket/class
          "../../syntax.rkt"
          "../../lock.rkt"
         "item.rkt"
         "types.rkt"
         "utils.rkt"
         "window.rkt"
         "combo.rkt"
         "../common/event.rkt"
         "../common/queue.rkt")

(provide 
 (protect-out choice%))

;; ----------------------------------------

(define-gtk gtk_combo_box_new_text (_fun -> _GtkWidget))
(define-gtk gtk_combo_box_append_text (_fun _GtkWidget _string -> _void))
(define-gtk gtk_combo_box_remove_text (_fun _GtkWidget _int -> _void))
(define-gtk gtk_combo_box_set_active (_fun _GtkWidget _int -> _void))
(define-gtk gtk_combo_box_get_active (_fun _GtkWidget -> _int))
(define-gtk gtk_bin_get_child (_fun _GtkWidget -> _GtkWidget))

(define-signal-handler connect-changed "changed"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx queue-clicked)))))

(defclass choice% item%
  (init parent cb label
        x y w h
        choices style font)
  (inherit get-gtk set-auto-size)

  (define gtk (as-gtk-allocation (gtk_combo_box_new_text)))
  (define count (length choices))

  (for ([l (in-list choices)])
    (gtk_combo_box_append_text gtk l))

  ;; Hack to access the combobox's private child, where is
  ;;  where the keyboard focus goes.
  (define button-gtk (extract-combo-button gtk))

  (super-new [parent parent]
             [gtk gtk]
             [extra-gtks (list button-gtk)]
             [callback cb]
             [no-show? (memq 'deleted style)])

  (gtk_combo_box_set_active gtk 0)

  (install-control-font (gtk_bin_get_child gtk) font)

  (set-auto-size)

  (connect-changed gtk)
  (connect-focus button-gtk)
  (connect-combo-key-and-mouse button-gtk)

  (define callback cb)
  (define/public (clicked)
    (callback this (new control-event%
                        [event-type 'choice]
                        [time-stamp (current-milliseconds)])))
  (define ignore-clicked? #f)
  (define/public (queue-clicked)
    ;; called in event-handling thread
    (unless ignore-clicked?
      (queue-window-event this (lambda () (clicked)))))

  (define/public (set-selection i)
    (atomically
     (set! ignore-clicked? #t)
     (gtk_combo_box_set_active gtk i)
     (set! ignore-clicked? #f)))

  (define/public (get-selection)
    (gtk_combo_box_get_active gtk))

  (define/public (number) count)

  (define/public (clear)
    (atomically
     (set! ignore-clicked? #t)
     (for ([i (in-range count)])
       (gtk_combo_box_remove_text gtk 0))
     (set! count 0)
     (set! ignore-clicked? #f)))

  (public [-append append])
  (define (-append l)
    (atomically
     (set! ignore-clicked? #t)
     (set! count (add1 count))
     (gtk_combo_box_append_text gtk l)
     (when (= count 1)
       (set-selection 0))
     (set! ignore-clicked? #f)))

  (define/public (delete i)
    (gtk_combo_box_remove_text gtk i)))
