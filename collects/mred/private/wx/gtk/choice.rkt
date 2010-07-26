#lang scheme/base
(require scheme/foreign
         scheme/class
          "../../syntax.rkt"
          "../../lock.rkt"
         "item.rkt"
         "types.rkt"
         "utils.rkt"
         "window.rkt"
         "../common/event.rkt")
(unsafe!)

(provide choice%)

;; ----------------------------------------

(define-gtk gtk_combo_box_new_text (_fun -> _GtkWidget))
(define-gtk gtk_combo_box_append_text (_fun _GtkWidget _string -> _void))
(define-gtk gtk_combo_box_remove_text (_fun _GtkWidget _int -> _void))
(define-gtk gtk_combo_box_set_active (_fun _GtkWidget _int -> _void))
(define-gtk gtk_combo_box_get_active (_fun _GtkWidget -> _int))

(define-gtk gtk_container_foreach (_fun _GtkWidget (_fun _GtkWidget -> _void) _pointer -> _void))
(define-gtk gtk_container_forall (_fun _GtkWidget (_fun _GtkWidget -> _void) _pointer -> _void))

(define-signal-handler connect-changed "changed"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (send wx queue-clicked))))

(defclass choice% item%
  (init parent cb label
        x y w h
        choices style font)
  (inherit get-gtk set-auto-size)

  (define gtk (gtk_combo_box_new_text))
  (define count (length choices))

  (for ([l (in-list choices)])
    (gtk_combo_box_append_text gtk l))

  ;; Hack to access the combobox's private child, where is
  ;;  where the keyboard focus goes.
  (define button-gtk
    (let ([all null]
          [ext null])
      (gtk_container_forall gtk (lambda (c) (set! all (cons c all))) #f)
      (gtk_container_foreach gtk (lambda (c) (set! ext (cons c ext))) #f)
      (for-each (lambda (e)
                  (set! all (filter (lambda (a) (not (ptr-equal? a e)))
                                    all)))
                ext)
      (unless (= 1 (length all))
        (error "expected Gtk combobox to have one private child"))
      (car all)))

  (super-new [parent parent]
             [gtk gtk]
             [extra-gtks (list button-gtk)]
             [callback cb]
             [no-show? (memq 'deleted style)])

  (gtk_combo_box_set_active gtk 0)

  (set-auto-size)

  (connect-changed gtk)
  (connect-focus button-gtk)

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
    (as-entry
     (lambda ()
       (set! ignore-clicked? #t)
       (gtk_combo_box_set_active gtk i)
       (set! ignore-clicked? #f))))
  (define/public (get-selection)
    (gtk_combo_box_get_active gtk))
  (define/public (number) count)
  (define/public (clear)
    (as-entry
     (lambda ()
       (for ([i (in-range count)])
         (gtk_combo_box_remove_text gtk 0))
       (set! count 0))))
  (define/public (append l)
    (as-entry
     (lambda ()
       (set! count (add1 count))
       (gtk_combo_box_append_text gtk l)
       (when (= count 1)
         (set-selection 0))))))
