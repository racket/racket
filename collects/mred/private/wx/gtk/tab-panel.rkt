#lang scheme/base
(require scheme/class
         scheme/foreign
          "../../syntax.rkt"
         "window.rkt"
         "client-window.rkt"
         "utils.rkt"
         "panel.rkt"
         "types.rkt"
         "widget.rkt")
(unsafe!)

(provide tab-panel%)

(define-gtk gtk_notebook_new (_fun -> _GtkWidget))
(define-gtk gtk_fixed_new (_fun -> _GtkWidget))
(define-gtk gtk_hbox_new (_fun _gboolean _int -> _GtkWidget))
(define-gtk gtk_label_new (_fun _string -> _GtkWidget))
(define-gtk gtk_label_set_text (_fun _GtkWidget _string -> _void))

(define-gtk gtk_notebook_append_page (_fun _GtkWidget _GtkWidget (_or-null _GtkWidget) -> _void))
(define-gtk gtk_notebook_set_current_page (_fun _GtkWidget _int -> _void))

(define-gtk gtk_fixed_move (_fun _GtkWidget _GtkWidget _int _int -> _void))

(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))

(define-gtk gtk_widget_ref (_fun _GtkWidget -> _void))
(define-gtk gtk_widget_unref (_fun _GtkWidget -> _void))

(define-struct page (bin-gtk label-gtk))

(define-signal-handler connect-changed "switch-page"
  (_fun _GtkWidget _pointer _int -> _void)
  (lambda (gtk ignored i)
    (let ([wx (gtk->wx gtk)])
      (send wx page-changed i))))

(define tab-panel%
  (class (client-size-mixin (panel-mixin window%))
    (init parent
          x y w h
          style
          labels)
    
    (inherit set-size set-auto-size get-gtk)

    (define gtk (gtk_notebook_new))
    ;; Reparented so that it's always in the current page's bin:
    (define client-gtk (gtk_fixed_new))

    (define empty-bin-gtk (gtk_hbox_new #f 0))
    (define current-bin-gtk #f)

    (define (select-bin bin-gtk)
      (set! current-bin-gtk bin-gtk)
      (gtk_box_pack_start bin-gtk client-gtk #t #t 0))

    (define pages
      (for/list ([lbl labels])
        (let ([bin-gtk (gtk_hbox_new #f 0)]
              [label-gtk (gtk_label_new lbl)])
          (gtk_notebook_append_page gtk bin-gtk label-gtk)
          (gtk_widget_show bin-gtk)
          (make-page bin-gtk label-gtk))))

    (if (null? pages)
        (begin
          (select-bin empty-bin-gtk)
          (gtk_notebook_append_page gtk empty-bin-gtk #f)
          (gtk_widget_show empty-bin-gtk))
        (begin
          (select-bin (page-bin-gtk (car pages)))))
    (gtk_widget_show client-gtk)
    
    (super-new [parent parent]
               [gtk gtk]
               [client-gtk client-gtk]
               [extra-gtks (list client-gtk)]
               [no-show? (memq 'deleted style)])

    (set-auto-size)

    (define/public (page-changed i)
      (let ([bin-gtk (page-bin-gtk (list-ref pages i))])
        (gtk_widget_ref client-gtk)
        (gtk_container_remove current-bin-gtk client-gtk)
        (select-bin bin-gtk)
        (gtk_widget_unref client-gtk)))
    (connect-changed gtk)
    
    (define/override (get-client-gtk) client-gtk)

    (define/public (set-label i str)
      (gtk_label_set_text (page-label-gtk (list-ref pages i)) str))

    (define/public (set-selection i)
      (gtk_notebook_set_current_page gtk i))

    (define/override (set-child-size child-gtk x y w h)
      (gtk_fixed_move client-gtk child-gtk x y)
      (super set-child-size child-gtk x y w h))))
