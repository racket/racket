#lang scheme/base
(require scheme/class
         scheme/foreign
          "../../syntax.rkt"
         "window.rkt"
         "client-window.rkt"
         "panel.rkt"
         "utils.rkt"
         "types.rkt")
(unsafe!)

(provide group-panel%)

(define-gtk gtk_frame_new (_fun _string -> _GtkWidget))
(define-gtk gtk_fixed_new (_fun -> _GtkWidget))

(define-gtk gtk_fixed_move (_fun _GtkWidget _GtkWidget _int _int -> _void))

(define-gtk gtk_frame_set_label (_fun _GtkWidget _string -> _void))

(define group-panel%
  (class (client-size-mixin (panel-mixin window%))
    (init parent
          x y w h
          style
          label)
    
    (inherit set-size set-auto-size get-gtk get-height)

    (define gtk (gtk_frame_new label))
    (define client-gtk (gtk_fixed_new))
    (gtk_container_add gtk client-gtk)
    (gtk_widget_show client-gtk)
    
    (super-new [parent parent]
               [gtk gtk]
               [client-gtk client-gtk]
               [extra-gtks (list client-gtk)]
               [no-show? (memq 'deleted style)])

    (set-auto-size)

    ;; The delta between the group box height and its
    ;; client height can go bad if the label is set.
    ;; Avoid the problem by effectively using the
    ;; original delta.
    (define orig-h (get-height))
    (define/override (get-client-size xb yb)
      (super get-client-size xb yb)
      (set-box! yb (- (get-height) orig-h)))

    (define/public (set-label s)
      (gtk_frame_set_label gtk s))
    
    (define/override (get-client-gtk) client-gtk)

    (define/override (set-child-size child-gtk x y w h)
      (gtk_fixed_move client-gtk child-gtk x y)
      (super set-child-size child-gtk x y w h))))
