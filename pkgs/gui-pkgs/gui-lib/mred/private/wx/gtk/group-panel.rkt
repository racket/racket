#lang racket/base
(require racket/class
         ffi/unsafe
          "../../syntax.rkt"
          "../../lock.rkt"
         "window.rkt"
         "client-window.rkt"
         "panel.rkt"
         "utils.rkt"
         "types.rkt")

(provide
 (protect-out group-panel%))

(define-gtk gtk_frame_new (_fun _string -> _GtkWidget))

(define-gtk gtk_frame_set_label (_fun _GtkWidget _string -> _void))
(define-gtk gtk_frame_get_label_widget (_fun _GtkWidget ->  _GtkWidget))

(define group-panel%
  (class (client-size-mixin (panel-container-mixin (panel-mixin window%)))
    (init parent
          x y w h
          style
          label)
    
    (inherit set-size set-auto-size infer-client-delta
             get-gtk get-height)

    (define gtk (as-gtk-allocation (gtk_frame_new label)))
    (define client-gtk 
      (atomically (let ([client-gtk (gtk_fixed_new)])
                    (gtk_container_add gtk client-gtk)
                    client-gtk)))
    (gtk_widget_show client-gtk)
    
    (super-new [parent parent]
               [gtk gtk]
               [client-gtk client-gtk]
               [extra-gtks (list client-gtk)]
               [no-show? (memq 'deleted style)])

    (infer-client-delta #t #t (gtk_frame_get_label_widget gtk))
    (set-auto-size)

    (define/public (set-label s)
      (gtk_frame_set_label gtk s))

    (define/override (gets-focus?) #f)
    
    (define/override (get-client-gtk) client-gtk)))
