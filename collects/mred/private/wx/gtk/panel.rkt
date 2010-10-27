#lang racket/base
(require racket/class
         ffi/unsafe
          "../../syntax.rkt"
          "../../lock.rkt"
         "window.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt")

(provide 
 (protect-out panel%
              panel-mixin))

(define-gtk gtk_fixed_new (_fun -> _GtkWidget))
(define-gtk gtk_event_box_new (_fun -> _GtkWidget))

(define-gtk gtk_fixed_move (_fun _GtkWidget _GtkWidget _int _int -> _void))

(define (panel-mixin %)
  (class %

    (define lbl-pos 'horizontal)
    (define children null)

    (super-new)

    (define/public (get-label-position) lbl-pos)
    (define/public (set-label-position pos) (set! lbl-pos pos))

    (define/override (reset-child-dcs)
      (when (pair? children)
        (for ([child (in-list children)])
          (send child reset-child-dcs))))
    
    (define/override (paint-children)
      (when (pair? children)
        (for ([child (in-list children)])
          (send child paint-children))))
    
    (define/override (set-size x y w h)
      (super set-size x y w h)
      (reset-child-dcs))

    (define/override (register-child child on?)
      (let ([now-on? (and (memq child children) #t)])
        (unless (eq? on? now-on?)
          (set! children 
                (if on?
                    (cons child children)
                    (remq child children))))))

    (define/public (set-item-cursor x y) (void))))

(define panel%
  (class (panel-mixin window%)
    (init parent
          x y w h
          style
          label)
    
    (inherit set-size get-gtk)
    
    (define gtk (as-gtk-allocation (gtk_event_box_new)))
    (define client-gtk (atomically
                        (let ([client (gtk_fixed_new)])
                          (gtk_container_add gtk client)
                          (gtk_widget_show client)
                          client)))

    (define/override (get-client-gtk) client-gtk)

    (super-new [parent parent]
               [gtk gtk]
               [extra-gtks (list client-gtk)]
               [no-show? (memq 'deleted style)])
    
    (connect-key-and-mouse gtk)
    (gtk_widget_add_events gtk (bitwise-ior GDK_BUTTON_PRESS_MASK
                                            GDK_BUTTON_RELEASE_MASK
                                            GDK_POINTER_MOTION_MASK
                                            GDK_FOCUS_CHANGE_MASK
                                            GDK_ENTER_NOTIFY_MASK
                                            GDK_LEAVE_NOTIFY_MASK))

    (define/override (set-child-size child-gtk x y w h)
      (gtk_fixed_move client-gtk child-gtk x y)
      (gtk_widget_set_size_request child-gtk w h))))
