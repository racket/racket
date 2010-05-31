#lang scheme/base
(require scheme/class
         scheme/foreign
          "../../syntax.rkt"
         "window.rkt"
         "utils.rkt"
         "types.rkt")
(unsafe!)

(provide panel%
         panel-mixin)

; (define-gtk gtk_alignment_new (_fun _gfloat _gfloat _gfloat _gfloat -> _GtkWidget))
(define-gtk gtk_fixed_new (_fun -> _GtkWidget))

(define-gtk gtk_fixed_move (_fun _GtkWidget _GtkWidget _int _int -> _void))

(define (panel-mixin %)
  (class %
    (define lbl-pos 'vertical)
    (super-new)

    (define/public (get-label-position) lbl-pos)
    (define/public (set-label-position pos) (set! lbl-pos pos))

    (def/public-unimplemented on-paint)
    (define/public (set-item-cursor x y) (void))
    (def/public-unimplemented get-item-cursor)))

(define panel%
  (class (panel-mixin window%)
    (init parent
          x y w h
          style
          label)
    
    (inherit set-size get-gtk)
    
    (super-new [parent parent]
               [gtk (gtk_fixed_new)] ; (gtk_alignment_new 0.0 0.0 1.0 1.0)]
               [no-show? (memq 'deleted style)])
    
    (define gtk (get-gtk))

    (define/override (set-child-size child-gtk x y w h)
      (gtk_fixed_move gtk child-gtk x y)
      (super set-child-size child-gtk x y w h))))
