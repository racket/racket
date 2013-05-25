#lang racket/base
(require ffi/unsafe
         racket/class
         racket/draw/private/local
          "../../syntax.rkt"
         "window.rkt"
         "utils.rkt"
         "types.rkt")

(provide
 (protect-out item%
              install-control-font))

(define _PangoFontDescription _pointer)
(define-gtk gtk_widget_modify_font (_fun _GtkWidget _PangoFontDescription -> _void))

(define (install-control-font gtk font)
  (when font
    (gtk_widget_modify_font gtk (send font get-pango))))

(defclass item% window%
  (inherit get-client-gtk)

  (init-field [callback void])
  (init [font #f])

  (super-new)

  (let ([client-gtk (get-client-gtk)])
    (connect-focus client-gtk)
    (connect-key-and-mouse client-gtk))
  (install-control-font (get-label-gtk) font)
  
  (define/public (get-label-gtk) (get-client-gtk))

  (def/public-unimplemented set-label)
  (def/public-unimplemented get-label)

  (define/public (command e)
    (callback this e)))





