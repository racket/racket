#lang scheme/base
(require scheme/class
         scheme/foreign
         "../../syntax.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "pixbuf.rkt")
(unsafe!)

(provide message%)

;; ----------------------------------------

(define-gtk gtk_label_new (_fun _string -> _GtkWidget))
(define-gtk gtk_label_set_text (_fun _GtkWidget _string -> _void))

(defclass message% item%
  (init parent label
        x y
        style font)
  (inherit set-auto-size get-gtk)

  (super-new [parent parent]
             [gtk (if (or (string? label)
                          (not label))
                      (gtk_label_new (or label ""))
                      (if (symbol? label)
                          (gtk_label_new (format "<~a>" label))
                          (gtk_image_new_from_pixbuf 
                           (bitmap->pixbuf label))))]
             [no-show? (memq 'deleted style)])

  (set-auto-size)

  (define/override (set-label s)
    (gtk_label_set_text (get-gtk) s))

  (def/public-unimplemented get-font))
