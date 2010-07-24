#lang scheme/base
(require scheme/class
         scheme/foreign
         "../../syntax.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "pixbuf.rkt")
(unsafe!)

(provide message%

         gtk_label_new_with_mnemonic
         gtk_label_set_text_with_mnemonic
         mnemonic-string)

;; ----------------------------------------

(define-gtk gtk_label_new (_fun _string -> _GtkWidget))
(define-gtk gtk_label_set_text (_fun _GtkWidget _string -> _void))
(define-gtk gtk_label_set_text_with_mnemonic (_fun _GtkWidget _string -> _void))
(define-gtk gtk_image_new_from_stock (_fun _string _int -> _GtkWidget))

(define (mnemonic-string s)
  (if (regexp-match? #rx"&" s)
      (regexp-replace*
       #rx"_&"
       (regexp-replace*
        #rx"&(.)"
        (regexp-replace* #rx"_" s "__")
        "_\\1")
       "\\&")
      (regexp-replace* #rx"_" s "__")))

(define (gtk_label_new_with_mnemonic s)
  (let ([l (gtk_label_new s)])
    (when (regexp-match? #rx"&" s)
      (let ([s (mnemonic-string s)])
        (gtk_label_set_text_with_mnemonic l s)))
    l))

(define icon-size 6) ; = GTK_ICON_SIZE_DIALOG

(defclass message% item%
  (init parent label
        x y
        style font)
  (inherit set-auto-size get-gtk)

  (super-new [parent parent]
             [gtk (if (or (string? label)
                          (not label))
                      (gtk_label_new_with_mnemonic (or label ""))
                      (if (symbol? label)
                          (case label
                            [(caution) (gtk_image_new_from_stock "gtk-dialog-warning" icon-size)]
                            [(stop) (gtk_image_new_from_stock "gtk-dialog-error" icon-size)]
                            [else (gtk_image_new_from_stock "gtk-dialog-question" icon-size)])
                          (gtk_image_new_from_pixbuf 
                           (bitmap->pixbuf label))))]
             [no-show? (memq 'deleted style)])

  (set-auto-size)

  (define/override (set-label s)
    (gtk_label_set_text_with_mnemonic (get-gtk) (mnemonic-string s)))

  (def/public-unimplemented get-font))
