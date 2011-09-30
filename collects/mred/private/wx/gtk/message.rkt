#lang racket/base
(require racket/class
         ffi/unsafe
         "../../syntax.rkt"
         "../../lock.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "pixbuf.rkt"
         "window.rkt")

(provide 
 (protect-out message%
              
              gtk_label_new_with_mnemonic
              gtk_label_set_text_with_mnemonic))

;; ----------------------------------------

(define-gtk gtk_label_new (_fun _string -> _GtkWidget))
(define-gtk gtk_label_set_text (_fun _GtkWidget _string -> _void))
(define-gtk gtk_label_set_text_with_mnemonic (_fun _GtkWidget _string -> _void))
(define-gtk gtk_image_new_from_stock (_fun _string _int -> _GtkWidget))
(define-gtk gtk_misc_set_alignment (_fun _GtkWidget _float _float -> _void))
(define-gtk gtk_image_set_from_pixbuf (_fun _GtkWidget _GdkPixbuf -> _void))

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
                      (as-gtk-allocation (gtk_label_new_with_mnemonic (or label "")))
                      (if (symbol? label)
                          (as-gtk-allocation
                           (case label
                             [(caution) (gtk_image_new_from_stock "gtk-dialog-warning" icon-size)]
                             [(stop) (gtk_image_new_from_stock "gtk-dialog-error" icon-size)]
                             [else (gtk_image_new_from_stock "gtk-dialog-question" icon-size)]))
                          (let ([pixbuf (bitmap->pixbuf label)])
                            (begin0
                             (as-gtk-allocation
                              (gtk_image_new_from_pixbuf pixbuf))
                             (release-pixbuf pixbuf)))))]
             [font font]
             [no-show? (memq 'deleted style)])

  (when (string? label)
    (gtk_misc_set_alignment (get-gtk) 0.0 0.0))

  (set-auto-size)

  (define/override (set-label s)
    (cond
     [(string? s)
      (gtk_label_set_text_with_mnemonic (get-gtk) (mnemonic-string s))]
     [else
      (let ([pixbuf (bitmap->pixbuf s)])
        (atomically
         (gtk_image_set_from_pixbuf (get-gtk) pixbuf)
         (release-pixbuf pixbuf)))]))

  (define/public (set-preferred-size)
    (gtk_widget_set_size_request (get-gtk) -1 -1)
    (set-auto-size)
    #t)

  (define/override (gets-focus?) #f)

  (def/public-unimplemented get-font))
