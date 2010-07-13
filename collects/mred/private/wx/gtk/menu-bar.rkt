#lang scheme/base
(require scheme/class
         scheme/foreign
          "../../syntax.rkt"
          "../common/freeze.rkt"
          "widget.rkt"
          "utils.rkt"
          "types.rkt")
(unsafe!)

(provide menu-bar%
         gtk_menu_item_new_with_mnemonic
         gtk_menu_shell_append
         fixup-mneumonic)

(define-gtk gtk_menu_bar_new (_fun -> _GtkWidget))
(define-gtk gtk_menu_shell_append (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_menu_item_new_with_mnemonic (_fun _string -> _GtkWidget))
(define-gtk gtk_menu_item_set_submenu (_fun _GtkWidget (_or-null _GtkWidget) -> _void))
(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))

(define (fixup-mneumonic title)
  (regexp-replace*
   "&&"
   (regexp-replace*
    #rx"&([^&])"
    (regexp-replace*
     #rx"_" 
     (regexp-replace #rx"\t.*$" title "")
     "__")
    "_\\1")
   "&"))

(define-signal-handler connect-button-press "button-press-event"
  (_fun _GtkWidget _GdkEventButton-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)])
      (let ([frame (send wx get-top-window)])
        (constrained-reply (send wx get-eventspace)
                           (lambda () (send frame on-menu-click) #f)
                           #t)))))

(defclass menu-bar% widget%
  (define menus null)

  (define gtk (gtk_menu_bar_new))
  (super-new [gtk gtk])

  (connect-button-press gtk)

  (define/public (get-gtk) gtk)

  (define top-wx #f)
  (define/public (set-top-window top)
    (set! top-wx top))
  (define/public (get-top-window)
    top-wx)

  (def/public-unimplemented set-label-top)
  (def/public-unimplemented number)
  (def/public-unimplemented enable-top)

  (define/public (delete which pos)
    (set! menus (let loop ([menus menus]
                           [pos pos])
                  (cond
                   [(null? menus) menus]
                   [(zero? pos) 
                    (gtk_container_remove gtk (caar menus))
                    (gtk_menu_item_set_submenu (caar menus) #f)
                    (cdr menus)]
                   [else (cons (car menus)
                               (loop (cdr menus)
                                     pos))]))))

  (public [append-menu append])
  (define (append-menu menu title)
    (send menu set-parent this)
    (let ([item (gtk_menu_item_new_with_mnemonic (fixup-mneumonic title))])
      (set! menus (append menus (list (list item menu title))))
      (let ([gtk (send menu get-gtk)])
        (g_object_ref gtk)
        (gtk_menu_item_set_submenu item gtk))
      (gtk_menu_shell_append gtk item)
      (gtk_widget_show item))))
