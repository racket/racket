#lang scheme/base
(require scheme/class
         scheme/foreign
          "../../syntax.rkt"
          "../../lock.rkt"
          "../common/freeze.rkt"
          "../common/queue.rkt"
          "widget.rkt"
          "window.rkt"
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
(define-gtk gtk_label_set_text_with_mnemonic (_fun _GtkWidget _string -> _void))
(define-gtk gtk_bin_get_child (_fun _GtkWidget -> _GtkWidget))

(define-gtk gtk_widget_set_usize (_fun _GtkWidget _int _int -> _void))

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

(define-signal-handler connect-select "select"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (let ([frame (send wx get-top-window)])
          (when frame
            (constrained-reply (send frame get-eventspace)
                               (lambda () (send frame on-menu-click))
                               (void))))))))

(define top-menu%
  (class widget%
    (init-field parent)
    (define/public (get-top-window) (send parent get-top-window))
    (super-new)))

(define-signal-handler connect-menu-key-press "key-press-event"
  (_fun _GtkWidget _GdkEventKey-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)])
      (or (not wx)
          (other-modal? wx)))))

(define-signal-handler connect-menu-button-press "button-press-event"
  (_fun _GtkWidget _GdkEventButton-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)])
      (or (not wx)
          (other-modal? wx)))))

(defclass menu-bar% widget%
  (inherit install-widget-parent)

  (define menus null)

  (define gtk (as-gtk-allocation (gtk_menu_bar_new)))
  (super-new [gtk gtk])

  (define/public (get-gtk) gtk)

  (connect-menu-key-press gtk)
  (connect-menu-button-press gtk)

  (define top-wx #f)

  (define/public (set-top-window top)
    (set! top-wx top)
    (install-widget-parent top)
    ;; return initial size; also, add a menu to make sure there is one,
    ;; and force the menu bar to be at least that tall always
    (atomically
     (let ([item (gtk_menu_item_new_with_mnemonic "Xyz")])
       (gtk_menu_shell_append gtk item)
       (gtk_widget_show item)
       (begin0
        (let ([req (make-GtkRequisition 0 0)])
          (gtk_widget_size_request gtk req)
          (gtk_widget_set_usize gtk -1 (GtkRequisition-height req))
          (GtkRequisition-height req))
        (gtk_container_remove gtk item)))))

  (define/public (get-top-window)
    top-wx)

  (define/public (get-dialog-level) 
    (send top-wx get-dialog-level))

  (define/public (set-label-top pos str)
    (let ([l (list-ref menus pos)])
      (let ([item-gtk (car l)])
	(gtk_label_set_text_with_mnemonic (gtk_bin_get_child item-gtk) 
                                          (fixup-mneumonic str)))))
    
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
    (atomically
     (let* ([item (gtk_menu_item_new_with_mnemonic (fixup-mneumonic title))]
            [item-wx (new top-menu% [parent this] [gtk item])])
       (connect-select item)
       (set! menus (append menus (list (list item menu item-wx))))
       (let ([gtk (send menu get-gtk)])
         (g_object_ref gtk)
         (gtk_menu_item_set_submenu item gtk))
       (gtk_menu_shell_append gtk item)
       (gtk_widget_show item)))))
