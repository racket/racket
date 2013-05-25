#lang racket/base
(require racket/class
         ffi/unsafe
          "../../syntax.rkt"
          "../../lock.rkt"
          "../common/freeze.rkt"
          "../common/queue.rkt"
          "widget.rkt"
          "window.rkt"
          "utils.rkt"
          "types.rkt")

(provide
 (protect-out menu-bar%
              gtk_menu_item_new_with_mnemonic
              gtk_menu_shell_append
              fixup-mnemonic))

(define-gtk gtk_menu_bar_new (_fun -> _GtkWidget))
(define-gtk gtk_menu_shell_append (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_menu_item_new_with_mnemonic (_fun _string -> _GtkWidget))
(define-gtk gtk_menu_item_set_submenu (_fun _GtkWidget (_or-null _GtkWidget) -> _void))
(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_label_set_text_with_mnemonic (_fun _GtkWidget _string -> _void))
(define-gtk gtk_bin_get_child (_fun _GtkWidget -> _GtkWidget))

(define-gtk gtk_widget_set_usize (_fun _GtkWidget _int _int -> _void))

(define-gtk ubuntu_menu_proxy_get _fpointer
  #:fail (lambda () #f))

(define (fixup-mnemonic title)
  (mnemonic-string (regexp-replace #rx"\t.*$" title "")))

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

(define-signal-handler connect-ubuntu-local "notify::ubuntu-local"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
	(send wx reset-menu-height)))))

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

  ;; Ubuntu patches Gtk so that a menu bar starts
  ;; as "remote" instead of appearing in a frame.
  ;; For configurations that put the menu bar in a frame,
  ;; the "notify::ubuntu-local" signal is issued.
  (connect-ubuntu-local gtk)

  ; (gtk_menu_set_accel_group gtk the-accelerator-group)

  (define top-wx #f)

  (define/public (set-top-window top)
    (set! top-wx top)
    (when ubuntu_menu_proxy_get
      (send top treat-focus-out-as-menu-click))
    (install-widget-parent top)
    (fix-menu-height))

  (define/public (reset-menu-height)
    (when top-wx
      (send top-wx reset-menu-height (fix-menu-height))))

  (define/private (fix-menu-height)
    ;; a menu to make sure there is one,
    ;; and force the menu bar to be at least that tall always
    (atomically
     (define item 
       (and (null? menus)
	    (gtk_menu_item_new_with_mnemonic "Xyz")))
     (when item
       (gtk_menu_shell_append gtk item)
       (gtk_widget_show item))
     (define req (make-GtkRequisition 0 0))
     (gtk_widget_size_request gtk req)
     (define height (GtkRequisition-height req))
     (gtk_widget_set_usize gtk -1 height)
     (when item
       (gtk_container_remove gtk item))
     height))

  (define/public (get-top-window)
    top-wx)

  (define/public (get-dialog-level) 
    (send top-wx get-dialog-level))

  (define/public (set-label-top pos str)
    (let ([l (list-ref menus pos)])
      (let ([item-gtk (car l)])
	(gtk_label_set_text_with_mnemonic (gtk_bin_get_child item-gtk) 
                                          (fixup-mnemonic str)))))
    
  (define/public (enable-top pos on?)
    (gtk_widget_set_sensitive (car (list-ref menus pos)) on?))

  (define/public (delete which pos)
    (atomically
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
                                      (sub1 pos)))])))))

  (public [append-menu append])
  (define (append-menu menu title)
    (send menu set-parent this)
    (atomically
     (let* ([item (let ([title (fixup-mnemonic title)])
                    (as-gtk-allocation
                     (gtk_menu_item_new_with_mnemonic title)))]
            [item-wx (new top-menu% [parent this] [gtk item])])
       (connect-select item)
       (set! menus (append menus (list (list item menu item-wx))))
       (let ([gtk (send menu get-gtk)])
         (g_object_ref gtk)
         (gtk_menu_item_set_submenu item gtk))
       (gtk_menu_shell_append gtk item)
       (gtk_widget_show item))))

  (define/public (activate-item menu)
    ;; Gtk takes care of menu activation as appropriate;
    ;; return #f to indcate that the key wasn't handled
    #f
    #;
    (let loop ([menus menus])
      (cond
       [(null? menus) (void)]
       [(eq? menu (cadar menus))
	(gtk_menu_shell_select_item gtk (caar menus))]
       [else (loop (cdr menus))]))))
