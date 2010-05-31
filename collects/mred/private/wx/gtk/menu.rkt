#lang scheme/base
(require scheme/class
         scheme/foreign
         "widget.rkt"
         "window.rkt"
          "../../syntax.rkt"
         "types.rkt"
         "const.rkt"
         "utils.rkt"
         "menu-bar.rkt")
(unsafe!)

(provide menu%)

(define-gtk gtk_menu_new (_fun -> _GtkWidget))
(define-gtk gtk_check_menu_item_new_with_mnemonic (_fun _string -> _GtkWidget))
(define-gtk gtk_separator_menu_item_new (_fun -> _GtkWidget))
(define-gdk gdk_unicode_to_keyval (_fun _uint32 -> _uint))
(define-gtk gtk_menu_item_set_accel_path (_fun _GtkWidget _string -> _void))
(define-gtk gtk_accel_map_add_entry (_fun _string _uint _int -> _void))
(define-gtk gtk_check_menu_item_set_active (_fun _GtkWidget _gboolean -> _void))
(define-gtk gtk_check_menu_item_get_active (_fun _GtkWidget -> _gboolean))
(define-gtk gtk_menu_item_set_label  (_fun _GtkWidget _string -> _void))
(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))

(define-signal-handler connect-menu-item-activate "activate"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (send wx do-on-select))))

(define menu-item-handler%
  (class widget%
    (init gtk)
    (init-field menu
                menu-item)
    (super-new [gtk gtk])

    (connect-menu-item-activate gtk)

    (define/public (get-item) menu-item)

    (define/public (do-on-select)
      (let ([top (send menu get-top-parent)])
        (when top
          (queue-window-event
           top
           (lambda () (send top on-menu-command menu-item))))))

    (define/public (on-select)
      (send menu on-select-item menu-item))))

(defclass menu% widget%
  (init label
        callback
        font)

  (define gtk (gtk_menu_new))
  (define/public (get-gtk) gtk)

  (super-new [gtk gtk])

  (define items null)

  (define parent #f)
  (define/public (set-parent p)
    (set! parent p))
  (define/public (get-top-parent)
    ;; Maybe be called in Gtk event-handler thread
    (and parent
         (if (parent . is-a? . menu%)
             (send parent get-top-parent)
             (send parent get-top-window))))

  (define/private (adjust-shortcut item-gtk title)
    (cond
     [(regexp-match #rx"\tCtrl[+](.)$" title)
      => (lambda (m)
           (let ([code (gdk_unicode_to_keyval 
                        (char->integer
                         (string-ref (cadr m) 0)))])
             (unless (zero? code)
               (let ([accel-path (format "<MrEd>/Thing/~a" title)])
                 (gtk_accel_map_add_entry accel-path
                                          code
                                          GDK_CONTROL_MASK)
                 (gtk_menu_item_set_accel_path item-gtk accel-path)))))]))
  
  (public [append-item append])
  (define (append-item i label help-str chckable?)
    (let* ([item-gtk ((if chckable?
                          gtk_check_menu_item_new_with_mnemonic
                          gtk_menu_item_new_with_mnemonic)
                      (fixup-mneumonic label))]
           [item (new menu-item-handler% 
                      [gtk item-gtk]
                      [menu this]
                      [menu-item i])])
      (set! items (append items (list (list item item-gtk label chckable?))))
      (adjust-shortcut item-gtk label)
      (gtk_menu_shell_append gtk item-gtk)
      (gtk_widget_show item-gtk)))

  (define/public (append-separator)
    (let ([item-gtk (gtk_separator_menu_item_new)])
      (set! items (append items (list (list #f item-gtk #f #f))))
      (gtk_menu_shell_append gtk item-gtk)
      (gtk_widget_show item-gtk)))

  (def/public-unimplemented select)
  (def/public-unimplemented get-font)
  (def/public-unimplemented set-width)
  (def/public-unimplemented set-title)

  (def/public-unimplemented set-help-string)
  (def/public-unimplemented number)

  (define/private (find-gtk item)
    (for/or ([i items])
      (and (car i)
           (eq? (send (car i) get-item) item)
           (cadr i))))

  (define/public (set-label item str)
    (let ([gtk (find-gtk item)])
      (when gtk
        (gtk_menu_item_set_label gtk str))))

  (define/public (enable item on?)
    (let ([gtk (find-gtk item)])
      (when gtk
        (gtk_widget_set_sensitive gtk on?))))

  (define/public (check item on?)
    (let ([gtk (find-gtk item)])
      (when gtk
        (gtk_check_menu_item_set_active gtk on?))))
    
  (define/public (checked? item)
    (let ([gtk (find-gtk item)])
      (when gtk
        (gtk_check_menu_item_get_active gtk))))

  (define/public (delete-by-position pos)
    (set! items
          (let loop ([items items]
                     [pos pos])
            (cond
             [(null? items) null]
             [(zero? pos)
              (gtk_container_remove gtk (cadar items))
              (cdr items)]
             [else (cons (car items)
                         (loop (cdr items) (sub1 pos)))]))))
  
  (define/public (delete item)
    (set! items
          (let loop ([items items])
            (cond
             [(null? items) null]
             [(eq? (send (caar items) get-item) item)
              (gtk_container_remove gtk (cadar items))
              (cdr items)]
             [else (cons (car items)
                         (loop (cdr items)))])))))
