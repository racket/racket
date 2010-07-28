#lang scheme/base
(require scheme/class
         scheme/foreign
         "widget.rkt"
         "window.rkt"
          "../../syntax.rkt"
         "types.rkt"
         "const.rkt"
         "utils.rkt"
         "menu-bar.rkt"
         "../common/event.rkt")
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
(define-gtk gtk_label_set_text_with_mnemonic (_fun _GtkWidget _string -> _void))
(define-gtk gtk_bin_get_child (_fun _GtkWidget -> _GtkWidget))

(define-gtk gtk_get_current_event_time (_fun -> _uint32))
(define-gtk gtk_menu_popup (_fun _GtkWidget _pointer _pointer 
                                 (_fun _GtkWidget _pointer _pointer _pointer -> _void)
                                 _pointer _uint _uint32
                                 -> _void))

(define-signal-handler connect-menu-item-activate "activate"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (send wx do-on-select))))

(define-signal-handler connect-menu-deactivate "deactivate"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (send wx do-no-selected))))

(define menu-item-handler%
  (class widget%
    (init gtk)
    (init-field menu
                menu-item)
    (super-new [gtk gtk])

    (connect-menu-item-activate gtk)

    (define/public (get-item) menu-item)

    (define/public (do-on-select)
      (send menu do-selected menu-item))

    (define/public (on-select)
      (send menu on-select-item menu-item))))

(defclass menu% widget%
  (init label
        callback
        font)

  (define cb callback)
  
  (define gtk (gtk_menu_new))
  (define/public (get-gtk) gtk)

  (super-new [gtk gtk])

  (connect-menu-deactivate gtk)

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

  (define on-popup #f)
  (define cancel-none-box (box #t))

  (define/public (popup x y queue-cb)
    (set! on-popup queue-cb)
    (set! cancel-none-box (box #f))
    (gtk_menu_popup gtk 
                    #f 
                    #f
                    (lambda (menu _x _y _push)
                      (ptr-set! _x _int x)
                      (ptr-set! _y _int y)
                      (ptr-set! _push _gboolean #t))
                    #f
                    0
                    (gtk_get_current_event_time)))

  (define/public (do-selected menu-item)
    ;; Called in event-pump thread
    (let ([top (get-top-parent)])
      (cond
       [top
        (queue-window-event
         top
         (lambda () (send top on-menu-command menu-item)))]
       [on-popup
        (let* ([e (new popup-event% [event-type 'menu-popdown])]
               [pu on-popup]
               [cnb cancel-none-box])
          (set! on-popup #f)
          (set-box! cancel-none-box #t)
          (send e set-menu-id menu-item)
          (pu (lambda () (cb this e))))]
       [parent (send parent do-selected menu-item)])))

  (define/public (do-no-selected)
    ;; Queue a none-selected event, but only tentatively, because
    ;; the selection event may come later and cancel the none-selected
    ;; event.
    (when on-popup
      (let* ([e (new popup-event% [event-type 'menu-popdown])]
             [pu on-popup]
             [cnb cancel-none-box])
        (send e set-menu-id #f)
        (pu (lambda () 
              (when (eq? on-popup pu)
                (set! on-popup #f))
              (unless (unbox cnb) 
                (cb this e)))))))
  
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
        (gtk_label_set_text_with_mnemonic (gtk_bin_get_child gtk) 
                                          (fixup-mneumonic str)))))

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
