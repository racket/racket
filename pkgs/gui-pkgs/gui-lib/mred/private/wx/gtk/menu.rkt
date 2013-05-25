#lang racket/base
(require racket/class
         ffi/unsafe
         "widget.rkt"
         "window.rkt"
          "../../syntax.rkt"
          "../../lock.rkt"
         "types.rkt"
         "const.rkt"
         "utils.rkt"
         "menu-bar.rkt"
         "../common/event.rkt")

(provide 
 (protect-out menu%))

(define-gtk gtk_menu_new (_fun -> _GtkWidget))
(define-gtk gtk_check_menu_item_new_with_mnemonic (_fun _string -> _GtkWidget))
(define-gtk gtk_separator_menu_item_new (_fun -> _GtkWidget))
(define-gdk gdk_unicode_to_keyval (_fun _uint32 -> _uint))
(define-gtk gtk_menu_item_set_accel_path (_fun _GtkWidget _string -> _void))
(define-gtk gtk_accel_map_add_entry (_fun _string _uint _int -> _void))
(define-gtk gtk_check_menu_item_set_active (_fun _GtkWidget _gboolean -> _void))
(define-gtk gtk_check_menu_item_get_active (_fun _GtkWidget -> _gboolean))
(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_label_set_text_with_mnemonic (_fun _GtkWidget _string -> _void))
(define-gtk gtk_bin_get_child (_fun _GtkWidget -> _GtkWidget))
(define-gtk gtk_menu_item_set_submenu (_fun _GtkWidget (_or-null _GtkWidget) -> _void))

(define-gtk gtk_get_current_event_time (_fun -> _uint32))
(define-gtk gtk_menu_popup (_fun _GtkWidget _pointer _pointer 
                                 (_fun _GtkWidget _pointer _pointer _pointer -> _void)
                                 _pointer _uint _uint32
                                 -> _void))

(define-gtk gtk_widget_get_screen (_fun _GtkWidget -> _GdkScreen))
(define-gdk gdk_screen_get_width (_fun _GdkScreen -> _int))
(define-gdk gdk_screen_get_height (_fun _GdkScreen -> _int))

(define-signal-handler connect-menu-item-activate "activate"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx do-on-select)))))

(define-signal-handler connect-menu-deactivate "deactivate"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx do-no-selected)))))

(define menu-item-handler%
  (class widget%
    (init gtk)
    (init-field menu
                menu-item)
    (super-new [gtk gtk])

    (connect-menu-item-activate gtk)

    (define/public (get-item) menu-item)

    (define/public (removing-item) (void))

    (define/public (do-on-select)
      (send menu do-selected menu-item))

    (define/public (on-select)
      (send menu on-select-item menu-item))))

(define separator-item-handler%
  (class object%
    (define/public (get-item) #f)
    (define/public (removing-item) (void))
    (super-new)))

(defclass menu% widget%
  (init label
        callback
        font)

  (inherit install-widget-parent)

  (define cb callback)
  
  (define gtk (as-gtk-allocation (gtk_menu_new)))
  (define/public (get-gtk) gtk)

  (super-new [gtk gtk])

  (connect-menu-deactivate gtk)

  (gtk_menu_set_accel_group gtk the-accelerator-group)

  (define items null)

  (define parent #f)
  (define/public (set-parent p)
    (set! parent p)
    (install-widget-parent p))
  (define/public (get-top-parent)
    ;; Maybe be called in Gtk event-handler thread
    (and parent
         (if (parent . is-a? . menu%)
             (send parent get-top-parent)
             (send parent get-top-window))))

  (define self-item #f)
  (define remover void)
  (define/public (set-self-item i r) (set! self-item i) (set! remover r))
  (define/public (get-item) self-item)
  (define/public (removing-item) 
    (set! self-item #f)
    (remover)
    (set! remover void))

  (define on-popup #f)
  (define cancel-none-box (box #t))

  (define/public (popup x y queue-cb)
    (set! on-popup queue-cb)
    (set! cancel-none-box (box #f))
    (gtk_menu_popup gtk 
                    #f 
                    #f
                    (lambda (menu _x _y _push)
                      (let ([r (make-GtkRequisition 0 0)])
                        (gtk_widget_size_request menu r)
                        ;; Try to keep the menu on the screen:
                        (let* ([s (gtk_widget_get_screen menu)]
                               [sw (gdk_screen_get_width s)]
                               [sh (gdk_screen_get_height s)])
                          (ptr-set! _x _int (min x
                                                 (max 0
                                                      (- sw
                                                         (GtkRequisition-width r)))))
                          (ptr-set! _y _int (min y
                                                 (max 0
                                                      (- sh
                                                         (GtkRequisition-height r)))))))
                      (ptr-set! _push _gboolean #t))
                    #f
                    0
                    (gtk_get_current_event_time)))

  (define ignore-callback? #f)

  (define/public (do-selected menu-item)
    ;; Called in event-pump thread
    (unless ignore-callback?
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
         [parent (send parent do-selected menu-item)]))))

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
  
  (define/private (adjust-shortcut item-gtk title need-clear?)
    (let ([m (regexp-match #rx"\t(Ctrl[+])?(Shift[+])?(Meta[+])?(Alt[+])?(.|[0-9]+)$" 
                           title)])
      (if m
          (let ([mask (+ (if (list-ref m 1) GDK_CONTROL_MASK 0)
                         (if (list-ref m 2) GDK_SHIFT_MASK 0)
                         (if (list-ref m 3) GDK_MOD1_MASK 0)
                         (if (list-ref m 4) GDK_META_MASK 0))]
                [code (let ([s (list-ref m 5)])
                        (if (= 1 (string-length s))
                            (gdk_unicode_to_keyval 
                             (char->integer (string-ref s 0)))
                            (string->number s)))])
            (unless (zero? code)
              (let ([accel-path (format "<GRacket>/Hardwired/~a" title)])
                (gtk_accel_map_add_entry accel-path
                                         code
                                         mask)
                (gtk_menu_item_set_accel_path item-gtk accel-path))))
          (when need-clear?
            (gtk_menu_item_set_accel_path item-gtk #f)))))
  
  (public [append-item append])
  (define (append-item i label help-str-or-submenu chckable?)
    (atomically
     (let ([item-gtk (let ([label (fixup-mnemonic label)])
                       (as-gtk-allocation
                        ((if (and chckable?
                                  (not (help-str-or-submenu . is-a? . menu%)))
                             gtk_check_menu_item_new_with_mnemonic
                             gtk_menu_item_new_with_mnemonic)
                         label)))])
       (if (help-str-or-submenu . is-a? . menu%)
           (let ([submenu help-str-or-submenu])
             (let ([gtk (send submenu get-gtk)])
               (g_object_ref gtk)
               (gtk_menu_item_set_submenu item-gtk gtk)
               (send submenu set-parent this)
               (send submenu set-self-item i
                     (lambda () (gtk_menu_item_set_submenu item-gtk #f)))
               (set! items (append items (list (list submenu item-gtk label chckable?))))))
           (let ([item (new menu-item-handler% 
                            [gtk item-gtk]
                            [menu this]
                            [menu-item i]
                            [parent this])])
             (set! items (append items (list (list item item-gtk label chckable?))))
             (adjust-shortcut item-gtk label #f)))
       (gtk_menu_shell_append gtk item-gtk)
       (gtk_widget_show item-gtk))))

  (define/public (append-separator)
    (atomically
     (let ([item-gtk (as-gtk-allocation (gtk_separator_menu_item_new))])
       (set! items (append items (list (list (new separator-item-handler%) item-gtk #f #f))))
       (gtk_menu_shell_append gtk item-gtk)
       (gtk_widget_show item-gtk))))

  (define/public (select bm)
    (send parent activate-item this))

  (def/public-unimplemented get-font)
  (def/public-unimplemented set-width)
  (def/public-unimplemented set-title)

  (define/public (set-help-string m s) (void))

  (define/public (number) (length items))

  (define/private (find-gtk item)
    (for/or ([i items])
      (and (car i)
           (eq? (send (car i) get-item) item)
           (cadr i))))

  (define/public (set-label item str)
    (let ([gtk (find-gtk item)])
      (when gtk
        (gtk_label_set_text_with_mnemonic (gtk_bin_get_child gtk) 
                                          (fixup-mnemonic str))
        (adjust-shortcut gtk str #t))))

  (define/public (enable item on?)
    (let ([gtk (find-gtk item)])
      (when gtk
        (gtk_widget_set_sensitive gtk on?))))

  (define/public (check item on?)
    (let ([gtk (find-gtk item)])
      (when gtk
        (atomically
         (set! ignore-callback? #t)
         (gtk_check_menu_item_set_active gtk on?)
         (set! ignore-callback? #f)))))
    
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
              (send (caar items) removing-item)
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
              (send (caar items) removing-item)
              (gtk_container_remove gtk (cadar items))
              (cdr items)]
             [else (cons (car items)
                         (loop (cdr items)))])))))
