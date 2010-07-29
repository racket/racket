#lang scheme/base
(require scheme/foreign
         scheme/class
         "../../syntax.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "window.rkt"
         "client-window.rkt"
         "widget.rkt"
         "procs.rkt"
         "../common/queue.rkt")
(unsafe!)

(provide frame%)

;; ----------------------------------------

(define GDK_GRAVITY_NORTH_WEST 1)
(define GDK_GRAVITY_STATIC 10)


(define-gtk gtk_window_new (_fun _int -> _GtkWidget))
(define-gtk gtk_window_set_title (_fun _GtkWindow _string -> _void))
(define-gtk gtk_fixed_new (_fun _gboolean _int -> _GtkWidget))
(define-gtk gtk_fixed_move (_fun _GtkWidget _GtkWidget _int _int -> _void))
(define-gtk gtk_window_get_size (_fun _GtkWidget (w : (_ptr o _int)) (h : (_ptr o _int))
                                      -> _void
                                      -> (values w h)))
(define-gtk gtk_window_set_decorated (_fun _GtkWidget _gboolean -> _void))
(define-gtk gtk_window_maximize (_fun _GtkWidget -> _void))
(define-gtk gtk_window_unmaximize (_fun _GtkWidget -> _void))
(define-gtk gtk_widget_set_uposition (_fun _GtkWidget _int _int -> _void))
(define-gtk gtk_window_get_position (_fun _GtkWidget (x : (_ptr o _int)) (y : (_ptr o _int)) 
                                          -> _void
                                          -> (values x y)))
(define-gtk gtk_window_set_gravity (_fun _GtkWindow _int -> _void))

(define (handle-delete gtk)
  (let ([wx (gtk->wx gtk)])
    (queue-window-event wx (lambda () 
                             (when (send wx on-close)
                               (send wx direct-show #f))))))
(define handle_delete
  (function-ptr handle-delete
                (_fun #:atomic? #t _GtkWidget -> _gboolean)))

(define (handle-configure gtk)
  (let ([wx (gtk->wx gtk)])
    (queue-window-event wx (lambda () 
                             (send wx on-size 0 0)))
    #f))
(define handle_configure
  (function-ptr handle-configure
                (_fun #:atomic? #t _GtkWidget -> _gboolean)))

(define-cstruct _GdkEventWindowState ([type _int]
                                      [window _GtkWindow]
                                      [send_event _int8]
                                      [changed_mask _int]
                                      [new_window_state _int]))


(define-signal-handler connect-window-state "window-state-event"
  (_fun _GtkWidget _GdkEventWindowState-pointer -> _gboolean)
  (lambda (gtk evt)
    (let ([wx (gtk->wx gtk)])
      (send wx on-window-state 
            (GdkEventWindowState-changed_mask evt)
            (GdkEventWindowState-new_window_state evt)))
    #f))

(define frame%
  (class (client-size-mixin window%)
    (init parent
          label
          x y w h
          style)
    (init [is-dialog? #f])
   
    (inherit get-gtk set-size on-size
             pre-on-char pre-on-event
             get-client-delta)

    (define gtk (gtk_window_new GTK_WINDOW_TOPLEVEL))
    (when (memq 'no-caption style)
      (gtk_window_set_decorated gtk #f))
    (define vbox-gtk (gtk_vbox_new #f 0))
    (define panel-gtk (gtk_fixed_new #f 10))
    (gtk_container_add gtk vbox-gtk)
    (gtk_box_pack_end vbox-gtk panel-gtk #t #t 0)
    (gtk_widget_show vbox-gtk)
    (gtk_widget_show panel-gtk)

    (define/override (get-client-gtk) panel-gtk)
    (define/override (get-window-gtk) gtk)

    (super-new [parent parent]
               [gtk gtk]
               [client-gtk panel-gtk]
               [no-show? #t]
               [add-to-parent? #f]
               [extra-gtks (list panel-gtk)])

    (set-size x y w h)

    (g_signal_connect gtk "delete_event" handle_delete)
    ;; (g_signal_connect gtk "configure_event" handle_configure)

    (when label
      (gtk_window_set_title gtk label))

    (define/public (set-child-position child-gtk x y)
      (gtk_fixed_move panel-gtk child-gtk x y))

    (define/public (on-close) (void))

    (define/public (set-menu-bar mb)
      (send mb set-top-window this)
      (let ([mb-gtk (send mb get-gtk)])
        (gtk_box_pack_start vbox-gtk mb-gtk #t #t 0)
        (gtk_widget_show mb-gtk)))

    (define/public (enforce-size min-x min-y max-x max-y inc-x inc-y)
      (void))

    (define/override (get-top-win) this)

    (define dc-lock (and (eq? 'windows (system-type)) (make-semaphore 1)))
    (define/public (get-dc-lock) dc-lock)

    (define/override (center dir wrt)
      (let ([w-box (box 0)]
            [h-box (box 0)]
            [sw-box (box 0)]
            [sh-box (box 0)])
        (get-size w-box h-box)
        (display-size sw-box sh-box #t)
        (let* ([sw (unbox sw-box)]
               [sh (unbox sh-box)]
               [fw (unbox w-box)]
               [fh (unbox h-box)])
          (set-top-position (if (or (eq? dir 'both)
                                    (eq? dir 'horizontal))
                                (quotient (- sw fw) 2)
                                -11111)
                            (if (or (eq? dir 'both)
                                    (eq? dir 'vertical))
                                (quotient (- sh fh) 2)
                                -11111)))))

    (define/override (set-top-position x y)
      (gtk_widget_set_uposition gtk
                                (if (= x -11111) -2 x)
                                (if (= y -11111) -2 y)))

    (define/override (get-size wb hb)
      (let-values ([(w h) (gtk_window_get_size gtk)])
        (set-box! wb w)
        (set-box! hb h)))

    (define/override (direct-show on?)
      (super direct-show on?)
      (register-frame-shown this on?))

    (define/override (on-client-size w h)
      (on-size w h))

    (define/augment (is-enabled-to-root?) #t)

    (define/public (set-icon bm mask mode) (void)) ;; FIXME
    
    (define/override (call-pre-on-event w e)
      (pre-on-event w e))
    (define/override (call-pre-on-char w e)
      (pre-on-char w e))

    (define/override (client-to-screen x y)
      (gtk_window_set_gravity gtk GDK_GRAVITY_STATIC)
      (let-values ([(dx dy) (gtk_window_get_position gtk)]
                   [(cdx cdy) (get-client-delta)])
        (gtk_window_set_gravity gtk GDK_GRAVITY_NORTH_WEST)
        (set-box! x (+ (unbox x) dx cdx))
        (set-box! y (+ (unbox y) dy cdy))))

    (def/public-unimplemented on-toolbar-click)
    (def/public-unimplemented on-menu-click)
    (def/public-unimplemented on-menu-command)
    (def/public-unimplemented on-mdi-activate)
    (def/public-unimplemented on-activate)
    (def/public-unimplemented designate-root-frame)
    (def/public-unimplemented system-menu)

    (define/public (set-modified mod?) (void))

    (define/public (create-status-line) (void))
    (define/public (set-status-text s) (void))
    (def/public-unimplemented status-line-exists?)

    (define maximized? #f)
    
    (define/public (is-maximized?)
      maximized?)
    (define/public (maximize on?)
      ((if on? gtk_window_maximize gtk_window_unmaximize) gtk))

    (define/public (on-window-state changed value)
      (when (positive? (bitwise-and changed GDK_WINDOW_STATE_MAXIMIZED))
        (set! maximized? (positive? (bitwise-and value GDK_WINDOW_STATE_MAXIMIZED)))))

    (def/public-unimplemented iconized?)
    (def/public-unimplemented get-menu-bar)
    (def/public-unimplemented iconize)
    (define/public (set-title s)
      (gtk_window_set_title gtk s))))

