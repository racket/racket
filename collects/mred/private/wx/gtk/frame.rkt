#lang scheme/base
(require scheme/foreign
         scheme/class
         "../../syntax.rkt"
         "../../lock.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "window.rkt"
         "client-window.rkt"
         "widget.rkt"
         "procs.rkt"
         "cursor.rkt"
         "../common/queue.rkt")
(unsafe!)

(provide frame%)

;; ----------------------------------------

(define GDK_GRAVITY_NORTH_WEST 1)
(define GDK_GRAVITY_STATIC 10)


(define-gtk gtk_window_new (_fun _int -> _GtkWidget))
(define-gtk gtk_window_set_title (_fun _GtkWindow _string -> _void))
(define-gtk gtk_fixed_new (_fun -> _GtkWidget))
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

(define-gtk gtk_window_resize (_fun _GtkWidget _int _int -> _void))

(define-gdk gdk_window_set_cursor (_fun _GdkWindow _pointer -> _void))

(define-cstruct _GdkGeometry ([min_width _int]
                              [min_height _int]
                              [max_width _int]
                              [max_height _int]
                              [base_width _int]
                              [base_height _int]
                              [width_inc _int]
                              [height_inc _int]
                              [min_aspect _double]
                              [max_aspect _double]
                              [win_gravity _int]))
(define-gtk gtk_window_set_geometry_hints (_fun _GtkWindow _GtkWidget _GdkGeometry-pointer _int -> _void))


(define-signal-handler connect-delete "delete-event"
  (_fun _GtkWidget -> _gboolean)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (queue-window-event wx (lambda () 
                                 (unless (other-modal? wx)
                                   (when (send wx on-close)
                                     (send wx direct-show #f)))))))))

(define-signal-handler connect-configure "configure-event"
  (_fun _GtkWidget _GdkEventConfigure-pointer -> _gboolean)
  (lambda (gtk a)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx remember-size 
              (GdkEventConfigure-width a)
              (GdkEventConfigure-height a))))
    #f))

(define-cstruct _GdkEventWindowState ([type _int]
                                      [window _GtkWindow]
                                      [send_event _int8]
                                      [changed_mask _int]
                                      [new_window_state _int]))


(define-signal-handler connect-window-state "window-state-event"
  (_fun _GtkWidget _GdkEventWindowState-pointer -> _gboolean)
  (lambda (gtk evt)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx on-window-state 
              (GdkEventWindowState-changed_mask evt)
              (GdkEventWindowState-new_window_state evt))))
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
             get-client-delta get-size
             get-parent get-eventspace
             adjust-client-delta)

    (define gtk (as-gtk-window-allocation
                 (gtk_window_new GTK_WINDOW_TOPLEVEL)))
    (when (memq 'no-caption style)
      (gtk_window_set_decorated gtk #f))    
    (define-values (vbox-gtk panel-gtk)
      (atomically
       (let ([vbox-gtk (gtk_vbox_new #f 0)]
             [panel-gtk (gtk_fixed_new)])
         (gtk_container_add gtk vbox-gtk)
         (gtk_box_pack_end vbox-gtk panel-gtk #t #t 0)
         (values vbox-gtk panel-gtk))))
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

    (connect-delete gtk)
    (connect-configure gtk)

    (when label
      (gtk_window_set_title gtk label))

    (define/override (set-child-size child-gtk x y w h)
      (gtk_fixed_move panel-gtk child-gtk x y)
      (gtk_widget_set_size_request child-gtk w h))

    (define/public (on-close) (void))

    (define/public (set-menu-bar mb)
      (let ([mb-gtk (send mb get-gtk)])
        (gtk_box_pack_start vbox-gtk mb-gtk #t #t 0)
        (gtk_widget_show mb-gtk))
      (let ([h (send mb set-top-window this)])
        ;; adjust client delta right away, so that we make
        ;; better assumptions about the client size and more
        ;; quickly converge to the right size of the frame
        ;; based on its content
        (adjust-client-delta 0 h))
      ;; Hack: calls back into the mred layer to re-compute
      ;;  sizes. By calling this early enough, the frame won't 
      ;;  grow if it doesn't have to grow to accomodate the menu bar.
      (send this resized))

    (define saved-enforcements (vector 0 0 -1 -1))

    (define/public (enforce-size min-x min-y max-x max-y inc-x inc-y)
      (define (to-max v) (if (= v -1) #x3FFFFFFF v))
      (set! saved-enforcements (vector min-x min-y max-x max-y))
      (gtk_window_set_geometry_hints gtk gtk 
                                     (make-GdkGeometry min-x min-y
                                                       (to-max max-x) (to-max max-y)
                                                       0 0
                                                       inc-x inc-y
                                                       0.0 0.0
                                                       0)
                                     (bitwise-ior GDK_HINT_MIN_SIZE
                                                  GDK_HINT_MAX_SIZE
                                                  GDK_HINT_RESIZE_INC)))

    (define/override (get-top-win) this)

    (define dc-lock (and (eq? 'windows (system-type)) (make-semaphore 1)))
    (define/public (get-dc-lock) dc-lock)

    (define/override (get-dialog-level) 0)
    (define/public (frame-relative-dialog-status win) #f)

    (define/override (get-unset-pos) -11111)

    (define/override (center dir wrt)
      (let ([w-box (box 0)]
            [h-box (box 0)]
            [sx-box (box 0)]
            [sy-box (box 0)]
            [sw-box (box 0)]
            [sh-box (box 0)])
        (get-size w-box h-box)
        (let ([p (get-parent)])
          (if p
              (begin
                (send p get-size sw-box sh-box)
                (set-box! sx-box (send p get-x))
                (set-box! sy-box (send p get-y)))
              (display-size sw-box sh-box #t)))
        (let* ([sw (unbox sw-box)]
               [sh (unbox sh-box)]
               [fw (unbox w-box)]
               [fh (unbox h-box)])
          (set-top-position (if (or (eq? dir 'both)
                                    (eq? dir 'horizontal))
                                (+ (unbox sx-box) (quotient (- sw fw) 2))
                                -11111)
                            (if (or (eq? dir 'both)
                                    (eq? dir 'vertical))
                                (+ (unbox sy-box) (quotient (- sh fh) 2))
                                -11111)))))

    (define/public (set-top-position x y)
      (when (and (vector? saved-enforcements)
                 (or (x . < . (vector-ref saved-enforcements 0))
                     (let ([max-x (vector-ref saved-enforcements 1)])
                       (and (max-x . > . -1) (x . > . max-x)))
                     (y . < . (vector-ref saved-enforcements 2))
                     (let ([max-y (vector-ref saved-enforcements 3)])
                       (and (max-y . > . -1) (y . > . max-y)))))
        (enforce-size 0 0 -1 -1 1 1))
      (gtk_widget_set_uposition gtk
                                (if (= x -11111) -2 x)
                                (if (= y -11111) -2 y)))

    (define/override (really-set-size gtk x y w h)
      (set-top-position x y)
      (gtk_window_resize gtk (max 1 w) (max 1 h)))

    (define/override (show on?)
      (when (and on?
                 (eventspace-shutdown? (get-eventspace)))
        (error (string->symbol
                (format "show method in ~a"
                        (if (frame-relative-dialog-status this)
                            'dialog%
                            'frame%)))
               "eventspace has been shutdown"))
      (super show on?))

    (define saved-child #f)
    (define/override (register-child child on?)
      (unless on? (error 'register-child-in-frame "did not expect #f"))
      (unless (or (not saved-child) (eq? child saved-child))
        (error 'register-child-in-frame "expected only one child"))
      (set! saved-child child))
    (define/override (register-child-in-parent on?)
      (void))

    (define/override (direct-show on?)
      (super direct-show on?)
      (register-frame-shown this on?))

    (define/public (destroy)
      (direct-show #f))

    (define/override (on-client-size w h)
      (void))

    (define/augment (is-enabled-to-root?) #t)

    (define/public (set-icon bm mask [mode 'both]) (void)) ;; FIXME
    
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

    (define waiting-cursor? #f)
    (define/public (set-wait-cursor-mode on?)
      (set! waiting-cursor? on?)
      (send in-window enter-window))

    (define current-cursor-handle #f)
    (define in-window #f)
    (define/override (set-parent-window-cursor in-win c)
      (set! in-window in-win)
      (let ([c (if waiting-cursor?
                   (get-watch-cursor-handle)
                   c)])
        (unless (eq? c current-cursor-handle)
          (atomically
           (set! current-cursor-handle c)
           (gdk_window_set_cursor (widget-window (get-gtk)) (if (eq? c (get-arrow-cursor-handle))
                                                                #f
                                                                c))))))
    (define/override (enter-window) (void))
    (define/override (leave-window) (void))

    (define/override (check-window-cursor win)
      (send in-window enter-window))
      
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

