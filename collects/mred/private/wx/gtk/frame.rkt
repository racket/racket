#lang racket/base
(require ffi/unsafe
         racket/class
         racket/promise
         racket/runtime-path
         racket/draw
         (for-syntax (only-in racket/base quote))
         "../../syntax.rkt"
         "../../lock.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "window.rkt"
         "client-window.rkt"
         "widget.rkt"
         "cursor.rkt"
         "pixbuf.rkt"
         "../common/queue.rkt")

(provide 
 (protect-out frame%
              display-origin
              display-size
	      display-count
              location->window
              get-current-mouse-state))

;; ----------------------------------------

(define GDK_GRAVITY_NORTH_WEST 1)
(define GDK_GRAVITY_STATIC 10)

(define _GList (_cpointer/null 'GList))
(define-glib g_list_insert (_fun _GList _pointer _int -> _GList))
(define-glib g_list_free (_fun _GList -> _void))

(define-gtk gtk_window_new (_fun _int -> _GtkWidget))
(define-gtk gtk_window_set_title (_fun _GtkWindow _string -> _void))
(define-gtk gtk_fixed_new (_fun -> _GtkWidget))
(define-gtk gtk_fixed_move (_fun _GtkWidget _GtkWidget _int _int -> _void))
(define-gtk gtk_window_get_size (_fun _GtkWidget (w : (_ptr o _int)) (h : (_ptr o _int))
                                      -> _void
                                      -> (values w h)))
(define-gtk gtk_window_set_decorated (_fun _GtkWidget _gboolean -> _void))
(define-gtk gtk_window_set_keep_above (_fun _GtkWidget _gboolean -> _void))
(define-gtk gtk_window_set_focus_on_map (_fun _GtkWidget _gboolean -> _void))
(define-gtk gtk_window_maximize (_fun _GtkWidget -> _void))
(define-gtk gtk_window_unmaximize (_fun _GtkWidget -> _void))
(define-gtk gtk_widget_set_uposition (_fun _GtkWidget _int _int -> _void))
(define-gtk gtk_window_get_position (_fun _GtkWidget (x : (_ptr o _int)) (y : (_ptr o _int)) 
                                          -> _void
                                          -> (values x y)))
(define-gtk gtk_window_set_gravity (_fun _GtkWindow _int -> _void))
(define-gtk gtk_window_set_icon_list (_fun _GtkWindow _GList -> _void))
(define-gtk gtk_window_fullscreen (_fun _GtkWindow -> _void))
(define-gtk gtk_window_get_focus (_fun _GtkWindow -> (_or-null _GtkWidget)))

(define-gtk gtk_window_resize (_fun _GtkWidget _int _int -> _void))

(define-gdk gdk_window_set_cursor (_fun _GdkWindow _pointer -> _void))
(define-gdk gdk_screen_get_root_window (_fun _GdkScreen -> _GdkWindow))
(define-gdk gdk_window_get_pointer (_fun _GdkWindow 
                                         (x : (_ptr o _int))
                                         (y : (_ptr o _int))
                                         (mods : (_ptr o _uint))
                                         -> _GdkWindow
                                         -> (values x y mods)))

(define-gtk gtk_window_iconify (_fun _GtkWindow -> _void))
(define-gtk gtk_window_deiconify (_fun _GtkWindow -> _void))

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
              (GdkEventConfigure-x a)
              (GdkEventConfigure-y a)
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

(define-runtime-path plt-16x16-file '(lib "icons/plt-16x16.png"))
(define-runtime-path plt-32x32-file '(lib "icons/plt-32x32.png"))
(define-runtime-path plt-48x48-file '(lib "icons/plt-48x48.png"))

(define icon-pixbufs+glist
  (delay
    (let ([icons (map
                  (lambda (fn)
                    (bitmap->pixbuf (make-object bitmap% fn 'png/alpha)))
                  (list plt-16x16-file
                        plt-32x32-file
                        plt-48x48-file))])
      (cons 
       ;; keep pixbuf pointers to avoid GC:
       icons
       ;; a glist:
       (for/fold ([l #f]) ([i (in-list icons)])
	 (g_list_insert l i -1))))))

;; used for location->window
(define all-frames (make-weak-hasheq))

(define frame%
  (class (client-size-mixin window%)
    (init parent
          label
          x y w h
          style)
    (init [is-dialog? #f])

    (inherit get-gtk set-size
             pre-on-char pre-on-event
             get-client-delta get-size
             get-parent get-eventspace
             adjust-client-delta
             queue-on-size)

    (define gtk (as-gtk-window-allocation
                 (gtk_window_new (if (memq 'float style)
				     GTK_WINDOW_POPUP
				     GTK_WINDOW_TOPLEVEL))))
    (when (memq 'no-caption style)
      (gtk_window_set_decorated gtk #f))
    (when (memq 'float style)
      (gtk_window_set_keep_above gtk #t)
      (gtk_window_set_focus_on_map gtk #f))
    (define-values (vbox-gtk panel-gtk)
      (atomically
       (let ([vbox-gtk (gtk_vbox_new #f 0)]
             [panel-gtk (gtk_fixed_new)])
         (gtk_container_add gtk vbox-gtk)
         (gtk_box_pack_end vbox-gtk panel-gtk #t #t 0)
         (values vbox-gtk panel-gtk))))
    (gtk_widget_show vbox-gtk)
    (gtk_widget_show panel-gtk)
    (connect-enter-and-leave gtk)

    (unless is-dialog?
      (gtk_window_set_icon_list gtk (cdr (force icon-pixbufs+glist))))

    (define/override (get-client-gtk) panel-gtk)
    (define/override (get-window-gtk) gtk)

    (super-new [parent parent]
               [gtk gtk]
               [client-gtk panel-gtk]
               [no-show? #t]
               [add-to-parent? #f]
               [extra-gtks (list panel-gtk)]
               [connect-size-allocate? #f])

    (set-size x y w h)

    (when (memq 'hide-menu-bar style)
      (gtk_window_fullscreen gtk))

    (connect-delete gtk)
    (connect-configure gtk)
    (connect-focus gtk)
    (connect-window-state gtk)

    (define saved-title (or label ""))
    (define is-modified? #f)

    (when label
      (gtk_window_set_title gtk label))

    ;(gtk_window_add_accel_group (widget-window gtk) the-accelerator-group)

    (define/override (set-child-size child-gtk x y w h)
      (gtk_fixed_move panel-gtk child-gtk x y)
      (gtk_widget_set_size_request child-gtk w h))

    (define/public (on-close) #t)

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
      ;;  grow if it doesn't have to grow to accommodate the menu bar.
      (send this resized))

    (define/public (reset-menu-height h)
      (adjust-client-delta 0 h))

    (define saved-enforcements (vector 0 0 -1 -1))

    (define/public (enforce-size min-x min-y max-x max-y inc-x inc-y)
      (define (to-max v) (if (= v -1) #x3FFFFF v))
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
              (display-size sw-box sh-box #t 0 void)))
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
      (unless (and (= x -11111) (= y -11111))
        (gtk_widget_set_uposition gtk
                                  (if (= x -11111) -2 x)
                                  (if (= y -11111) -2 y))))

    (define/override (really-set-size gtk x y processed-x processed-y w h)
      (set-top-position x y)
      (gtk_window_resize gtk (max 1 w) (max 1 h)))

    (define/override (show on?)
      (let ([es (get-eventspace)])
        (when (and on?
                   (eventspace-shutdown? es))
          (error (string->symbol
                  (format "show method in ~a"
                          (if (frame-relative-dialog-status this)
                              'dialog%
                              'frame%)))
                 "eventspace has been shutdown")
          (when saved-child
            (if (eq? (current-thread) (eventspace-handler-thread es))
                (send saved-child paint-children)
                (let ([s (make-semaphore)])
                  (queue-callback (lambda ()
                                    (when saved-child
                                      (send saved-child paint-children))
                                    (semaphore-post s)))
                  (sync/timeout 1 s))))))
      (super show on?))

    (define saved-child #f)
    (define/override (register-child child on?)
      (unless on? (error 'register-child-in-frame "did not expect #f"))
      (unless (or (not saved-child) (eq? child saved-child))
        (error 'register-child-in-frame "expected only one child"))
      (set! saved-child child))
    (define/override (register-child-in-parent on?)
      (void))

    (define/override (refresh-all-children)
      (when saved-child
        (send saved-child refresh)))

    (define/override (direct-show on?)
      ;; atomic mode
      (if on?
          (hash-set! all-frames this #t)
          (hash-remove! all-frames this))
      (super direct-show on?)
      (when on? (gtk_window_deiconify gtk))
      (register-frame-shown this on?))

    (define/public (destroy)
      ;; atomic mode
      (direct-show #f))

    (define/override (on-client-size w h)
      (void))

    (define/augment (is-enabled-to-root?) #t)

    (define big-icon #f)
    (define small-icon #f)
    (define/public (set-icon bm [mask #f] [mode 'both])
      (let ([bm (if mask
		    (let* ([nbm (make-object bitmap%
					     (send bm get-width)
					     (send bm get-height)
					     #f
					     #t)]
			   [dc (make-object bitmap-dc% nbm)])
		      (send dc draw-bitmap bm 0 0
			    'solid (make-object color% "black")
			    mask)
		      (send dc set-bitmap #f)
		      nbm)
		    bm)])
	(case mode
	  [(small) (set! small-icon bm)]
	  [(big) (set! big-icon bm)]
	  [(both) 
	   (set! small-icon bm)
	   (set! big-icon bm)])
	(let ([small-pixbuf
	       (if small-icon
		   (bitmap->pixbuf small-icon)
		   (car (car (force icon-pixbufs+glist))))]
	      [big-pixbufs
	       (if big-icon
		   (list (bitmap->pixbuf big-icon))
		   (cdr (car (force icon-pixbufs+glist))))])
	(atomically
	 (let ([l (for/fold ([l #f]) ([i (cons small-pixbuf
					       big-pixbufs)])
		    (g_list_insert l i -1))])
	   (gtk_window_set_icon_list gtk l)
	   (g_list_free l))))))
    
    (define child-has-focus? #f)
    (define reported-activate #f)
    (define queued-active? #f)
    (define/public (on-focus-child on?)
      ;; atomic mode
      (set! child-has-focus? on?)
      (unless queued-active?
	(set! queued-active? #t)
	(queue-window-event this
			    (lambda ()
			      (let ([on? child-has-focus?])
				(set! queued-active? #f)
				(unless (eq? on? reported-activate)
				  (set! reported-activate on?)
				  (on-activate on?)))))))

    (define treat-focus-out-as-menu-click? #f)
    (define/public (treat-focus-out-as-menu-click)
      (set! treat-focus-out-as-menu-click? #t))

    (define focus-here? #f)
    (define/override (on-focus? on?)
      (when (and (not on?) treat-focus-out-as-menu-click?)
        (on-menu-click))
      (on-focus-child on?)
      (cond
       [on?
	(if (ptr-equal? (gtk_window_get_focus gtk) gtk)
	    (begin
	      (set! focus-here? #t)
	      (super on-focus? on?))
	    #f)]
       [focus-here?
	(set! focus-here? #f)
	(super on-focus? on?)]
       [else #f]))

    (define/public (get-focus-window [even-if-not-active? #f])
      (let ([f-gtk (gtk_window_get_focus gtk)])
        (and f-gtk
             (or even-if-not-active?
                 (positive? (bitwise-and (get-gtk-object-flags f-gtk)
                                         GTK_HAS_FOCUS)))
             (gtk->wx f-gtk))))
    
    (define/override (call-pre-on-event w e)
      (pre-on-event w e))
    (define/override (call-pre-on-char w e)
      (pre-on-char w e))

    (define/override (internal-client-to-screen x y)
      (gtk_window_set_gravity gtk GDK_GRAVITY_STATIC)
      (let-values ([(dx dy) (gtk_window_get_position gtk)]
                   [(cdx cdy) (get-client-delta)])
        (gtk_window_set_gravity gtk GDK_GRAVITY_NORTH_WEST)
        (set-box! x (+ (unbox x) dx cdx))
        (set-box! y (+ (unbox y) dy cdy))))

    (define/public (on-toolbar-click) (void))
    (define/public (on-menu-click) (void))

    (define/public (on-menu-command c) (void))

    (def/public-unimplemented on-mdi-activate)

    (define/public (on-activate on?) (void))

    (define/public (designate-root-frame) (void))

    (def/public-unimplemented system-menu)

    (define/public (set-modified mod?)
      (unless (eq? is-modified? (and mod? #t))
        (set! is-modified? (and mod? #t))
        (set-title saved-title)))

    (define waiting-cursor? #f)
    (define/public (set-wait-cursor-mode on?)
      (set! waiting-cursor? on?)
      (when in-window
        (send in-window enter-window)))

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
      (when in-window
        (send in-window enter-window)))
      
    (define maximized? #f)
    (define is-iconized? #f)
    
    (define/public (is-maximized?)
      maximized?)
    (define/public (maximize on?)
      ((if on? gtk_window_maximize gtk_window_unmaximize) gtk))

    (define/public (on-window-state changed value)
      (when (positive? (bitwise-and changed GDK_WINDOW_STATE_MAXIMIZED))
        (set! maximized? (positive? (bitwise-and value GDK_WINDOW_STATE_MAXIMIZED))))
      (when (positive? (bitwise-and changed GDK_WINDOW_STATE_ICONIFIED))
        (set! is-iconized? (positive? (bitwise-and value GDK_WINDOW_STATE_ICONIFIED)))))

    (define/public (iconized?)
      is-iconized?)
    (define/public (iconize on?)
      (if on?
          (gtk_window_iconify gtk)
          (gtk_window_deiconify gtk)))
      
    (def/public-unimplemented get-menu-bar)

    (define/public (set-title s)
      (set! saved-title s)
      (gtk_window_set_title gtk (if is-modified?
                                    (string-append s "*")
                                    s)))
    
    (define/public (display-changed) (void))))

;; ----------------------------------------

(define-gdk gdk_screen_get_width (_fun _GdkScreen -> _int))
(define-gdk gdk_screen_get_height (_fun _GdkScreen -> _int))

(define-gdk gdk_screen_get_monitor_geometry (_fun _GdkScreen _int _GdkRectangle-pointer -> _void))
(define-gdk gdk_screen_get_n_monitors (_fun _GdkScreen -> _int))

(define (monitor-rect num fail)
  (let ([s (gdk_screen_get_default)]
	[r (make-GdkRectangle 0 0 0 0)])
    (unless (num . < . (gdk_screen_get_n_monitors s))
      (fail))
    (gdk_screen_get_monitor_geometry s num r)
    r))

(define (display-origin x y all? num fail)
  (let ([r (monitor-rect num fail)])
    (set-box! x (- (GdkRectangle-x r)))
    (set-box! y (- (GdkRectangle-y r)))))

(define (display-size w h all? num fail)
  (let ([r (monitor-rect num fail)])
    (set-box! w (GdkRectangle-width r))
    (set-box! h (GdkRectangle-height r))))

(define (display-count)
  (gdk_screen_get_n_monitors (gdk_screen_get_default)))

(define (location->window x y)
  (for/or ([f (in-hash-keys all-frames)])
    (let ([fx (send f get-x)]
          [fw (send f get-width)])
      (and (<= fx x (+ fx fw))
           (let ([fy (send f get-y)]
                 [fh (send f get-height)])
             (<= fy y (+ fy fh)))
           f))))

;; ----------------------------------------

(define (get-current-mouse-state)
  (define-values (x y mods) (gdk_window_get_pointer
                             (gdk_screen_get_root_window
                              (gdk_screen_get_default))))
  (define (maybe mask sym)
    (if (zero? (bitwise-and mods mask))
        null
        (list sym)))
  (values (make-object point% x y)
          (append
           (maybe GDK_BUTTON1_MASK 'left)
           (maybe GDK_BUTTON2_MASK 'middle)
           (maybe GDK_BUTTON3_MASK 'right)
           (maybe GDK_SHIFT_MASK 'shift)
           (maybe GDK_LOCK_MASK 'caps)
           (maybe GDK_CONTROL_MASK 'control)
           (maybe GDK_MOD1_MASK 'alt)
           (maybe GDK_META_MASK 'meta))))

(define (tell-all-frames-signal-changed n)
  (define frames (for/list ([f (in-hash-keys all-frames)]) f))
  (for ([f (in-hash-keys all-frames)])
    (define e (send f get-eventspace))
    (unless (eventspace-shutdown? e)
      (parameterize ([current-eventspace e])
        (queue-callback
         (λ () 
           (send f display-changed)))))))

(define-signal-handler 
  connect-monitor-changed-signal
  "monitors-changed"
  (_fun _GdkScreen -> _void)
  (λ (screen) (tell-all-frames-signal-changed 1)))

(define-signal-handler 
  connect-screen-changed-signal
  "screen-changed"
  (_fun _GdkScreen -> _void)
  (λ (screen) (tell-all-frames-signal-changed 2)))

(define-signal-handler 
  connect-composited-changed-signal
  "composited-changed"
  (_fun _GdkScreen -> _void)
  (λ (screen) (tell-all-frames-signal-changed 3)))

(define (screen-size-signal-connect connect-signal)
  (void (connect-signal (cast (gdk_screen_get_default) _GdkScreen _GtkWidget))))
(screen-size-signal-connect connect-monitor-changed-signal)
(screen-size-signal-connect connect-screen-changed-signal)
(screen-size-signal-connect connect-composited-changed-signal)
