#lang racket/base
(require ffi/unsafe
         racket/class
         racket/draw
         ffi/unsafe/alloc
         (except-in racket/draw/private/color
                    color% make-color)
         racket/draw/private/local
         "../common/backing-dc.rkt"
         "../common/canvas-mixin.rkt"
         "../../syntax.rkt"
	 "../../lock.rkt"
         "../common/event.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "window.rkt"
         "client-window.rkt"
         "widget.rkt"
         "dc.rkt"
         "gl-context.rkt"
         "combo.rkt"
         "pixbuf.rkt"
         "gcwin.rkt"
         "panel.rkt")

(provide 
 (protect-out canvas%
              canvas-panel%))

;; ----------------------------------------

(define-gobj g_object_freeze_notify (_fun _GtkWidget -> _void))
(define-gobj g_object_thaw_notify (_fun _GtkWidget -> _void))

(define-gobj g_object_set_double (_fun _GtkWidget _string _double* (_pointer = #f) -> _void)
  #:c-id g_object_set)
(define-gobj g_object_get_double (_fun _GtkWidget _string (r : (_ptr o _double)) (_pointer = #f) 
				       -> _void -> r)
  #:c-id g_object_get)

(define-gtk gtk_drawing_area_new (_fun -> _GtkWidget))

(define-gtk gtk_combo_box_entry_new_text (_fun -> _GtkWidget))
(define-gtk gtk_combo_box_append_text (_fun _GtkWidget _string -> _void))
(define-gtk gtk_combo_box_remove_text (_fun _GtkWidget _int -> _void))
(define-gtk gtk_combo_box_popup (_fun _GtkWidget -> _void))

(define-gtk gtk_widget_queue_draw (_fun _GtkWidget -> _void))

(define-gtk gtk_fixed_set_has_window (_fun _GtkWidget _gboolean -> _void))

(define-gtk gtk_hscrollbar_new (_fun _pointer -> _GtkWidget))
(define-gtk gtk_vscrollbar_new (_fun _pointer -> _GtkWidget))

(define-gtk gtk_widget_set_double_buffered (_fun _GtkWidget _gboolean -> _void))

(define _GtkAdjustment _GtkWidget) ; no, actually a GtkObject
(define-gtk gtk_adjustment_new (_fun _double* _double* _double* _double* _double* _double* -> _GtkAdjustment))
(define-gtk gtk_adjustment_configure (_fun _GtkAdjustment _double* _double* _double* _double* _double* _double* -> _void)
  #:fail (lambda ()
	   ;; This by-hand version doesn't produce quite the same notifications.
	   (lambda (gtk value lower upper step-inc page-inc page-size)
	     (atomically
              (g_object_freeze_notify gtk)
              (g_object_set_double gtk "lower" lower)
              (g_object_set_double gtk "upper" upper)
              (g_object_set_double gtk "step-increment" step-inc)
              (g_object_set_double gtk "page-increment" page-inc)
              (g_object_set_double gtk "page-size" page-size)
              (let ([value (max lower (min value (- upper page-size)))])
                (gtk_adjustment_set_value gtk value))
              (g_object_thaw_notify gtk)))))
(define-gtk gtk_adjustment_get_value (_fun _GtkAdjustment -> _double*))
(define-gtk gtk_adjustment_set_value (_fun _GtkAdjustment _double* -> _void))
(define-gtk gtk_adjustment_get_upper (_fun _GtkAdjustment -> _double*)
  #:fail (lambda ()
	   (lambda (gtk)
	     (g_object_get_double gtk "upper"))))
(define-gtk gtk_adjustment_set_upper (_fun _GtkAdjustment _double* -> _void)
  #:fail (lambda ()
	   (lambda (gtk upper)
	     (g_object_set_double gtk "upper" upper))))
(define-gtk gtk_adjustment_get_page_size (_fun _GtkAdjustment -> _double*)
  #:fail (lambda ()
	   (lambda (gtk)
	     (g_object_get_double gtk "page-size"))))
(define-gtk gtk_adjustment_set_page_size (_fun _GtkAdjustment _double* -> _void)
  #:fail (lambda ()
	   (lambda (gtk page-size)
	     (g_object_set_double gtk "page-size" page-size))))
(define-gtk gtk_adjustment_get_page_increment (_fun _GtkAdjustment -> _double*)
  #:fail (lambda ()
	   (lambda (gtk)
	     (g_object_get_double gtk "page-increment"))))
(define-gtk gtk_adjustment_set_page_increment (_fun _GtkAdjustment _double* -> _void)
  #:fail (lambda ()
	   (lambda (gtk page-inc)
	     (g_object_set_double gtk "page-increment" page-inc))))

(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_bin_get_child (_fun _GtkWidget -> _GtkWidget))

(define-gobj g_object_set_bool (_fun _GtkWidget _string _gboolean [_pointer = #f] -> _void)
  #:c-id g_object_set)

(define _GtkIMContext (_cpointer 'GtkIMContext))
(define-gtk gtk_im_multicontext_new (_fun -> _GtkIMContext))
(define-gtk gtk_im_context_set_use_preedit (_fun _GtkIMContext _gboolean -> _void))
(define-gtk gtk_im_context_focus_in (_fun _GtkIMContext -> _void))
(define-gtk gtk_im_context_focus_out (_fun _GtkIMContext -> _void))
(define-gtk gtk_im_context_filter_keypress (_fun _GtkIMContext _GdkEventKey-pointer -> _gboolean))
(define-gtk gtk_im_context_set_client_window (_fun _GtkIMContext _GdkWindow -> _void))
(define-gtk gtk_im_context_set_cursor_location (_fun _GtkIMContext _GdkRectangle-pointer -> _void))

(define im-string-result #f)
(define im-filtering? #f)
(define im-canvas #f)

(define-signal-handler connect-commit "commit"
  (_fun _GtkIMContext _string -> _void)
  (lambda (im str)
    (cond
     [im-filtering?
      ;; filtering an event => we can handle the string
      ;; result directly
      (set! im-string-result str)]
     [(and im-canvas
	   (weak-box-value im-canvas))
      ;; not filtering, but there's a target canvas =>
      ;; queue a made-up key press event for each character
      ;; of the string
      => (lambda (wx)
	   (for ([c (in-string str)])
	     (let ([e (new key-event%
			   [key-code c]
			   [shift-down #f]
			   [control-down #f]
			   [meta-down #f]
			   [alt-down #f]
			   [x 0]
			   [y 0]
			   [time-stamp 0]
			   [caps-down #f])])
	       (queue-window-event wx (lambda () 
					(send wx dispatch-on-char e #f))))))])))

(define im (gtk_im_multicontext_new))
(void (connect-commit (cast im _pointer _GtkWidget)))

(gtk_im_context_set_use_preedit im #f)

;; We rely some on the implementation of GtkComboBoxEntry to replace
;; the drawing routine.
(define-cstruct _GList ([data _pointer]))
(define-gdk gdk_window_get_children (_fun _pointer -> _GList-pointer/null))
(define-gdk gdk_window_hide (_fun _pointer -> _void))
(define (get-subwindow gtk)
  (let* ([win (widget-window gtk)]
         [subs (gdk_window_get_children win)])
    (if subs
        (GList-data subs)
        win)))

(define-signal-handler connect-changed "changed"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx combo-maybe-clicked)))))

(define-gtk gtk_combo_box_set_active (_fun _GtkWidget _int -> _void))
(define-gtk gtk_combo_box_get_active (_fun _GtkWidget -> _int))
(define-gtk gtk_combo_box_set_button_sensitivity (_fun _GtkWidget _int -> _void)
  #:fail (lambda () 
	   ;; Not available? Too bad. A combo-field% will in rare
	   ;;  cases not be enabled as it should be.
	   (lambda (w m) (void))))
(define GTK_SENSITIVITY_ON 1)

(define-signal-handler connect-expose "expose-event"
  (_fun _GtkWidget _GdkEventExpose-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)])
      (if wx
          (begin
            (unless (send wx paint-or-queue-paint)
              (let ([gc (send wx get-canvas-background-for-clearing)])      
                (when gc
                  (gdk_draw_rectangle (widget-window gtk) gc #t
                                      0 0 32000 32000)
                  (gdk_gc_unref gc))))
            (not (send wx is-panel?)))
          #f))))

(define-signal-handler connect-value-changed-h "value-changed"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (do-value-changed gtk 'horizontal)))

(define-signal-handler connect-value-changed-v "value-changed"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (do-value-changed gtk 'vertical)))

(define-signal-handler connect-unrealize "unrealize"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx unrealize)))))

(define-signal-handler connect-unmap "unmap"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx unrealize)))))

(define (do-value-changed gtk dir)
  (let ([wx (gtk->wx gtk)])
    (when wx
      (when (send wx deliver-scroll-callbacks?)
        (queue-window-event wx (lambda () 
                                 (send wx do-scroll dir)
                                 (flush-display))))))
  #t)

(define canvas%
  (canvas-mixin
   (class (canvas-autoscroll-mixin (client-size-mixin window%))
     (init parent
           x y w h
           style
           [ignored-name #f]
           [gl-config #f])

     (inherit get-gtk get-size get-client-size 
              get-top-win
              set-auto-size 
              adjust-client-delta infer-client-delta
              is-auto-scroll? is-disabled-scroll?
              get-virtual-width get-virtual-height
              refresh-for-autoscroll refresh-all-children
	      reset-auto-scroll
              get-eventspace
	      register-extra-gtk)

     (define is-combo? (memq 'combo style))
     (define has-border? (or (memq 'border style)
                             (memq 'control-border style)))

     (define margin (if has-border? 1 0))

     (define-values (client-gtk container-gtk gtk 
                                hscroll-adj vscroll-adj hscroll-gtk vscroll-gtk resize-box
                                combo-button-gtk
                                scroll-width)
       (atomically ;; need to connect all children to gtk to avoid leaks
        (cond
         [(or (memq 'hscroll style)
              (memq 'auto-hscroll style)
              (memq 'vscroll style)
              (memq 'auto-vscroll style))
          (let* ([client-gtk (if (is-panel?)
                                 (gtk_fixed_new)
                                 (gtk_drawing_area_new))]
                 [container-gtk (if (is-panel?)
                                    (gtk_fixed_new)
                                    client-gtk)]
                 [hadj (gtk_adjustment_new 0.0 0.0 1.0 1.0 1.0 1.0)]
                 [vadj (gtk_adjustment_new 0.0 0.0 1.0 1.0 1.0 1.0)]
                 [hs? (or (memq 'hscroll style)
                          (memq 'auto-hscroll style))]
                 [vs? (or (memq 'vscroll style)
                          (memq 'auto-vscroll style))])
            (let ([h (as-gtk-allocation (gtk_hbox_new #f 0))]
                  [v (gtk_vbox_new #f 0)]
                  [v2 (gtk_vbox_new #f 0)]
                  [h2 (gtk_vbox_new #f 0)]
                  [hscroll (gtk_hscrollbar_new hadj)]
                  [vscroll (gtk_vscrollbar_new vadj)]
                  [resize-box (gtk_drawing_area_new)])
              ;; |------------------------------------|
              ;; | h |-----------------| |-----------||
              ;; |   | v               | | v2        ||
              ;; |   |                 | | [vscroll] ||
              ;; |   | [h2 [hscroll]]  | | [resize]  ||
              ;; |   |-----------------| |-----------|| 
              ;; |------------------------------------|
              (unless (eq? client-gtk container-gtk)
                (gtk_fixed_set_has_window client-gtk #t)) ; imposes clipping
              (when has-border?
                (gtk_container_set_border_width h margin)
                (connect-expose-border h))
              (gtk_box_pack_start h v #t #t 0)
              (gtk_box_pack_start v client-gtk #t #t 0)
              (gtk_box_pack_start h v2 #f #f 0)
              (gtk_box_pack_start v2 vscroll #t #t 0)
              (gtk_box_pack_start v h2 #f #f 0)
              (gtk_box_pack_start h2 hscroll #t #t 0)
              (gtk_box_pack_start v2 resize-box #f #f 0)
              (when hs?
                (gtk_widget_show hscroll))
              (gtk_widget_show vscroll)
              (gtk_widget_show h)
              (gtk_widget_show v)
              (when vs?
                (gtk_widget_show v2))
              (gtk_widget_show h2)
              (when hs?
                (gtk_widget_show resize-box))
              (gtk_widget_show client-gtk)
              (unless (eq? client-gtk container-gtk)
                (gtk_container_add client-gtk container-gtk)
                (gtk_widget_show container-gtk))
              (let ([req (make-GtkRequisition 0 0)])
                (gtk_widget_size_request vscroll req)
                (values client-gtk container-gtk h hadj vadj 
                        (and hs? h2)
                        (and vs? v2)
                        (and hs? vs? resize-box)
                        #f
                        (GtkRequisition-width req)))))]
         [is-combo?
          (let* ([gtk (as-gtk-allocation (gtk_combo_box_entry_new_text))]
                 [orig-entry (gtk_bin_get_child gtk)])
	    (gtk_combo_box_set_button_sensitivity gtk GTK_SENSITIVITY_ON)
            (values orig-entry gtk gtk #f #f #f #f #f (extract-combo-button gtk) 0))]
         [has-border?
          (let ([client-gtk (gtk_drawing_area_new)]
                [h (as-gtk-allocation (gtk_hbox_new #f 0))])
            (gtk_box_pack_start h client-gtk #t #t 0)
            (gtk_container_set_border_width h margin)
            (connect-expose-border h)
            (gtk_widget_show client-gtk)
            (values client-gtk client-gtk h #f #f #f #f #f #f 0))]
         [else
          (let ([client-gtk (as-gtk-allocation (gtk_drawing_area_new))])
            (values client-gtk client-gtk client-gtk #f #f #f #f #f #f 0))])))

     (define for-gl? (memq 'gl style))
     (when for-gl?
       (prepare-widget-gl-context client-gtk gl-config)
       (gtk_widget_set_double_buffered client-gtk #f))

     (define dc #f)

     (super-new [parent parent]
                [gtk gtk]
                [client-gtk client-gtk]
                [no-show? (memq 'deleted style)]
                [extra-gtks (append
                             (if (eq? client-gtk container-gtk)
                                 null
                                 (list container-gtk))
                             (if (eq? client-gtk gtk)
                                 null
                                 (if hscroll-adj
                                     (list client-gtk hscroll-adj vscroll-adj)
                                     (if combo-button-gtk
                                         (list client-gtk combo-button-gtk)
                                         (list client-gtk)))))])

     (define/private (check-combo)
       (when is-combo?
	 (set! combo-button-gtk (re-extract-combo-button gtk combo-button-gtk this))))

     (set-size x y w h)

     (define/override (set-size x y w h)
       (super set-size x y w h)
       (when (and (is-auto-scroll?)
                  (not (is-panel?)))
         (reset-auto-scroll))
       (on-size))
     
     (set! dc (new dc% [canvas this] [transparent? (memq 'transparent style)]))

     (gtk_widget_realize gtk)
     (gtk_widget_realize client-gtk)

     (when resize-box 
       (let ([r (make-GtkRequisition 0 0)])
         (gtk_widget_size_request hscroll-gtk r)
         (gtk_widget_set_size_request resize-box 
                                      (GtkRequisition-height r)
                                      (GtkRequisition-height r))))

     (connect-expose client-gtk)
     #;(gtk_widget_set_double_buffered client-gtk #f)
     (connect-key-and-mouse client-gtk)
     (connect-focus client-gtk)
     (gtk_widget_add_events client-gtk (bitwise-ior GDK_KEY_PRESS_MASK
                                                    GDK_KEY_RELEASE_MASK
                                                    GDK_BUTTON_PRESS_MASK
                                                    GDK_BUTTON_RELEASE_MASK
                                                    GDK_POINTER_MOTION_MASK
                                                    GDK_FOCUS_CHANGE_MASK
                                                    GDK_ENTER_NOTIFY_MASK
                                                    GDK_LEAVE_NOTIFY_MASK))
     (unless (or (memq 'no-focus style)
                 (is-panel?))
       (set-gtk-object-flags! client-gtk (bitwise-ior (get-gtk-object-flags client-gtk)
                                                      GTK_CAN_FOCUS)))
     (check-combo)
     (when combo-button-gtk
       (connect-combo-key-and-mouse combo-button-gtk))
     (connect-unrealize client-gtk)
     (connect-unmap client-gtk)

     (when hscroll-adj (connect-value-changed-h hscroll-adj))
     (when vscroll-adj (connect-value-changed-v vscroll-adj))

     (set-auto-size)
     (adjust-client-delta (+ (* 2 margin) 
                             (if (or (memq 'vscroll style)
                                     (memq 'auto-vscroll style))
                                 scroll-width
                                 0))
                          (+ (* 2 margin) 
                             (if (or (memq 'hscroll style)
                                     (memq 'auto-hscroll style))
                                 scroll-width
                                 0)))

     (define/public (is-panel?) #f)

     ;; Direct update is ok for a canvas, and it
     ;; allows pushing updates to the screen even
     ;; if the eventspace thread is busy indefinitely
     (define/override (direct-update?) #t)

     (define/public (get-dc) dc)

     (define/public (make-compatible-bitmap w h)
       (send dc make-backing-bitmap w h))

     (define/override (get-client-gtk) client-gtk)
     (define/override (get-container-gtk) container-gtk)
     (define/override (handles-events? gtk) (not (ptr-equal? gtk combo-button-gtk)))

     (define/override (internal-pre-on-event gtk e)
       (if (and (ptr-equal? gtk combo-button-gtk)
                (send e button-down?))
           (begin
             (on-popup)
             #t)
           #f))
     (define/public (popup-combo)
       ;; Unfortunately, the user has to hold the mouse 
       ;; button down when popping up the menu this way,
       ;; whereas the default handler (that we subvert in
       ;; `internal-pre-on-event') keeps the menu open if
       ;; the user release the mouse button right away.
       (gtk_combo_box_popup gtk))

     (define/override (get-client-delta)
       (values margin margin))

     ;; The `queue-paint' and `paint-children' methods
     ;; are defined by `canvas-mixin' from ../common/canvas-mixin
     (define/public (queue-paint) (void))
     (define/public (request-canvas-flush-delay)
       (request-flush-delay (get-flush-window)))
     (define/public (cancel-canvas-flush-delay req)
       (cancel-flush-delay req))
     (define/public (queue-canvas-refresh-event thunk)
       (queue-window-refresh-event this thunk))
     (define/public (skip-pre-paint?) #f)

     (define/public (paint-or-queue-paint)
       ;; in atomic mode
       (if for-gl?
	   (queue-paint)
	   (or (do-canvas-backing-flush #f)
	       (begin
		 (queue-paint)
		 #f))))

     ;; overridden to extend for scheduled periodic flushes:
     (define/public (schedule-periodic-backing-flush)
       (void))
     (define/public (do-canvas-backing-flush ctx)
       (do-backing-flush this dc (if is-combo?
                                     (get-subwindow client-gtk)
                                     (widget-window client-gtk))))

     (define/public (on-paint) (void))

     (define flush-win-box (mcons #f 0))
     (define/public (get-flush-window) 
       (atomically
	(if (zero? (bitwise-and (get-gtk-object-flags client-gtk) 
				GTK_MAPPED))
	    (mcons #f #f)
	    (if (win-box-valid? flush-win-box)
		flush-win-box
		(begin
		  (set! flush-win-box (window->win-box (widget-window client-gtk)))
		  flush-win-box)))))
     (define/public (unrealize)
       (unrealize-win-box flush-win-box))

     (define/public (begin-refresh-sequence)
       (send dc suspend-flush))
     (define/public (end-refresh-sequence)
       (send dc resume-flush))

     ;; The `flush' method should be improved to flush local
     ;; to the enclosing frame, instead of flushing globally.
     (define/public (flush)
       (flush-display))

     (define/private (refresh-one)
       (queue-paint))
     (define/override (refresh)
       (refresh-one)
       (refresh-all-children))

     (define/public (queue-backing-flush)
       ;; called atomically
       (unless for-gl?
         (gtk_widget_queue_draw client-gtk)))
     
     (define/override (reset-child-dcs)
       (when (dc . is-a? . dc%)
         (reset-dc)))

     (send dc start-backing-retained)

     (define/private (reset-dc)
       (send dc reset-backing-retained)
       (refresh-one)
       (send dc set-auto-scroll
             (if (get-virtual-width)
                 (gtk_adjustment_get_value hscroll-adj)
                 0)
             (if (get-virtual-height)
                 (gtk_adjustment_get_value vscroll-adj)
                 0)))

     (define/override (internal-on-client-size w h)
       (reset-dc))
     (define/override (on-client-size w h) 
       (on-size))

     ;; this `on-size' method is for `editor-canvas%', only:
     (define/public (on-size) (void))

     (define/public (show-scrollbars h? v?)
       (when hscroll-gtk
         (if h?
             (gtk_widget_show hscroll-gtk)
             (gtk_widget_hide hscroll-gtk)))
       (when vscroll-gtk
         (if v?
             (gtk_widget_show vscroll-gtk)
             (gtk_widget_hide vscroll-gtk)))
       (when (and hscroll-gtk vscroll-gtk)
         (cond
          [(and v? h?)
           (gtk_widget_show resize-box)]
          [(and v? (not h?))
           ;; remove corner 
           (gtk_widget_hide resize-box)]))
       (adjust-client-delta (+ (* 2 margin) (if v? scroll-width 0))
                            (+ (* 2 margin) (if h? scroll-width 0))))

     (define suspend-scroll-callbacks? #f)
     (define/public (deliver-scroll-callbacks?) (not suspend-scroll-callbacks?))
     (define/private (as-scroll-change thunk)
       (atomically
        (set! suspend-scroll-callbacks? #t)
        (begin0
         (thunk)
         (set! suspend-scroll-callbacks? #f))))


     (define/private (configure-adj adj scroll-gtk len page pos)
       (when (and scroll-gtk adj)
         (as-scroll-change
          (lambda ()
            (if (zero? len)
                (gtk_adjustment_configure adj 0 0 1 1 1 1)
                (let ([pos (if (= pos -1)
                               (gtk_adjustment_get_value adj)
                               pos)])
                  (gtk_adjustment_configure adj pos 0 (+ len page) 1 page page)))))))

     (define/override (do-set-scrollbars h-step v-step
                                         h-len v-len
                                         h-page v-page
                                         h-pos v-pos)
       (configure-adj hscroll-adj hscroll-gtk h-len h-page h-pos)
       (configure-adj vscroll-adj vscroll-gtk v-len v-page v-pos))

     (define/override (reset-dc-for-autoscroll)
       (reset-dc))

     (define/private (dispatch which proc [default (void)])
       (if (eq? which 'vertical)
           (if vscroll-adj (proc vscroll-adj) default)
           (if hscroll-adj (proc hscroll-adj) default)))

     (define/public (set-scroll-page which v)
       (dispatch which (lambda (adj)
                         (let ([old (gtk_adjustment_get_page_size adj)])
                           (unless (= old v)
                             (as-scroll-change
                              (lambda ()
                                (gtk_adjustment_set_page_size adj v)
                                (gtk_adjustment_set_page_increment adj v)
                                (gtk_adjustment_set_upper adj (+ (- v old)
                                                                 (gtk_adjustment_get_upper adj))))))))))
     (define/public (set-scroll-range which v)
       (dispatch which (lambda (adj)
                         (as-scroll-change
                          (lambda ()
                            (gtk_adjustment_set_upper adj (+ v (gtk_adjustment_get_page_size adj))))))))
     (define/public (set-scroll-pos which v)
       (dispatch which (lambda (adj) 
                         (as-scroll-change
                          (lambda ()
                            (gtk_adjustment_set_value adj v))))))

     (define/private (is-disabled-scroll-dir? which)
       (or (if (eq? which 'vertical)
               (not vscroll-gtk)
               (not hscroll-gtk))
           (is-disabled-scroll?)))

     (define/public (get-scroll-page which) 
       (if (or (is-disabled-scroll-dir? which) (is-auto-scroll?))
           0
           (->long (dispatch which gtk_adjustment_get_page_size 0))))
     (define/public (get-scroll-range which)
       (if (or (is-disabled-scroll-dir? which) (is-auto-scroll?))
           0
           (->long (dispatch which (lambda (adj)
                                     (- (gtk_adjustment_get_upper adj)
                                        (gtk_adjustment_get_page_size adj)))
                             0))))
     (define/public (get-scroll-pos which)
       (if (or (is-disabled-scroll-dir? which) (is-auto-scroll?))
           0
           (->long (dispatch which gtk_adjustment_get_value 0))))
     
     (define clear-bg?
       (and (not (memq 'transparent style)) 
            (not (memq 'no-autoclear style))))
     (define transparent?
       (memq 'transparent style))
     (define gc #f)
     (define bg-col (make-object color% "white"))
     (define/public (get-canvas-background) (if transparent?
                                                #f
                                                bg-col))
     (define/public (set-canvas-background col) (set! bg-col col))
     (define/public (get-canvas-background-for-backing) (and clear-bg? bg-col))
     (define/public (get-canvas-background-for-clearing) 
       ;; called in event-dispatch mode
       (if clear-bg?
           (let* ([conv (lambda (x) (bitwise-ior x (arithmetic-shift x 8)))]
                  [w (widget-window gtk)]
                  [gc (gdk_gc_new w)])
             (gdk_gc_set_rgb_fg_color gc (make-GdkColor 0
                                                        (conv (color-red bg-col))
                                                        (conv (color-green bg-col))
                                                        (conv (color-blue bg-col))))
             gc)
           #f))

     (when is-combo?
       (connect-changed client-gtk))

     (define combo-count 0)
     (define/public (clear-combo-items)
       (atomically
        (for ([n (in-range combo-count)])
          (gtk_combo_box_remove_text gtk 0))
        (set! combo-count 0)))
     (define/public (append-combo-item str)
       (atomically
        (set! combo-count (add1 combo-count))
        (gtk_combo_box_append_text gtk str)))

     (when is-combo? (append-combo-item "..."))

     (define/public (on-popup) (void))

     (define/public (combo-maybe-clicked)
       (let ([i (gtk_combo_box_get_active gtk)])
         (when (i . > . -1)
	   (set-no-active-item)
           (queue-window-event this (lambda () (on-combo-select i))))))
     (define/public (on-combo-select i) (void))

     (define/private (set-no-active-item)
       ;; (gtk_combo_box_set_active gtk -1) should work,
       ;; or using (.._iter #f) should work, but neither
       ;; causes the "changed" signal to be emitted when the
       ;; currently active item is re-selected, so we
       ;; hack around the problem by adding an item, making
       ;; it active, then removing it
       (atomically
	(gtk_combo_box_append_text gtk "dummy")
	(gtk_combo_box_set_active gtk combo-count)
	(gtk_combo_box_remove_text gtk combo-count)))

     (define/public (set-combo-text t) (void))

     (define/override (focus-change on?)
       ;; input-method management
       (if on?
	   (begin
	     (set! im-canvas (make-weak-box this))
	     (gtk_im_context_focus_in im)
	     (gtk_im_context_set_client_window im (widget-window client-gtk))
	     (let ([w (box 0)]
		   [h (box 0)])
	       (get-client-size w h)
	       (gtk_im_context_set_cursor_location 
		im 
		(make-GdkRectangle 0 0 (unbox w) (unbox h)))))
	   (when (and im-canvas
		      (eq? this (weak-box-value im-canvas)))
	     (gtk_im_context_focus_out im)
	     (set! im-canvas #f))))

     (define/override (filter-key-event e)
       ;; give the input method a chance to handle the
       ;; key event; see call in "window.rkt" for
       ;; information on the results
       (if (and im-canvas
		(eq? this (weak-box-value im-canvas)))
	   (begin
	     (set! im-filtering? #t)
	     (set! im-string-result #f)
	     (if (begin0
		  (gtk_im_context_filter_keypress im e)
		  (set! im-filtering? #f))
		 im-string-result
		 'none))
	   'none))
     
     (define/public (do-scroll direction)
       (if (is-auto-scroll?)
           (refresh-for-autoscroll)
           (on-scroll (new scroll-event%
                           [event-type 'thumb]
                           [direction direction]
                           [position (get-scroll-pos direction)]))))
     (define/public (on-scroll e) (void))

     (define/public (scroll x y)
       (when (is-auto-scroll?) 
         (as-scroll-change
          (lambda ()
            (when (and hscroll-adj (>= x 0))
              (gtk_adjustment_set_value 
               hscroll-adj 
               (floor
                (* x (- (gtk_adjustment_get_upper hscroll-adj)
                        (gtk_adjustment_get_page_size hscroll-adj))))))
            (when (and vscroll-adj  (>= y 0))
              (gtk_adjustment_set_value 
               vscroll-adj
               (floor
                (* y (- (gtk_adjustment_get_upper vscroll-adj)
                        (gtk_adjustment_get_page_size vscroll-adj))))))))
         (refresh-for-autoscroll)))

     (define/override (get-virtual-h-pos)
       (inexact->exact (ceiling (gtk_adjustment_get_value hscroll-adj))))
     (define/override (get-virtual-v-pos)
       (inexact->exact (ceiling (gtk_adjustment_get_value vscroll-adj))))

     (define/public (set-resize-corner on?) (void))

     (define reg-blits null)
     
     (define/private (register-one-blit x y w h on-pixbuf off-pixbuf)
       (let* ([cwin (widget-window client-gtk)])
         (atomically
          (let ([win (create-gc-window cwin x y w h)])
            (let ([r (scheme_add_gc_callback
                      (make-gc-show-desc win on-pixbuf w h)
                      (make-gc-hide-desc win off-pixbuf w h))])
              (cons win r))))))
     
     (define/public (register-collecting-blit x y w h on off on-x on-y off-x off-y)
       (let ([on (fix-bitmap-size on w h on-x on-y)]
             [off (fix-bitmap-size off w h off-x off-y)])
         (let ([on-pixbuf (bitmap->pixbuf on)]
               [off-pixbuf (bitmap->pixbuf off)])
           (atomically
            (set! reg-blits (cons (register-one-blit x y w h on-pixbuf off-pixbuf) reg-blits))))))
     
     (define/public (unregister-collecting-blits)
       (atomically
        (for ([r (in-list reg-blits)])
          (g_object_unref (car r))
          (scheme_remove_gc_callback (cdr r)))
        (set! reg-blits null))))))

;; ----------------------------------------

(define canvas-panel%
  (class (panel-container-mixin (panel-mixin canvas%))
    (inherit get-container-gtk
             get-client-gtk
             get-virtual-h-pos
             get-virtual-v-pos)
    (define/override (is-panel?) #t)

    (define/override (set-child-size child-gtk x y w h)
      ;; ensure that container is big enough to hold the child:
      (let ([container-gtk (get-container-gtk)]
            [req (make-GtkRequisition 0 0)])
        (gtk_widget_size_request container-gtk req)
        (gtk_widget_set_size_request container-gtk
                                     (max (GtkRequisition-width req)
                                          (+ x w))
                                     (max (GtkRequisition-height req)
                                          (+ y h))))
      (super set-child-size child-gtk x y w h))

    (define/override (reset-dc-for-autoscroll)
      (super reset-dc-for-autoscroll)
      (gtk_fixed_move (get-client-gtk) (get-container-gtk) 
                      (- (get-virtual-h-pos))
                      (- (get-virtual-v-pos))))

    (super-new)))
