#lang racket/base
(require ffi/unsafe
         racket/class
         ffi/unsafe/atomic
         "../../syntax.rkt"
         "../../lock.rkt"
         "../common/event.rkt"
         "../common/freeze.rkt"
         "../common/queue.rkt"
         "../common/local.rkt"
         "keycode.rkt"
         "queue.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "widget.rkt")

(provide window%
         gtk->wx
         queue-window-event

         gtk_widget_show
         gtk_widget_hide
         gtk_widget_realize 
         gtk_container_add
         gtk_widget_add_events
         gtk_widget_size_request
         gtk_widget_set_size_request
         gtk_widget_grab_focus
         gtk_widget_set_sensitive

         connect-focus
         connect-key-and-mouse
         do-button-event

         (struct-out GtkRequisition) _GtkRequisition-pointer
         (struct-out GtkAllocation) _GtkAllocation-pointer

	 widget-window

         the-accelerator-group
         gtk_window_add_accel_group
         gtk_menu_set_accel_group)

;; ----------------------------------------

(define-gtk gtk_container_add (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_widget_realize (_fun _GtkWidget -> _void))
(define-gtk gtk_widget_add_events (_fun _GtkWidget _int -> _void))

(define-gdk gdk_keyval_to_unicode (_fun _uint -> _uint32))

(define-cstruct _GtkRequisition ([width _int]
                                 [height _int]))
(define-cstruct _GtkAllocation ([x _int]
                                [y _int]
                                [width _int]
                                [height _int]))

(define _GdkEventFocus-pointer _pointer)

(define-gtk gtk_widget_size_request (_fun _GtkWidget _GtkRequisition-pointer -> _void))
(define-gtk gtk_widget_size_allocate (_fun _GtkWidget _GtkAllocation-pointer -> _void))
(define-gtk gtk_widget_set_size_request (_fun _GtkWidget _int _int -> _void))
(define-gtk gtk_widget_grab_focus (_fun _GtkWidget -> _void))
(define-gtk gtk_widget_is_focus (_fun _GtkWidget -> _gboolean))
(define-gtk gtk_widget_set_sensitive (_fun _GtkWidget _gboolean -> _void))

(define _GtkAccelGroup (_cpointer 'GtkAccelGroup))
(define-gtk gtk_accel_group_new (_fun -> _GtkAccelGroup))
(define-gtk gtk_window_add_accel_group (_fun _GtkWindow _GtkAccelGroup -> _void))
(define-gtk gtk_menu_set_accel_group (_fun _GtkWidget _GtkAccelGroup -> _void))

(define the-accelerator-group (gtk_accel_group_new))

(define-cstruct _GtkWidgetT ([obj _GtkObject]
			     [private_flags _uint16]
			     [state _byte]
			     [saved_state _byte]
			     [name _pointer]
			     [style _pointer]
			     [req _GtkRequisition]
			     [alloc _GtkAllocation]
			     [window _GdkWindow]
			     [parent _GtkWidget]))

(define (widget-window gtk)
  (GtkWidgetT-window (cast gtk _GtkWidget _GtkWidgetT-pointer)))

;; ----------------------------------------

(define-signal-handler connect-focus-in "focus-in-event"
  (_fun _GtkWidget _GdkEventFocus-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (queue-window-event wx (lambda () (send wx on-set-focus))))
      #f)))
(define-signal-handler connect-focus-out "focus-out-event"
  (_fun _GtkWidget _GdkEventFocus-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (queue-window-event wx (lambda () (send wx on-kill-focus))))
      #f)))
(define (connect-focus gtk)
  (connect-focus-in gtk)
  (connect-focus-out gtk))

(define-signal-handler connect-size-allocate "size-allocate"
  (_fun _GtkWidget _GtkAllocation-pointer -> _gboolean)
  (lambda (gtk a)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx save-size 
              (GtkAllocation-x a)
              (GtkAllocation-y a)
              (GtkAllocation-width a)
              (GtkAllocation-height a))))
    #t))
;; ----------------------------------------

(define-signal-handler connect-key-press "key-press-event"
  (_fun _GtkWidget _GdkEventKey-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)])
      (and
       wx
       (let* ([modifiers (GdkEventKey-state event)]
              [bit? (lambda (m v) (positive? (bitwise-and m v)))]
              [k (new key-event%
                      [key-code (let ([kv (GdkEventKey-keyval event)])
                                  (or
                                   (map-key-code kv)
                                   (integer->char (gdk_keyval_to_unicode kv))))]
                      [shift-down (bit? modifiers GDK_SHIFT_MASK)]
                      [control-down (bit? modifiers GDK_CONTROL_MASK)]
                      [meta-down (bit? modifiers GDK_META_MASK)]
                      [alt-down (bit? modifiers GDK_MOD1_MASK)]
                      [x 0]
                      [y 0]
                      [time-stamp (GdkEventKey-time event)]
                      [caps-down (bit? modifiers GDK_LOCK_MASK)])])
         (if (send wx handles-events? gtk)
             (begin
               (queue-window-event wx (lambda () (send wx dispatch-on-char k #f)))
               #t)
             (constrained-reply (send wx get-eventspace)
                                (lambda () (send wx dispatch-on-char k #t))
                                #t)))))))

(define-signal-handler connect-button-press "button-press-event"
  (_fun _GtkWidget _GdkEventButton-pointer -> _gboolean)
  (lambda (gtk event)
    (unless (gtk_widget_is_focus gtk)
      (let ([wx (gtk->wx gtk)])
        (when wx
          (unless (other-modal? wx)
            (gtk_widget_grab_focus gtk)))))
    (do-button-event gtk event #f #f)))

(define-signal-handler connect-button-release "button-release-event"
  (_fun _GtkWidget _GdkEventButton-pointer -> _gboolean)
  (lambda (gtk event)
    (do-button-event gtk event #f #f)))

(define-signal-handler connect-pointer-motion "motion-notify-event"
  (_fun _GtkWidget _GdkEventMotion-pointer -> _gboolean)
  (lambda (gtk event)
    (do-button-event gtk event #t #f)))

(define-signal-handler connect-enter "enter-notify-event"
  (_fun _GtkWidget _GdkEventCrossing-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)]) (when wx (send wx enter-window)))
    (do-button-event gtk event #f #t)))

(define-signal-handler connect-leave "leave-notify-event"
  (_fun _GtkWidget _GdkEventCrossing-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)]) (when wx (send wx leave-window)))
    (do-button-event gtk event #f #t)))

(define (connect-key-and-mouse gtk [skip-press? #f])
  (connect-key-press gtk)
  (connect-button-press gtk)
  (unless skip-press? (connect-button-release gtk))
  (connect-pointer-motion gtk)
  (connect-enter gtk)
  (connect-leave gtk))

(define (do-button-event gtk event motion? crossing?)
  (let ([type (if motion?
                  GDK_MOTION_NOTIFY
                  (if crossing?
                      (GdkEventCrossing-type event)
                      (GdkEventButton-type event)))])
    (unless (or (= type GDK_2BUTTON_PRESS)
                (= type GDK_3BUTTON_PRESS))
      (let ([wx (gtk->wx gtk)])
        (and
         wx
         (let* ([modifiers (if motion?
                               (GdkEventMotion-state event)
                               (if crossing?
                                   (GdkEventCrossing-state event)
                                   (GdkEventButton-state event)))]
                [bit? (lambda (m v) (positive? (bitwise-and m v)))]
                [type (cond
                       [(= type GDK_MOTION_NOTIFY)
                        'motion]
                       [(= type GDK_ENTER_NOTIFY)
                        'enter]
                       [(= type GDK_LEAVE_NOTIFY)
                        'leave]
                       [(= type GDK_BUTTON_PRESS)
                        (case (GdkEventButton-button event)
                          [(1) 'left-down]
                          [(3) 'right-down]
                          [else 'middle-down])]
                       [else
                        (case (GdkEventButton-button event)
                          [(1) 'left-up]
                          [(3) 'right-up]
                          [else 'middle-up])])]
                [m (new mouse-event%
                        [event-type type]
                        [left-down (case type
                                     [(left-down) #t]
                                     [(left-up) #f]
                                     [else (bit? modifiers GDK_BUTTON1_MASK)])]
                        [middle-down (case type
                                       [(middle-down) #t]
                                       [(middle-up) #f]
                                       [else (bit? modifiers GDK_BUTTON2_MASK)])]
                        [right-down (case type
                                      [(right-down) #t]
                                      [(right-up) #f]
                                      [else (bit? modifiers GDK_BUTTON3_MASK)])]
                        [x (->long ((if motion? 
                                        GdkEventMotion-x 
                                        (if crossing? GdkEventCrossing-x GdkEventButton-x))
                                    event))]
                        [y (->long ((if motion? GdkEventMotion-y 
                                        (if crossing? GdkEventCrossing-y GdkEventButton-y))
                                    event))]
                        [shift-down (bit? modifiers GDK_SHIFT_MASK)]
                        [control-down (bit? modifiers GDK_CONTROL_MASK)]
                        [meta-down (bit? modifiers GDK_META_MASK)]
                        [alt-down (bit? modifiers GDK_MOD1_MASK)]
                        [time-stamp ((if motion? GdkEventMotion-time 
                                         (if crossing? GdkEventCrossing-time GdkEventButton-time))
                                     event)]
                        [caps-down (bit? modifiers GDK_LOCK_MASK)])])
           (if (send wx handles-events? gtk)
               (begin
                 (queue-window-event wx (lambda ()
                                          (send wx dispatch-on-event m #f)))
                 #t)
               (constrained-reply (send wx get-eventspace)
                                  (lambda () (send wx dispatch-on-event m #t))
                                  #t))))))))

;; ----------------------------------------

(define (internal-error str)
  (log-error
   (apply string-append
          (format "internal error: ~a" str)
          (append
           (for/list ([c (continuation-mark-set->context (current-continuation-marks))])
             (let ([name (car c)]
                   [loc (cdr c)])
               (cond
                [loc
                 (string-append
                  "\n"
                  (cond 
                   [(srcloc-line loc)
                    (format "~a:~a:~a" 
                            (srcloc-source loc)
                            (srcloc-line loc)
                            (srcloc-column loc))]
                   [else
                    (format "~a::~a" 
                            (srcloc-source loc)
                            (srcloc-position loc))])
                  (if name (format " ~a" name) ""))]
                [else (format "\n ~a" name)])))
           '("\n")))))

(define window%
  (class widget%
    (init-field parent
                gtk)
    (init [no-show? #f]
          [extra-gtks null]
          [add-to-parent? #t])

    (super-new [gtk gtk]
               [extra-gtks extra-gtks]
               [parent parent])

    (define save-x (get-unset-pos))
    (define save-y (get-unset-pos))
    (define save-w 0)
    (define save-h 0)

    (define/public (get-unset-pos) 0)

    (connect-size-allocate gtk)

    (when add-to-parent?
      (gtk_container_add (send parent get-client-gtk) gtk))

    (define/public (get-gtk) gtk)
    (define/public (get-client-gtk) gtk)
    (define/public (get-window-gtk) (send parent get-window-gtk))

    (define/public (move x y)
      (set-size x y -1 -1))

    (define/public (set-size x y w h)
      (unless (and (or (= x -11111) (= save-x x))
                   (or (= y -11111) (= save-y y))
                   (or (= w -1) (= save-w w))
                   (or (= h -1) (= save-h h)))
        (unless (= x -11111) (set! save-x x))
        (unless (= y -11111) (set! save-y y))
        (unless (= w -1) (set! save-w w))
        (unless (= h -1) (set! save-h h))
        (set! save-w (max save-w client-delta-w))
        (set! save-h (max save-h client-delta-h))
        (tentative-client-size (+ save-w client-delta-w)
                               (+ save-h client-delta-h))
        (really-set-size gtk save-x save-y save-w save-h)))

    (define/public (save-size x y w h)
      (set! save-w w)
      (set! save-h h))

    (define/public (really-set-size gtk x y w h)
      (send parent set-child-size gtk x y w h))

    (define/public (set-child-size child-gtk x y w h)
      (gtk_widget_set_size_request child-gtk w h)
      (gtk_widget_size_allocate child-gtk (make-GtkAllocation x y w h)))

    (define on-size-queued? #f)
    (define/public (remember-size w h)
      ;; called in event-pump thread
      (unless (and (= save-w w)
                   (= save-h h))
        (set! save-w w)
        (set! save-h h)
        (unless on-size-queued?
          (set! on-size-queued? #t)
          (queue-window-event this (lambda () 
                                     (set! on-size-queued? #f)
                                     (on-size w h))))))

    (define client-delta-w 0)
    (define client-delta-h 0)
    (define min-client-delta-w 0)
    (define min-client-delta-h 0)
    (define/public (remember-client-size w h)
      ;; Called in the Gtk event-loop thread
      ;(set! client-delta-w (max min-client-delta-w (- save-w w)))
      ;(set! client-delta-h (max min-client-delta-h (- save-h h)))
      #;(queue-window-event this (lambda () (on-size 0 0)))
      (void))
    (define/public (tentative-client-size w h)
      (void))

    (define/public (adjust-client-delta dw dh)
      (set! client-delta-w dw)
      (set! client-delta-h dh))

    (define/public (infer-client-delta [w? #t] [h? #t] [sub-h-gtk #f])
      (let ([req (make-GtkRequisition 0 0)]
            [creq (make-GtkRequisition 0 0)]
            [hreq (make-GtkRequisition 0 0)])
        (gtk_widget_size_request gtk req)
        (gtk_widget_size_request (get-client-gtk) creq)
        (when sub-h-gtk
          (gtk_widget_size_request sub-h-gtk hreq))
        (when w?
          (set! client-delta-w (- (GtkRequisition-width req)
                                  (max (GtkRequisition-width creq)
                                       (GtkRequisition-width hreq)))))
        (when h?
          (set! client-delta-h (- (GtkRequisition-height req)
                                  (GtkRequisition-height creq))))))

    (define/public (set-auto-size)
      (let ([req (make-GtkRequisition 0 0)])
        (gtk_widget_size_request gtk req)
        (set-size -11111
                  -11111
                  (GtkRequisition-width req)
                  (GtkRequisition-height req))))

    (define shown? #f)
    (define/public (direct-show on?)
      (atomically
       (if on?
           (gtk_widget_show gtk)
           (gtk_widget_hide gtk))
       (set! shown? (and on? #t))
       (register-child-in-parent on?))
      (when on? (reset-child-dcs)))
    (define/public (show on?)
      (direct-show on?))
    (define/public (reset-child-dcs) (void))
    (define/public (is-shown?) shown?)
    (define/public (is-shown-to-root?)
      (and shown?
           (if parent
               (send parent is-shown-to-root?)
               #t)))

    (unless no-show? (show #t))

    (define/public (get-x) (if (= save-x -11111) 0 save-x))
    (define/public (get-y) (if (= save-y -11111) 0 save-y))
    (define/public (get-width) save-w)
    (define/public (get-height) save-h)

    (define/public (get-parent) parent)

    (define/public (get-top-win) (send parent get-top-win))

    (define/public (get-dialog-level) (send parent get-dialog-level))

    (define/public (get-size xb yb)
      (set-box! xb save-w)
      (set-box! yb save-h))
    (define/public (get-client-size xb yb)
      (get-size xb yb)
      (set-box! xb (max 0 (- (unbox xb) client-delta-w)))
      (set-box! yb (max 0 (- (unbox yb) client-delta-h))))

    (define enabled? #t)
    (define/pubment (is-enabled-to-root?)
      (and enabled?
           (inner (send parent is-enabled-to-root?)
                  is-enabled-to-root?)))
    (define/public (enable on?)
      (set! enabled? on?)
      (gtk_widget_set_sensitive gtk on?))
    (define/public (is-window-enabled?) enabled?)

    (define/public (drag-accept-files on?) (void))

    (define/public (set-focus)
      (gtk_widget_grab_focus (get-client-gtk)))

    (define cursor-handle #f)
    (define/public (set-cursor v)
      (set! cursor-handle (and v
                               (send (send v get-driver) get-handle)))
      (check-window-cursor this))
    (define/public (enter-window)
      (set-window-cursor this #f))
    (define/public (leave-window)
      (when parent
        (send parent enter-window)))
    (define/public (set-window-cursor in-win c)
      (set-parent-window-cursor in-win (or c cursor-handle)))
    (define/public (set-parent-window-cursor in-win c)
      (when parent
        (send parent set-window-cursor in-win c)))
    (define/public (check-window-cursor win)
      (when parent
        (send parent check-window-cursor win)))

    (define/public (on-set-focus) (void))
    (define/public (on-kill-focus) (void))

    (define/public (handles-events? gtk) #f)
    (define/public (dispatch-on-char e just-pre?) 
      (cond
       [(other-modal? this) #t]
       [(call-pre-on-char this e) #t]
       [just-pre? #f]
       [else (when enabled? (on-char e)) #t]))
    (define/public (dispatch-on-event e just-pre?) 
      (cond
       [(other-modal? this) #t]
       [(call-pre-on-event this e) #t]
       [just-pre? #f]
       [else (when enabled? (on-event e)) #t]))

    (define/public (call-pre-on-event w e)
      (or (send parent call-pre-on-event w e)
          (pre-on-event w e)))
    (define/public (call-pre-on-char w e)
      (or (send parent call-pre-on-char w e)
          (pre-on-char w e)))
    (define/public (pre-on-event w e) #f)
    (define/public (pre-on-char w e) #f)

    (define/public (on-char e) (void))
    (define/public (on-event e) (void))

    (define/public (on-size w h) (void))

    (define/public (register-child child on?)
      (void))
    (define/public (register-child-in-parent on?)
      (when parent
        (send parent register-child this on?)))

    (def/public-unimplemented on-drop-file)
    (def/public-unimplemented get-handle)
    (def/public-unimplemented set-phantom-size)

    (define/public (popup-menu m x y)
      (let ([gx (box x)]
            [gy (box y)])
        (client-to-screen gx gy)
        (send m popup (unbox gx) (unbox gy)
              (lambda (thunk) (queue-window-event this thunk)))))

    (define/public (center a b) (void))
    (define/public (refresh) (void))

    (define/public (screen-to-client x y)
      (let ([xb (box 0)]
            [yb (box 0)])
        (client-to-screen xb yb)
        (set-box! x (- (unbox x) (unbox xb)))
        (set-box! y (- (unbox y) (unbox yb)))))
    (define/public (client-to-screen x y)
      (let-values ([(dx dy) (get-client-delta)])
        (send parent client-to-screen x y)
        (set-box! x (+ (unbox x) save-x dx))
        (set-box! y (+ (unbox y) save-y dy))))

    (define/public (get-client-delta)
      (values 0 0))

    (def/public-unimplemented get-position)
    (def/public-unimplemented fit)

    (define/public (gets-focus?) #t)

    (def/public-unimplemented centre)))

(define (queue-window-event win thunk)
  (queue-event (send win get-eventspace) thunk))
