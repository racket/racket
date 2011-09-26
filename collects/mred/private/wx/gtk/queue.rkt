#lang racket/base
(require racket/class
         ffi/unsafe
         "utils.rkt"
         "types.rkt"
         "../../lock.rkt"
         "../common/queue.rkt"
         "../common/freeze.rkt"
         "clipboard.rkt"
         "const.rkt"
	 "w32.rkt"
         "unique.rkt")

(provide (protect-out gtk-start-event-pump
                      try-to-sync-refresh
                      set-widget-hook!
                      x11-display)
         ;; from common/queue:
         current-eventspace
         queue-event
         yield)


;; ------------------------------------------------------------
;; Gtk initialization

(define-gtk gtk_init_check (_fun (_ptr io _int) (_ptr io _gcpointer) -> _gboolean))
(define-gdk gdk_set_program_class (_fun _string -> _void))

(define x11-display
  (let* ([argc-ptr (scheme_register_process_global "PLT_X11_ARGUMENT_COUNT" #f)]
         [argc (or (and argc-ptr (cast argc-ptr _pointer _long)) 0)]
         [argv (and (positive? argc)
                    (scheme_register_process_global "PLT_X11_ARGUMENTS" #f))]
         [display (getenv "DISPLAY")])
    ;; Convert X11 arguments, if any, to Gtk form:
    (let-values ([(args single-instance?)
                  (if (zero? argc)
                      (values null #f)
                      (let loop ([i 1][si? #f])
                        (if (= i argc) 
                            (values null si?)
                            (let ([s (ptr-ref argv _bytes i)])
                              (cond
                               [(bytes=? s #"-display")
                                (let-values ([(args si?) (loop (+ i 2) si?)]
                                             [(d) (ptr-ref argv _bytes (add1 i))])
                                  (set! display (bytes->string/utf-8 d #\?))
                                  (values (list* #"--display" d args)
                                          si?))]
                               [(bytes=? s #"-synchronous")
                                (let-values ([(args si?) (loop (+ i 1) si?)])
                                  (values (cons #"--sync" args)
                                          si?))]
                               [(bytes=? s #"-singleInstance")
                                (loop (add1 i) #t)]
                               [(or (bytes=? s #"-iconic")
                                    (bytes=? s #"-rv")
                                    (bytes=? s #"+rv")
                                    (bytes=? s #"-reverse"))
                                ;; ignored with 0 arguments
                                (loop (add1 i) #t)]
                               [else
                                ;; all other ignored flags have a single argument
                                (loop (+ i 2) #t)])))))])
      (let-values ([(new-argc new-argv)
                    (values (add1 (length args))
                            (cast (cons (path->bytes (find-system-path 'run-file))
                                        args)
                                  (_list i _bytes)
                                  _pointer))])
        (unless (gtk_init_check new-argc new-argv)
          (error (format
                  "Gtk initialization failed for display ~s"
                  (or display ":0"))))
        (when single-instance?
          (do-single-instance))
        (let ([v (scheme_register_process_global "Racket-GUI-wm-class" #f)])
          (when v
            (gdk_set_program_class (cast v _pointer _string))))
        display))))

;; ------------------------------------------------------------
;; Gtk event pump

(define-gtk gtk_events_pending (_fun -> _gboolean))
(define-gtk gtk_main_iteration_do (_fun _gboolean -> _gboolean))

(define _GMainContext (_cpointer 'GMainContext))
(define _GdkEvent (_cpointer 'GdkEvent))

(define-cstruct _GPollFD ([fd _int]
                          [events _short]
                          [revents _short]))

(define-glib g_main_context_default (_fun -> _GMainContext))
(define-glib g_main_context_query (_fun _GMainContext
					_int
					_pointer
					_pointer ;; GPollFD array
					_int
					-> _int))

(define-gdk gdk_event_handler_set (_fun (_fun _GdkEvent _pointer -> _void)
                                        _pointer
                                        (_fun _pointer -> _void)
                                        -> _void))
(define-gdk gdk_event_copy (_fun _GdkEvent -> _GdkEvent))
(define-gdk gdk_event_free (_fun _GdkEvent -> _void))
(define-gtk gtk_main_do_event (_fun _GdkEvent -> _void))
(define-gtk gtk_get_event_widget (_fun _GdkEvent -> (_or-null _GtkWidget)))

(define poll-fd-count 1)
(define poll-fds (make-GPollFD 0 0 0))
(define timeout (malloc _int))

;; These are OS-specific, but they tend to be the same across OSes:
(define POLLIN #x1)
(define POLLOUT #x4)
(define POLLERR #x8)
(define POLLHUP #x10)

(define-mz scheme_get_fdset (_fun _pointer _int -> _gcpointer))
(define-mz scheme_fdset (_fun _gcpointer _int -> _void))
(define-mz scheme_set_wakeup_time (_fun _gcpointer _double -> _void))
(define-mz scheme_add_fd_eventmask (_fun _gcpointer _int -> _void)
  #:fail #f)

(define (install-wakeup fds)
  (let ([n (g_main_context_query (g_main_context_default)
                                 #x7FFFFFFF ; max-int, hopefully
                                 timeout
                                 poll-fds
                                 poll-fd-count)])
    (let ([to (ptr-ref timeout _int)])
      (when (to . >= . 0)
        (scheme_set_wakeup_time fds (+ (current-inexact-milliseconds) to))))
    (if (n . > . poll-fd-count)
        (begin
          (set! poll-fds (malloc _GPollFD n))
          (set! poll-fd-count n)
          (install-wakeup fds))
	(if (eq? 'windows (system-type))
	    ;; We don't know how to deal with GLib FDs under
	    ;;  Windows, but we should wake up on any Windows event
	    (scheme_add_fd_eventmask fds QS_ALLINPUT)
	    ;; Normal FD handling under Unix variants:
            (for ([i (in-range n)])
              (let* ([gfd (ptr-ref poll-fds _GPollFD i)]
                     [fd (GPollFD-fd gfd)]
                     [events (GPollFD-events gfd)])
                (when (not (zero? (bitwise-and events POLLIN)))
                  (scheme_fdset (scheme_get_fdset fds 0) fd))
                (when (not (zero? (bitwise-and events POLLOUT)))
                  (scheme_fdset (scheme_get_fdset fds 1) fd))
                (when (not (zero? (bitwise-and events (bitwise-ior POLLERR POLLHUP))))
                  (scheme_fdset (scheme_get_fdset fds 2) fd))))))))

(set-check-queue! gtk_events_pending)
(set-queue-wakeup! install-wakeup)

(define widget-hook (lambda (gtk) #f))
(define (set-widget-hook! proc) (set! widget-hook proc))

(define (event-dispatch evt ignored)
  (let* ([gtk (gtk_get_event_widget evt)]
         [wx (and gtk (widget-hook gtk))])
    (cond
     [(and (= (ptr-ref evt _GdkEventType) GDK_EXPOSE)
           wx
           (send wx direct-update?))
      (gtk_main_do_event evt)]
     [(or
       ;; event for a window that we control?
       (and wx (send wx get-eventspace))
       ;; event to get X selection data?
       (and (= (ptr-ref evt _GdkEventType) GDK_SELECTION_REQUEST)
            (let ([s (cast evt _pointer _GdkEventSelection-pointer)])
              (= (GdkEventSelection-selection s)
                 primary-atom))
            (get-selection-eventspace)))
      => (lambda (e)
           (let ([evt (gdk_event_copy evt)])
             (queue-event e (lambda () 
                              (call-as-nonatomic-retry-point
                               (lambda ()
                                 (gtk_main_do_event evt)
                                 (gdk_event_free evt)))))))]
     [else
      (gtk_main_do_event evt)])))
(define (uninstall ignored)
  (printf "uninstalled!?\n"))

(gdk_event_handler_set event-dispatch
                       #f
                       uninstall)

(define (dispatch-all-ready)
  (pre-event-sync #f)
  (clean-up-destroyed)
  (when (gtk_events_pending)
    (gtk_main_iteration_do #f)
    (dispatch-all-ready)))

(define-gdk gdk_window_process_all_updates (_fun -> _void))

(define (gtk-start-event-pump)
  (thread (lambda ()
            (let loop ()
              (unless (let ([any-tasks? (sync/timeout 0 boundary-tasks-ready-evt)])
                        (sync/timeout (and any-tasks? (* sometimes-delay-msec 0.001))
                                      queue-evt 
                                      (if any-tasks?
                                          (wrap-evt (system-idle-evt)
                                                    (lambda (v) #f))
                                          boundary-tasks-ready-evt)))
                (pre-event-sync #t))
              (atomically (dispatch-all-ready))
              (loop)))))

(define (try-to-sync-refresh)
  (atomically
   (pre-event-sync #t)))
