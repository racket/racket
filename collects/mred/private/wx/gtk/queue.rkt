#lang racket/base
(require racket/class
         ffi/unsafe
         "utils.rkt"
         "types.rkt"
         "../../lock.rkt"
         "../common/queue.rkt"
         "../common/freeze.rkt"
         "const.rkt"
	 "w32.rkt")

(provide gtk-start-event-pump

         set-widget-hook!

         ;; from common/queue:
         current-eventspace
         queue-event
         yield)

;; ------------------------------------------------------------
;; Gtk event pump

(define-gtk gtk_init (_fun _int _pointer -> _void))
(gtk_init 0 #f)

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

(define-mz scheme_get_fdset (_fun _pointer _int -> _pointer))
(define-mz scheme_fdset (_fun _pointer _int -> _void))
(define-mz scheme_set_wakeup_time (_fun _pointer _double -> _void))
(define-mz scheme_add_fd_eventmask (_fun _pointer _int -> _void)
  #:fail #f)

(define (install-wakeup fds)
  (pre-event-sync #t)
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
     [(and (= (ptr-ref evt _int) GDK_EXPOSE)
           wx
           (send wx direct-update?))
      (gtk_main_do_event evt)]
     [(and wx (send wx get-eventspace))
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
  (when (gtk_events_pending)
    (gtk_main_iteration_do #f)
    (dispatch-all-ready)))

(define (gtk-start-event-pump)
  (thread (lambda ()
            (let loop ()
              (sync queue-evt)
              (atomically (dispatch-all-ready))
              (loop)))))
