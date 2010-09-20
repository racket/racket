#lang racket/base
(require ffi/unsafe
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "../../lock.rkt"
         "../common/queue.rkt")

(provide win32-start-event-pump

         ;; from common/queue:
         current-eventspace
         queue-event
         yield)

;; ------------------------------------------------------------
;; Win32 event pump

(define _LPMSG _pointer)

(define-cstruct _MSG ([hwnd _HWND]
		      [message _UINT]
		      [wParam _WPARAM]
		      [lParam _LPARAM]
		      [time _DWORD]
		      [pt _POINT]))

(define-user32 GetQueueStatus (_wfun _UINT -> _DWORD))
(define-user32 GetMessageW (_wfun _LPMSG _HWND _UINT _UINT -> _BOOL))
(define-user32 PeekMessageW (_wfun _LPMSG _HWND _UINT _UINT _UINT -> _BOOL))
(define-user32 TranslateMessage (_wfun _LPMSG -> _BOOL))
(define-user32 DispatchMessageW (_wfun _LPMSG -> _LRESULT))
(define-user32 PostQuitMessage (_wfun _int -> _void))

(define-mz scheme_add_fd_eventmask (_fun _pointer _int -> _void))

(define msg (malloc _MSG 'raw))

(define (events-ready?)
  (GetQueueStatus QS_ALLINPUT))

(define (install-wakeup fds)
  (pre-event-sync #t)
  (scheme_add_fd_eventmask fds QS_ALLINPUT))

(set-check-queue! events-ready?)
(set-queue-wakeup! install-wakeup)

(define (dispatch-all-ready)
  (pre-event-sync #f)
  (let ([v (PeekMessageW msg #f 0 0 PM_REMOVE)])
    (when v
      (TranslateMessage msg)
      (DispatchMessageW msg)
      (dispatch-all-ready))))

(define (win32-start-event-pump)
  (thread (lambda ()
            (let loop ()
              (sync queue-evt)
              (as-entry dispatch-all-ready)
              (loop)))))
