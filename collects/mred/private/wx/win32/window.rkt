#lang racket/base
(require ffi/unsafe
	 racket/class
	 racket/draw
          "../../syntax.rkt"
	 "utils.rkt"
	 "types.rkt"
	 "wndclass.rkt"
	 "queue.rkt")

(provide window%
	 queue-window-event
	 
	 CreateWindowExW
	 GetWindowRect)

(define-user32 CreateWindowExW (_wfun _DWORD
				      _string/utf-16
				      _string/utf-16
				      _DWORD
				      _int _int _int _int
				      _HWND _HMENU _HINSTANCE _pointer
				      -> _HWND))
(define-user32 GetWindowRect (_wfun _HWND (r : (_ptr o _RECT)) -> _void -> r))
(define-user32 GetClientRect (_wfun _HWND (r : (_ptr o _RECT)) -> _void -> r))

(define-user32 MoveWindow(_wfun _HWND _int _int _int _int _BOOL -> _BOOL))

(define-user32 ShowWindow (_wfun _HWND _int -> _BOOL))
(define SW_SHOW 5)
(define SW_HIDE 0)

(define-user32 GetDialogBaseUnits (_fun -> _LONG))
(define measure-dc #f)

(define-values (dlu-x dlu-y)
  (let ([v (GetDialogBaseUnits)])
    (values (* 1/4 (bitwise-and v #xFF))
	    (* 1/8 (arithmetic-shift v -16)))))

(defclass window% object%
  (init-field parent win32)
  (init style)
  
  (super-new)
  
  (define eventspace (current-eventspace))

  (set-win32-wx! win32 this)

  (unless (memq 'invisible style)
    (show #t))
  
  (define/public (get-win32) win32)
  (define/public (get-client-win32) win32)
  (define/public (get-eventspace) eventspace)
  
  (define/public (wndproc w msg wparam lparam)
    (DefWindowProcW w msg wparam lparam))
  
  (define/public (show on?)
    (direct-show on?))

  (define/public (direct-show on?)
    (void (ShowWindow win32 (if on? SW_SHOW SW_HIDE))))

  (def/public-unimplemented on-drop-file)
  (def/public-unimplemented pre-on-event)
  (def/public-unimplemented pre-on-char)
  (def/public-unimplemented on-size)
  (def/public-unimplemented on-set-focus)
  (def/public-unimplemented on-kill-focus)
  (def/public-unimplemented get-handle)

  (define/public (is-window-enabled?)
    #t)

  (define/public (is-enabled-to-root?)
    (and (is-window-enabled?)
	 (send parent is-enabled-to-root?)))

  (define/public (is-shown-to-root?)
    (and (is-shown?)
	 (send parent is-shown-to-root?)))

  (define/public (is-shown?)
    #t)

  (def/public-unimplemented set-phantom-size)

  (define/public (get-x)
    (let ([r (GetWindowRect win32)])
      (- (RECT-left r) (send parent get-x))))
  (define/public (get-y)
    (let ([r (GetWindowRect win32)])
      (- (RECT-top r) (send parent get-y))))

  (define/public (get-width)
    (let ([r (GetWindowRect win32)])
      (- (RECT-right r) (RECT-left r))))
  (define/public (get-height)
    (let ([r (GetWindowRect win32)])
      (- (RECT-bottom r) (RECT-top r))))

  (define/public (set-size x y w h)
    (void
     (if (or (= x -11111)
	     (= y -11111)
	     (= w -1)
	     (= h -1))
	 (let ([r (GetWindowRect win32)])
	   (MoveWindow win32 
		       (if (= x -11111) (RECT-left r) x)
		       (if (= y -11111) (RECT-right r) y)
		       (if (= w -1) (- (RECT-right r) (RECT-left r)) w)
		       (if (= h -1) (- (RECT-bottom r) (RECT-top r)) h)
		       #t))
	 (MoveWindow win32 x y w h #t))))
  (define/public (move x y)
    (set-size x y -1 -1))

  (define/public (auto-size label min-w min-h)
    (unless measure-dc
      (let* ([bm (make-object bitmap% 1 1)]
	     [dc (make-object bitmap-dc% bm)]
	     [font (make-object font% 8 'system)])
	(send dc set-font font)
	(set! measure-dc dc)))
    (let-values ([(w h d a) (send measure-dc get-text-extent label #f #t)]
		 [(->int) (lambda (v) (inexact->exact (floor v)))])
      (set-size -11111 -11111 
		(max (->int w) (->int (* dlu-x min-w)))
		(max (->int h) (->int (* dlu-y min-h))))))

  (def/public-unimplemented popup-menu)
  (def/public-unimplemented center)

  (define/public (get-parent) parent)

  (def/public-unimplemented refresh)
  (def/public-unimplemented screen-to-client)
  (def/public-unimplemented client-to-screen)
  (def/public-unimplemented drag-accept-files)
  (def/public-unimplemented enable)
  (def/public-unimplemented get-position)

  (define/public (get-client-size w h)
    (let ([r (GetClientRect (get-client-win32))])
      (set-box! w (- (RECT-right r) (RECT-left r)))
      (set-box! h (- (RECT-bottom r) (RECT-top r)))))

  (def/public-unimplemented get-size)
  (def/public-unimplemented fit)
  (def/public-unimplemented set-cursor)
  (def/public-unimplemented set-focus)
  (def/public-unimplemented gets-focus?)
  (def/public-unimplemented centre))

;; ----------------------------------------

(define (queue-window-event win thunk)
  (queue-event (send win get-eventspace) thunk))
