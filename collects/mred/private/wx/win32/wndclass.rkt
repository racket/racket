#lang racket/base
(require ffi/unsafe
         ffi/unsafe/alloc
	 racket/class
         "../../lock.rkt"
         "../common/utils.rkt"
	 "utils.rkt"
	 "types.rkt"
	 "const.rkt"
	 "icons.rkt")

(provide
 (protect-out hInstance
              DefWindowProcW
              background-hbrush
              set-hwnd-wx!
              hwnd->wx
              hwnd->ctlproc
              any-hwnd->wx
              CreateWindowExW
              CreateWindowExW/control
              clean-up-destroyed
              MessageBoxW
              _WndProc))

;; ----------------------------------------
;; We use the "user data" field of an HWND to
;;  store a weak pointer back to the Racket object.
;;  The weak pointer must be wrapped in an immuable cell.
;;  In addition, if we need to save a control's old
;;  ctlproc, we put it in the same immutable cell.
;; So:
;;  <user-data>   = (make-immutable-cell <remembered>)
;;  <remembered>  = <wx-weak-box>
;;                | (cons <ctlproc> <wx-weak-box>)
;;  <wx-weak-box> = (make-weak-box <object%>)

(define all-hwnds (make-hash))

;; call in atomic mode:
(define (register-hwnd! hwnd)
  (hash-set! all-hwnds (cast hwnd _pointer _long) hwnd))

;; call in atomic mode:
(define (alloc-hwnd-cell hwnd)
  (let ([c (GetWindowLongW hwnd GWLP_USERDATA)])
    (or c
        (let ([c (malloc-immobile-cell #f)])
          (SetWindowLongW hwnd GWLP_USERDATA c)
          c))))

(define (set-hwnd-wx! hwnd wx)
  (let* ([c (atomically (alloc-hwnd-cell hwnd))]
         [v (ptr-ref c _racket)])
    (ptr-set! c _racket (cons (make-weak-box wx)
                              (and v (cdr v))))))

(define (set-hwnd-ctlproc! hwnd ctlproc)
  (let* ([c (atomically (alloc-hwnd-cell hwnd))]
         [v (ptr-ref c _racket)])
    (ptr-set! c _racket (cons (and v (car v))
                              ctlproc))))

(define (hwnd->wx hwnd)
  (let ([c (GetWindowLongW hwnd GWLP_USERDATA)])
    (and c (let ([wb (ptr-ref c _racket)])
             (and wb
                  (car wb)
                  (weak-box-value (car wb)))))))

(define (hwnd->ctlproc hwnd)
  (let ([c (GetWindowLongW hwnd GWLP_USERDATA)])
    (and c (let ([wb (ptr-ref c _racket)])
             (and wb (cdr wb))))))

(define (any-hwnd->wx hwnd)
  (and
   (atomically (hash-ref all-hwnds (cast hwnd _pointer _long) #f))
   (let ([c (GetWindowLongW hwnd GWLP_USERDATA)])
     (and c 
          (let ([wx (let ([wb (ptr-ref c _racket)])
                      (and wb 
                           (car wb)
                           (weak-box-value (car wb))))])
            (and wx
                 (send wx is-hwnd? hwnd)
                 wx))))))

;; call in atomic mode:
(define (unregister-hwnd? hwnd [same? (lambda (v) (eq? v hwnd))])
  (let ([addr (cast hwnd _pointer _long)])
    (and (same? (hash-ref all-hwnds addr #f))
         (let ([c (GetWindowLongW hwnd GWLP_USERDATA)])
           (when c
             (free-immobile-cell c)
             (SetWindowLongW hwnd GWLP_USERDATA #f))
           (hash-remove! all-hwnds addr)
           #t))))

;; ----------------------------------------

(define-cstruct _INITCOMMONCONTROLSEX
  ([dwSize _DWORD]
   [dwICC _DWORD]))

(define-comctl32 InitCommonControlsEx (_wfun _INITCOMMONCONTROLSEX-pointer -> _BOOL))

(void
 (InitCommonControlsEx (make-INITCOMMONCONTROLSEX
			(ctype-sizeof _INITCOMMONCONTROLSEX)
			0)))

;; ----------------------------------------

(define _WndProc (_wfun #:atomic? #t #:keep (box null)
			_HWND _UINT _WPARAM _LPARAM -> _LRESULT))

(define (wind-proc w msg wparam lparam)
  (if (= msg WM_DESTROY)
      (begin
        (unregister-hwnd? w (lambda (x) x))
        (DefWindowProcW w msg wparam lparam))
      (let ([wx (hwnd->wx w)])
        (if wx
            (send wx wndproc w msg wparam lparam DefWindowProcW)
            (DefWindowProcW w msg wparam lparam)))))

(define wind-proc-ptr (function-ptr wind-proc _WndProc))

(define (control-proc w msg wParam lParam)
  (if (= msg WM_DESTROY)
      (let ([default-ctlproc (hwnd->ctlproc w)])
        (unregister-hwnd? w (lambda (x) x))
        (default-ctlproc w))
      (let ([wx (hwnd->wx w)])
        (if wx
            (send wx ctlproc w msg wParam lParam
                  (lambda (w msg wParam lParam)
                    ((hwnd->ctlproc w) w msg wParam lParam)))
            (let ([default-ctlproc (hwnd->ctlproc w)])
              (default-ctlproc w msg wParam lParam))))))

(define control_proc (function-ptr control-proc _WndProc))

(define (subclass-control hwnd)
  (let ([old-control-proc (function-ptr (GetWindowLongW hwnd GWLP_WNDPROC) _WndProc)])
    (set-hwnd-ctlproc! hwnd old-control-proc)
    (SetWindowLongW hwnd GWLP_WNDPROC control_proc)))

;; ----------------------------------------

(define-user32 DestroyWindow (_wfun _HWND -> (r : _BOOL)
                                    -> (unless r (failed 'DestroyWindow))))

(define (maybe-destroy-window hwnd)
  (atomically
   (when (unregister-hwnd? hwnd)
     (DestroyWindow hwnd))))

(define (clean-up-destroyed)
  (free-remembered-now maybe-destroy-window))

(define-user32 _CreateWindowExW (_wfun _DWORD
                                       _string/utf-16
                                       _string/utf-16
                                       _UDWORD
                                       _int _int _int _int
                                       _HWND _HMENU _HINSTANCE _pointer
                                       -> _HWND)
  #:c-id CreateWindowExW)

(define (make-CreateWindowEx register!)
  ((allocator remember-to-free-later)
   (lambda (dwExStyle lpClassName lpWindowName dwStyle x y nWidth nHeight hWndParent hMenu hInstance lpParam)
     (let ([hwnd (_CreateWindowExW dwExStyle lpClassName lpWindowName dwStyle x y nWidth nHeight hWndParent hMenu hInstance lpParam)])
       (register! hwnd)
       hwnd))))

(define CreateWindowExW (make-CreateWindowEx register-hwnd!))
(define CreateWindowExW/control (make-CreateWindowEx (lambda (hwnd)
                                                       (register-hwnd! hwnd)
                                                       (subclass-control hwnd))))

;; ----------------------------------------

(define-cstruct _WNDCLASS ([style _UINT]
			   [lpfnWndProc _fpointer]
			   [cbClsExtra _int]
			   [cbWndExtra _int]
			   [hInstace _HINSTANCE]
			   [hIcon _HICON]
			   [hCursor _HCURSOR]
			   [hbrBackground _HBRUSH]
			   [lpszMenuName _permanent-string/utf-16]
			   [lpszClassName _permanent-string/utf-16]))

(define-user32 RegisterClassW (_wfun _WNDCLASS-pointer -> _ATOM))
(define-kernel32 GetModuleHandleW (_wfun _pointer -> _HINSTANCE))
(define-user32 LoadCursorW (_wfun _HINSTANCE _pointer -> _HCURSOR))
(define-user32 LoadIconW (_wfun _HINSTANCE _string/utf-16 -> _HICON))

(define-user32 GetClassInfoW (_wfun _HINSTANCE _string/utf-16 (i : (_ptr o _WNDCLASS)) -> (r : _BOOL)
                                    -> (if r i (failed 'GetClassInfoW))))

(define-user32 DefWindowProcW (_wfun _HWND _UINT _WPARAM _LPARAM -> _LRESULT))
(define-user32 DefWindowProcW/raw _fpointer
  #:c-id DefWindowProcW)

#;(define-user32 PostQuitMessage (_wfun _int -> _void))

(define hInstance (GetModuleHandleW #f))

(define background-hbrush (let ([p (ptr-add #f (+ COLOR_BTNFACE 1))])
                            (cpointer-push-tag! p 'HBRUSH)
                            p))
 
(void (RegisterClassW (make-WNDCLASS CS_OWNDC
				     wind-proc-ptr
				     0
                                     0
				     hInstance
				     (LoadIconW hInstance "WXSTD_FRAME")
                                     #f
                                     background-hbrush
				     #f ; menu
				     "PLTFrame")))

(void (RegisterClassW (make-WNDCLASS 0 ; using CS_OWNDC creates trouble when resizing?
				     wind-proc-ptr
				     0
                                     0
				     hInstance
				     #f
                                     #f
                                     #f ; transparent
				     #f ; menu
				     "PLTCanvas")))

(void (RegisterClassW (make-WNDCLASS 0
				     wind-proc-ptr
				     0
                                     0
				     hInstance
				     #f
				     #f
                                     background-hbrush
				     #f ; menu
				     "PLTPanel")))

(define controls-are-transparent? #f)

(void (RegisterClassW (make-WNDCLASS 0
				     wind-proc-ptr
				     0
                                     0
				     hInstance
				     #f
				     #f
                                     (if controls-are-transparent?
                                         #f  ; transparent
                                         background-hbrush)
				     #f ; menu
				     "PLTTabPanel")))

(define-user32 MessageBoxW (_fun _HWND _string/utf-16 _string/utf-16 _UINT -> _int))

(define (register-no-cursor orig-name)
  (let ([i (GetClassInfoW hInstance orig-name)])
    (set-WNDCLASS-lpszClassName! i (string-append "PLT" orig-name))
    (set-WNDCLASS-hCursor! i #f)
    (void (RegisterClassW i))))

(register-no-cursor "BUTTON")
(register-no-cursor "STATIC")
(register-no-cursor "LISTBOX")
(register-no-cursor "COMBOBOX")
(register-no-cursor "msctls_trackbar32")
(register-no-cursor "msctls_progress32")
(register-no-cursor "SysTabControl32")
