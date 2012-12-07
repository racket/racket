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
              CreateDialogIndirectParamW dialog-proc
              clean-up-destroyed
              MessageBoxW
              _WndProc
	      app-icon))

;; ----------------------------------------
;; We use the "user data" field of an HWND to
;;  store a weak pointer back to the Racket object.
;;  The weak pointer must be wrapped in an immuable cell.
;;  In addition, if we need to save a control's old
;;  ctlproc, we put it in the same immutable cell.

(define all-hwnds (make-hash))

;; call in atomic mode:
(define (register-hwnd! hwnd)
  (hash-set! all-hwnds (cast hwnd _pointer _intptr) #t)
  (let ([c (malloc-immobile-cell (vector #f #f #f))])
    (void (SetWindowLongPtrW hwnd GWLP_USERDATA c))))
  
(define (set-hwnd-wx! hwnd wx)
  (let* ([c (GetWindowLongPtrW hwnd GWLP_USERDATA)]
         [v (ptr-ref c _racket)])
    (vector-set! v 0 (make-weak-box wx))))

(define (set-hwnd-ctlproc! hwnd save-ptr ctlproc)
  (let* ([c (GetWindowLongPtrW hwnd GWLP_USERDATA)]
         [v (ptr-ref c _racket)])
    (vector-set! v 1 ctlproc)
    (vector-set! v 2 save-ptr)))

(define (hwnd->wx hwnd)
  (let ([c (GetWindowLongPtrW hwnd GWLP_USERDATA)])
    (and c (let ([v (ptr-ref c _racket)])
             (and v
                  (let ([wb (vector-ref v 0)])
                    (and wb
                         (weak-box-value wb))))))))

(define (any-hwnd->wx hwnd)
  (and
   (atomically (hash-ref all-hwnds (cast hwnd _pointer _intptr) #f))
   (let ([wx (hwnd->wx hwnd)])
     (and wx
          (send wx is-hwnd? hwnd)
          wx))))

(define (hwnd->ctlproc hwnd)
  (let ([c (GetWindowLongPtrW hwnd GWLP_USERDATA)])
    (and c (let ([v (ptr-ref c _racket)])
             (and v (vector-ref v 1))))))

(define (hwnd->ctlproc-fptr hwnd)
  (let ([c (GetWindowLongPtrW hwnd GWLP_USERDATA)])
    (and c (let ([v (ptr-ref c _racket)])
             (and v (vector-ref v 2))))))

;; call in atomic mode:
(define (can-unregister-hwnd? hwnd)
  (hash-ref all-hwnds (cast hwnd _pointer _intptr) #f))

;; call in atomic mode:
(define (unregister-hwnd! hwnd)
  (let ([c (GetWindowLongPtrW hwnd GWLP_USERDATA)])
    (when c
      (free-immobile-cell c)
      (SetWindowLongPtrW hwnd GWLP_USERDATA #f))
    (hash-remove! all-hwnds (cast hwnd _pointer _intptr))))

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
        (unregister-hwnd! w)
        (DefWindowProcW w msg wparam lparam))
      (let ([wx (hwnd->wx w)])
        (if wx
            (send wx wndproc w msg wparam lparam DefWindowProcW)
            (DefWindowProcW w msg wparam lparam)))))

(define wind-proc-ptr (function-ptr wind-proc _WndProc))

(define-user32 CallWindowProcW (_wfun _fpointer _HWND _UINT _WPARAM _LPARAM -> _LRESULT))

(define (control-proc w msg wParam lParam)
  (let ([default-ctlproc (hwnd->ctlproc-fptr w)])
    (if (= msg WM_DESTROY)
        (begin
          (SetWindowLongPtrW w GWLP_WNDPROC (hwnd->ctlproc-fptr w))
          (unregister-hwnd! w)
          (CallWindowProcW default-ctlproc w msg wParam lParam))
        (let ([wx (hwnd->wx w)])
          (if wx
              (send wx ctlproc w msg wParam lParam
                    (lambda (w msg wParam lParam)
                      (CallWindowProcW default-ctlproc w msg wParam lParam)))
              (CallWindowProcW default-ctlproc w msg wParam lParam))))))

(define control_proc (function-ptr control-proc _WndProc))

(define (subclass-control hwnd)
  (let* ([fptr (GetWindowLongPtrW hwnd GWLP_WNDPROC)]
         [old-control-proc (function-ptr fptr _WndProc)])
    (set-hwnd-ctlproc! hwnd fptr old-control-proc)
    (SetWindowLongPtrW hwnd GWLP_WNDPROC control_proc)))


(define _DialogProc (_wfun _HWND _UINT _WPARAM _LPARAM -> _INT_PTR))

(define (dlgproc w msg wParam lParam)
  (if (= msg WM_DESTROY)
      (begin
        (unregister-hwnd! w)
        0)
      (let ([wx (hwnd->wx w)])
        (if wx
            (send wx wndproc w msg wParam lParam
                  (lambda (w msg wParam lParam) 0))
            0))))

(define dialog-proc (function-ptr dlgproc _DialogProc))

;; ----------------------------------------

(define-user32 DestroyWindow (_wfun _HWND -> (r : _BOOL)
                                    -> (unless r (failed 'DestroyWindow))))

(define (maybe-destroy-window hwnd)
  (atomically
   (when (can-unregister-hwnd? hwnd)
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
   (lambda (dwExStyle lpClassName lpWindowName dwStyle 
                      x y nWidth nHeight 
                      hWndParent hMenu hInstance lpParam)
     (let ([hwnd (_CreateWindowExW dwExStyle lpClassName lpWindowName dwStyle 
                                   x y nWidth nHeight 
                                   hWndParent hMenu hInstance lpParam)])
       (register! hwnd)
       hwnd))))

(define CreateWindowExW (make-CreateWindowEx register-hwnd!))
(define CreateWindowExW/control (make-CreateWindowEx (lambda (hwnd)
                                                       (register-hwnd! hwnd)
                                                       (subclass-control hwnd))))


(define-user32 _CreateDialogIndirectParamW (_wfun _HINSTANCE
                                                  _pointer ; _DLGTEMPLATE-pointer
                                                  _HWND
                                                  _fpointer
                                                  _LPARAM
                                                  -> _HWND)
  #:c-id CreateDialogIndirectParamW)

(define CreateDialogIndirectParamW
  ((allocator remember-to-free-later)
   (lambda (hInstance lpTemplate hWndParent lpDialogFunc lParamInit)
     (let ([hwnd (_CreateDialogIndirectParamW
                  hInstance lpTemplate hWndParent lpDialogFunc lParamInit)])
       (register-hwnd! hwnd)
       hwnd))))

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

(define-kernel32 GetModuleFileNameW (_wfun #:save-errno 'windows _pointer _pointer _DWORD -> _DWORD))
(define ERROR_INSUFFICIENT_BUFFER 122)
(define-shell32 ExtractIconW (_wfun _HINSTANCE _string/utf-16 _UINT -> (r : _HICON)
                                    -> (or r (failed 'ExtractIconW))))

(define app-icon
  (let ([path
	 (let loop ([size 1024])
	   (let ([p (make-bytes (* (ctype-sizeof _WCHAR) 1024))])
	     (let ([r (GetModuleFileNameW #f p size)])
	       (cond
		[(and (or (zero? r) (= r size))
		      (= (saved-errno) ERROR_INSUFFICIENT_BUFFER))
		 (loop (* size 2))]
		[(zero? r) (failed 'GetModuleFileNameW)]
		[else (cast p _gcpointer _string/utf-16)]))))])
    (if path
	(ExtractIconW hInstance path 0)
	(LoadIconW #f IDI_APPLICATION))))

(void (RegisterClassW (make-WNDCLASS CS_OWNDC
				     wind-proc-ptr
				     0
                                     0
				     hInstance
				     app-icon
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
(register-no-cursor "SysListView32")
