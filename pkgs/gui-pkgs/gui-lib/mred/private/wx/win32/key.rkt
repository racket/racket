#lang racket/base
(require racket/class
         ffi/unsafe
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "../common/event.rkt")

(provide
 (protect-out maybe-make-key-event
              generates-key-event?
	      reset-key-mapping
              key-symbol-to-menu-key))

(define-user32 GetKeyState (_wfun _int -> _SHORT))
(define-user32 MapVirtualKeyW (_wfun _UINT _UINT -> _UINT))
(define-user32 VkKeyScanW (_wfun _WCHAR -> _SHORT))

(define (generates-key-event? msg)
  (let ([message (MSG-message msg)])
    (and (or (eq? message WM_KEYDOWN)
             (eq? message WM_SYSKEYDOWN)
             (eq? message WM_KEYUP)
             (eq? message WM_SYSKEYUP))
         (maybe-make-key-event #t 
                               (MSG-wParam msg)
                               (MSG-lParam msg)
                               #f
                               (or (= message WM_KEYUP)
                                   (= message WM_SYSKEYUP))
                               (MSG-hwnd msg)))))

(define (THE_SCAN_CODE lParam)
  (bitwise-and (arithmetic-shift lParam -16) #x1FF))

(define generic_ascii_code (make-hasheq))

;; The characters in find_shift_alts are things that we'll try
;; to include in keyboard events as char-if-Shift-weren't-pressed,
;; char-if-AltGr-weren't-pressed, etc.
(define find_shift_alts (string-append
			 "!@#$%^&*()_+-=\\|[]{}:\";',.<>/?~`"
			 "abcdefghijklmnopqrstuvwxyz"
			 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			 "0123456789"))
(define other-key-codes #f)
(define (get-other-key-codes)
  (or other-key-codes
      (begin
	(set! other-key-codes
	      (list->vector
	       (for/list ([i (in-string find_shift_alts)])
		 (VkKeyScanW (char->integer i)))))
	other-key-codes)))
(define (reset-key-mapping)
  (set! other-key-codes #f))
(define (other-orig j)
  (char->integer (string-ref find_shift_alts j)))

;; If a virtual key code has no mapping here, then the key should be
;; ignored by WM_KEYDOWN and processed by WM_CHAR instead
(define win32->symbol
  (hasheq VK_CANCEL 'cancel
          VK_BACK #\backspace
          VK_TAB #\tab
          VK_CLEAR 'clear
          VK_RETURN #\return
          VK_SHIFT 'shift
          VK_CONTROL 'control
          VK_MENU  'menu
          VK_PAUSE 'pause
          VK_SPACE #\space
          VK_ESCAPE 'escape
          VK_PRIOR 'prior
          VK_NEXT  'next
          VK_END 'end
          VK_HOME  'home
          VK_LEFT  'left
          VK_UP 'up
          VK_RIGHT 'right
          VK_DOWN  'down
          VK_SELECT 'select
          VK_PRINT 'print
          VK_EXECUTE 'execute
          VK_INSERT 'insert
          VK_DELETE #\rubout
          VK_HELP  'help
          VK_NUMPAD0 'numpad0
          VK_NUMPAD1 'numpad1
          VK_NUMPAD2 'numpad2
          VK_NUMPAD3 'numpad3
          VK_NUMPAD4 'numpad4
          VK_NUMPAD5 'numpad5
          VK_NUMPAD6 'numpad6
          VK_NUMPAD7 'numpad7
          VK_NUMPAD8 'numpad8
          VK_NUMPAD9 'numpad9
          VK_MULTIPLY 'multiply
          VK_ADD 'add
          VK_SUBTRACT 'subtract
          VK_DECIMAL 'decimal
          VK_DIVIDE 'divide
          VK_F1 'f1
          VK_F2 'f2
          VK_F3 'f3
          VK_F4 'f4
          VK_F5 'f5
          VK_F6 'f6
          VK_F7 'f7
          VK_F8 'f8
          VK_F9 'f9
          VK_F10 'f10
          VK_F11 'f11
          VK_F12 'f12
          VK_F13 'f13
          VK_F14 'f14
          VK_F15 'f15
          VK_F16 'f16
          VK_F17 'f17
          VK_F18 'f18
          VK_F19 'f19
          VK_F20 'f20
          VK_F21 'f21
          VK_F22 'f22
          VK_F23 'f23
          VK_F24 'f24
          VK_NUMLOCK 'numlock
          VK_SCROLL 'scroll))


(define (maybe-make-key-event just-check? wParam lParam is-char? is-up? hwnd)
  (let* ([control-down? (not (zero? (arithmetic-shift (GetKeyState VK_CONTROL) -1)))]
         [rcontrol-down? (and control-down?
                              (not (zero? (arithmetic-shift (GetKeyState VK_RCONTROL) -1))))]
         [lcontrol-down? (and control-down?
                              (not (zero? (arithmetic-shift (GetKeyState VK_LCONTROL) -1))))]
         [shift-down? (not (zero? (arithmetic-shift (GetKeyState VK_SHIFT) -1)))]
	 [rshift-down? (and shift-down?
                            (not (zero? (arithmetic-shift (GetKeyState VK_RSHIFT) -1))))]
         [caps-down? (not (zero? (arithmetic-shift (GetKeyState VK_CAPITAL) -1)))]
         [alt-down? (= (bitwise-and (HIWORD lParam) KF_ALTDOWN) KF_ALTDOWN)]
         [ralt-down? (and alt-down?
			  (not (zero? (arithmetic-shift (GetKeyState VK_RMENU) -1))))]
         [lalt-down? (and alt-down?
			  (not (zero? (arithmetic-shift (GetKeyState VK_LMENU) -1))))])
    (let-values ([(id other-shift other-altgr other-shift-altgr)
                  (cond
		   [(symbol? wParam)
		    (values wParam #f #f #f)]
		   [is-char?
		      ;; wParam is a character or symbol
		      (let ([id wParam]
                            [sc (THE_SCAN_CODE lParam)])
                        ;; Remember scan codes to help with some key-release events:
                        (when (byte? id)
                          (hash-set! generic_ascii_code id sc))
                        ;; Look for elements of find_shift_alts that have a different
                        ;; shift/AltGr state:
                        (let ([k (MapVirtualKeyW sc 1)])
                          (if (zero? k)
                              (values (integer->char id) #f #f #f)
                              (for/fold ([id id][s #f][a #f][sa #f]) ([o (in-vector (get-other-key-codes))]
                                                                      [j (in-naturals)])
                                (if (= (bitwise-and o #xFF) k)
                                    ;; Figure out whether it's different in the shift
                                    ;; for AltGr dimension, or both:
                                    (if (eq? (zero? (bitwise-and o #x100)) shift-down?)
                                        ;; different Shift
                                        (if (eq? (= (bitwise-and o #x600) #x600)
                                                 (and control-down? alt-down?))
                                            ;; same AltGr
                                            (values id (other-orig j) a sa)
                                            ;; different AltGr
                                            (values id s a (other-orig j)))
                                        ;; same Shift
                                        (if (eq? (= (bitwise-and o #x600) #x600)
                                                 (and control-down? alt-down?))
                                            ;; same AltGr
                                            (values id s a sa)
                                            ;; different AltGr
                                            (values id s (other-orig j) sa)))
                                    (values id s a sa))))))]
		   [else
                      ;; wParam is a virtual key code
                      (let ([id (hash-ref win32->symbol wParam #f)]
                            [override-mapping? (and control-down?
						    ;; not AltGR:
						    (not (and lcontrol-down?
							      ralt-down?)))]
                            [try-generate-release
                             (lambda ()
                               (let ([sc (THE_SCAN_CODE lParam)])
                                 (for/fold ([id #f]) ([i (in-range 256)] #:when (not id))
                                   (and (equal? sc (hash-ref generic_ascii_code i #f))
                                        (let ([id i])
                                          (if (id . < . 127)
                                              (char->integer (char-downcase (integer->char id)))
                                              id))))))])
                        (if (not id)
                            (if (or override-mapping? is-up?)
                                ;; Non-AltGr Ctl- combination, or a release event: 
                                ;; map manually, because the default mapping is
                                ;; unsatisfactory
                                ;; Set id to the unshifted key:
                                (let* ([id (bitwise-and (MapVirtualKeyW wParam 2) #xFFFF)]
                                       [id (cond
                                            [(zero? id) #f]
                                            [(id . < . 128)
                                             (char->integer (char-downcase (integer->char id)))]
                                            [else id])])
                                  (let-values ([(s a sa)
                                                ;; Look for shifted alternate:
                                                (for/fold ([s #f][a #f][sa #f]) ([o (in-vector (get-other-key-codes))]
                                                                                 [j (in-naturals)])
                                                  (if (= (bitwise-and o #xFF) wParam)
                                                      (if (not (zero? (bitwise-and o #x100)))
                                                          (if (= (bitwise-and o #x600) #x600)
                                                              (values s a (other-orig j))
                                                              (values (other-orig j) a sa))
                                                          (if (= (bitwise-and o #x600) #x600)
                                                              (values s (other-orig j) sa)
                                                              (values s a sa)))
                                                      (values s a sa)))])
                                    (if (and id shift-down?)
                                        ;; shift was pressed, so swap role of shifted and unshifted
                                        (values s id sa a)
                                        (values id s a sa))))
                                (values (and is-up? (try-generate-release)) #f #f #f))
                            (cond
                             [(and (not override-mapping?) (not is-up?)
                                   ;; Let these get translated to WM_CHAR or skipped
                                   ;; entirely:
                                   (memq wParam
                                         (list VK_ESCAPE VK_SPACE VK_RETURN VK_TAB VK_BACK)))
                              (values #f #f #f #f)]
                             [(and (not id) is-up?)
                              (values (try-generate-release) #f #f #f)]
                             [else
                              (values id #f #f #f)])))])])
      (and id
           (if just-check?
               #t
               (let* ([id (if (number? id) (integer->char id) id)]
		      [key-id (case id
                                [(#\033) 'escape]
                                [(shift) (if rshift-down?
                                             'rshift
                                             id)]
                                [(control) (if rcontrol-down?
                                               'rcontrol
                                               id)]
                                [else id])]
                      [e (new key-event%
                              [key-code (if is-up?
                                           'release
                                           key-id)]
                              [shift-down shift-down?]
                              [control-down control-down?]
                              [meta-down alt-down?]
                              [alt-down #f]
                              [x 0]
                              [y 0]
                              [time-stamp 0]
                              [caps-down caps-down?]
			      [control+meta-is-altgr (and control-down?
                                                          alt-down?
							  (not rcontrol-down?)
							  (not lalt-down?))])]
		      [as-key (lambda (v)
				(if (integer? v) (integer->char v) v))])
		 (when is-up?
		   (send e set-key-release-code key-id))
		 (when other-shift 
		   (send e set-other-shift-key-code (as-key other-shift)))
		 (when other-altgr 
		   (send e set-other-altgr-key-code (as-key other-altgr)))
		 (when other-shift-altgr 
		   (send e set-other-shift-altgr-key-code (as-key other-shift-altgr)))
                 e))))))

(define (key-symbol-to-menu-key k)
  (hash-ref keysyms k #f))

(define keysyms
  '#hash((numpad5 . |Numpad 5|)
         (numpad1 . |Numpad 1|)
         (escape . Escape)
         (right . Right)
         (prior . Prior)
         (cancel . Cancel)
         (start . Start)
         (f22 . F22)
         (f17 . F17)
         (f13 . F13)
         (f8 . F8)
         (f3 . F3)
         (divide . Divide)
         (add . Add)
         (numpad8 . |Numpad 8|)
         (numpad3 . |Numpad 3|)
         (select . Select)
         (down . Down)
         (next . Next)
         (clear . Clear)
         (scroll . Scroll)
         (f21 . F21)
         (f16 . F16)
         (f12 . F12)
         (f9 . F9)
         (f4 . F4)
         (f1 . F1)
         (separator . Separator)
         (numpad9 . |Numpad 9|)
         (numpad4 . |Numpad 4|)
         (help . Help)
         (execute . Execute)
         (left . Left)
         (end . End)
         (menu . Menu)
         (print . Print)
         (f23 . F23)
         (f18 . F18)
         (f14 . F14)
         (f7 . F7)
         (f2 . F2)
         (decimal . Decimal)
         (multiply . Multiply)
         (numpad7 . |Numpad 7|)
         (numpad2 . |Numpad 2|)
         (insert . Insert)
         (snapshot . Snapshot)
         (up . Up)
         (home . Home)
         (pause . Pause)
         (f24 . F24)
         (f19 . F19)
         (f15 . F15)
         (f11 . F11)
         (f6 . F6)
         (f5 . F5)
         (subtract . Subtract)
         (numpad-enter . |Numpad Enter|)
         (numpad6 . |Numpad 6|)))
