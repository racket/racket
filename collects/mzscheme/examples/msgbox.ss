;; Build with
;;   mzc ++ldf user32.lib msgbox.ss
;; so that MessageBox() is linked in.

(module msgbox mzscheme
  (require (lib "cffi.ss" "compiler"))
  
  ;; c-declare is really file-specific, and
  ;;  not module-specific
  (c-declare "#include <windows.h>")

  ;; A direct hook to the MessageBox() function.
  ;; We never have a parent window, but NULL is ok,
  ;;  and we can pretend that a window is a string
  ;;  for the purposes of providing a NULL:
  (define unsafe-message-box
    (c-lambda ((pointer "void") ; always use #f
	       nonnull-char-string ; title
	       nonnull-char-string ; message
	       int)   ; style
	      int
	      "MessageBox"))

  ;; Functions that really just access constants:
  (define get-mb-okcancel
    (c-lambda () int "___result = MB_OKCANCEL;"))
  (define get-mb-yesno
    (c-lambda () int "___result = MB_YESNO;"))

  ;; Nice function for clients to use:
  (define (message-box title message style)
    (unsafe-message-box
     #f
     title
     message
     (case style
       [(ok-cancel) (get-mb-okcancel)]
       [(yes-no) (get-mb-yesno)]
       [else (raise-type-error 'message-box "'ok-cancel or 'yes-no" style)])))

  (provide message-box))
