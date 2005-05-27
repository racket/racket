(module mzrl mzscheme

(require (lib "foreign.ss")) (unsafe!)
(provide readline add-history set-completion-function!)

; libtermcap maybe needed
(define libtermcap  (with-handlers ([exn:fail? void])
		      (ffi-lib "libtermcap")))
(define libreadline (ffi-lib "libreadline"))

(define readline
  (get-ffi-obj "readline" libreadline (_fun _string -> _string/eof)))

(define add-history
  (get-ffi-obj "add_history" libreadline (_fun _string -> _void)))

;; Simple completion: use this with a (string -> list-of string) function that
;; returns the completions for a given string.  (should clean up bytes/string)
(define (set-completion-function! func)
  (if func
    (set-ffi-obj! "rl_completion_entry_function" libreadline
                  (_fun _string _int -> _pointer)
                  (completion-function func))
    (set-ffi-obj! "rl_completion_entry_function" libreadline _pointer #f)))
(define (completion-function func)
  (let ([cur '()])
    (define (complete str state)
      (if (zero? state)
        (begin (set! cur (func str)) (complete str 1))
        (and (pair? cur)
             (begin0 (malloc (add1 (bytes-length (car cur)))
                             (car cur) 'eternal)
               (set! cur (cdr cur))))))
    complete))

(set-ffi-obj! "rl_readline_name" libreadline _string "mzscheme")

;; make it possible to run Scheme threads while waiting for input
(set-ffi-obj! "rl_event_hook" libreadline (_fun -> _int)
              (lambda () (sync/enable-break (current-input-port)) 0))

)
