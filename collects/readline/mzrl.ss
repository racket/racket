(module mzrl mzscheme

(require (lib "foreign.ss")) (unsafe!)
(provide readline readline-bytes
         add-history add-history-bytes
         set-completion-function!)

;; libtermcap maybe needed
(define libtermcap  (with-handlers ([exn:fail? void]) (ffi-lib "libtermcap")))
(define libreadline (ffi-lib "libreadline"))

(define readline
  (get-ffi-obj "readline" libreadline (_fun _string -> _string/eof)))

(define readline-bytes
  (get-ffi-obj "readline" libreadline (_fun _bytes -> _bytes/eof)))

(define add-history
  (get-ffi-obj "add_history" libreadline (_fun _string -> _void)))

(define add-history-bytes
  (get-ffi-obj "add_history" libreadline (_fun _bytes -> _void)))

;; Simple completion: use this with a (string -> list-of string) function that
;; returns the completions for a given string.  (should clean up bytes/string)
(define set-completion-function!
  (case-lambda
    [(func) (set-completion-function! _string)]
    [(func type)
     (if func
       (set-ffi-obj! "rl_completion_entry_function" libreadline
                     (_fun type _int -> _pointer)
                     (completion-function func))
       (set-ffi-obj! "rl_completion_entry_function" libreadline _pointer #f))]))
(define (completion-function func)
  (let ([cur '()])
    (define (complete str state)
      (if (zero? state)
        (begin (set! cur (func str)) (complete str 1))
        (and (pair? cur)
             (begin0 (malloc (add1 (bytes-length (car cur))) (car cur) 'raw)
               (set! cur (cdr cur))))))
    complete))

(set-ffi-obj! "rl_readline_name" libreadline _bytes #"mzscheme")

;; need to capture the real input port below
(define real-input-port (current-input-port))
(unless (eq? 'stdin (object-name (current-input-port)))
  (fprintf (current-output-port)
           "mzrl warning: could not capture the real input port"))

;; make it possible to run Scheme threads while waiting for input
(set-ffi-obj! "rl_event_hook" libreadline (_fun -> _int)
              (lambda () (sync/enable-break real-input-port) 0))

)
