#lang scheme/base

(require mzlib/foreign (only-in '#%foreign ffi-obj)) (unsafe!)
(provide readline readline-bytes
         add-history add-history-bytes
         history-length history-get history-delete
         set-completion-function!)

;; libtermcap needed on some platforms
(define libtermcap  (with-handlers ([exn:fail? void]) (ffi-lib "libtermcap")))
(define libreadline (ffi-lib "libreadline" '("5" "6" "4" "")))

(define make-byte-string ; helper for the two types below
  (get-ffi-obj "scheme_make_byte_string" #f (_fun _pointer -> _scheme)))

(define _bytes/eof/free ; register a finalizer on the resulting bytes
  (make-ctype _pointer
    (lambda (x) (and (not (eof-object? x)) x))
    (lambda (x)
      (if x
        (let ([b (make-byte-string x)])
          (register-finalizer b (lambda (_) (free x)))
          b)
        eof))))

(define _string/eof/free ; make a Scheme str from C str & free immediately
  (make-ctype _pointer
    (lambda (x) (and (not (eof-object? x)) (string->bytes/utf-8 x)))
    (lambda (x)
      (if x
        (let ([s (bytes->string/utf-8 (make-byte-string x))]) (free x) s)
        eof))))

(define readline
  (get-ffi-obj "readline" libreadline (_fun _string -> _string/eof/free)))

(define readline-bytes
  (get-ffi-obj "readline" libreadline (_fun _bytes -> _bytes/eof/free)))

(define add-history
  (get-ffi-obj "add_history" libreadline (_fun _string -> _void)))

(define add-history-bytes
  (get-ffi-obj "add_history" libreadline (_fun _bytes -> _void)))

(define history-length
  (let ([hl (ffi-obj #"history_length" libreadline)])
    (lambda () (ptr-ref hl _int))))
(define history-base
  (let ([hb (ffi-obj #"history_base" libreadline)])
    (lambda () (ptr-ref hb _int))))

;; The history library has this great feature: *some* function consume
;; an index that is relative to history_base, and *some* get a plain
;; offset.  Someone just had so much fun they had to share.  This
;; deals with this absurdity, checks the range of the index, and deals
;; with negative offsets.
(define (hist-idx who idx base?)
  (let* ([len (history-length)]
         [idx (cond [(<= 0 idx (sub1 len)) idx]
                    [(<= (- len) idx -1)   (+ len idx)]
                    [else (error who "index out of history range, -~a - ~a"
                                 len (sub1 len))])])
    (if base? (+ idx (history-base)) idx)))

;; actually, returns a pointer to a struct with the string, but all we
;; care about is the string...
(define history-get
  (get-ffi-obj "history_get" libreadline
    (_fun (i) :: (_int = (hist-idx 'history-get i #t)) -> (_ptr o _string))))

(define history-remove ; returns HIST_ENTRY* that history_free() frees
  (get-ffi-obj "remove_history" libreadline
    (_fun (i) :: (_int = (hist-idx 'history-delete i #f)) -> _pointer)))
(define history-free ; ignore histdata_t return value
  (get-ffi-obj "free_history_entry" libreadline (_fun _pointer -> _void)
               ;; if not available, use free
               (lambda () free)))

(define (history-delete idx)
  (history-free (history-remove idx)))

;; Simple completion: use this with a (string -> (list-of string)) function
;; that returns the completions for a given string (can be used with other
;; input string types too, depending on the `type' argument).  Use #f to remove
;; a completion function that was previously set.
(define set-completion-function!
  (case-lambda
    [(func) (set-completion-function! func _string)]
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
        (begin (set! cur (func str)) (complete #f 1))
        (and (pair? cur)
             (let* ([cur (begin0 (car cur) (set! cur (cdr cur)))]
                    [cur (if (string? cur) (string->bytes/utf-8 cur) cur)])
               (malloc (add1 (bytes-length cur)) cur 'raw)))))
    complete))

(set-ffi-obj! "rl_readline_name" libreadline _bytes #"mzscheme")

;; need to capture the real input port below
(define real-input-port (current-input-port))
(unless (eq? 'stdin (object-name real-input-port))
  (log-warning "mzrl warning: could not capture the real input port\n"))
(unless (terminal-port? real-input-port)
  (log-warning "mzrl warning: input port is not a terminal\n"))

;; make it possible to run Scheme threads while waiting for input
(set-ffi-obj! "rl_event_hook" libreadline (_fun -> _int)
              (lambda () (sync/enable-break real-input-port) 0))
