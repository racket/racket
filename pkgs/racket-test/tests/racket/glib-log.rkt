#lang racket/base
(require ffi/unsafe
         ffi/unsafe/os-thread
         ffi/unsafe/vm
         rackunit)

;; Check whether glib-logging support works right when use by
;; different threads

(define-values (pthread_create
                scheme_glib_log_message_test-pointer
                scheme_glib_log_message_test)
  (case (system-type 'vm)
    [(racket)
     (values
      (get-ffi-obj 'pthread_create #f (_fun (_ptr o _pointer) (_pointer = #f) _pointer _pointer -> _int)
                   (lambda () #f))
      (get-ffi-obj 'scheme_glib_log_message_test #f _fpointer
                   (lambda () #f))
      (get-ffi-obj 'scheme_glib_log_message_test #f (_fun _string -> _pointer)
                   (lambda () #f)))]
    [(chez-scheme)
     (define scheme_glib_log_message_test
       (let ([glib-log-message (cast (vm-primitive 'glib-log-message)
                                     _intptr
                                     (_fun _bytes/nul-terminated _int _bytes/nul-terminated -> _void))]
             [warning (arithmetic-shift 1 4)])
         (lambda (msg)
           (cond
             [(not msg)
              (glib-log-message #f warning #"test")]
             [else
              (let loop ([bstr (if (string? msg) (string->bytes/utf-8 msg) (cast msg _pointer _bytes))])
                (define m (regexp-match-positions #rx";" bstr))
                (cond
                  [(not m)
                   (glib-log-message #"test" warning bstr)]
                  [else
                   (loop (subbytes bstr 0 (caar m)))
                   (loop (subbytes bstr (cdar m)))]))]))))
     (cond
       ;; prefer C library `pthread_create`, because that makes the thread more foreign to Chez Scheme:
       [(get-ffi-obj 'pthread_create #f (_fun (_ptr o _pointer) (_pointer = #f) _intptr _pointer -> _int)
                     (lambda () #f))
        => (lambda (pthread_create)
             (values
              pthread_create
              (vm-eval
               ;; We have to drop down to Chez Scheme here, because `__collect-safe`
               ;; is accessible from Racket only via `#:async-apply`, and that ends up
               ;; testing the FFI instead of a part of glib logging.
               `(let ([callable (foreign-callable __collect_safe
                                                  ',scheme_glib_log_message_test
                                                  (string)
                                                  void)])
                  (lock-object callable)
                  (foreign-callable-entry-point callable)))
              scheme_glib_log_message_test))]
       [else
        (values
         (lambda (proc arg)
           (call-in-os-thread (lambda () (proc arg))))
         ;; scheme_glib_log_message_test-pointer
         scheme_glib_log_message_test
         ;; scheme_glib_log_message_test
         scheme_glib_log_message_test)])]
    [else
     (values #f #f #f)]))
    
(when (and pthread_create
           scheme_glib_log_message_test-pointer)
  (define r (make-log-receiver (current-logger) 'warning))
  (scheme_glib_log_message_test "hello")
  (check-equal? '#(warning "test: hello" #f #f) (sync r))
  
  (check-equal? #f (sync/timeout 0 r))
  
  (scheme_glib_log_message_test #f)
  (check-equal? '#(warning "test" #f #f) (sync r))  
  (check-equal? #f (sync/timeout 0 r))
  
  (define (make s)
    (define p (malloc (add1 (bytes-length s)) 'raw))
    (memcpy p s (bytes-length s))
    (ptr-set! p _byte (bytes-length s) 0)
    p)
  
  (define hello (make #"hello"))
  (pthread_create scheme_glib_log_message_test-pointer hello)
  (check-equal? '#(warning "test: hello" #f #f) (sync r))
  
  ;; Make sure it's a queue (preserves order):
  (define seq (make #"first;middle;last"))
  (pthread_create scheme_glib_log_message_test-pointer seq)
  (check-equal? '#(warning "test: first" #f #f) (sync r))
  (check-equal? '#(warning "test: middle" #f #f) (sync r))
  (check-equal? '#(warning "test: last" #f #f) (sync r))
  
  (printf "Used: ~s\n" (list hello seq))
  
  (printf "glib logging support ok\n"))
