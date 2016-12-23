#lang racket/base
(require ffi/unsafe
         rackunit)

;; Check whether glib-logging support works right when use by
;; different threads

(define pthread_create
  (get-ffi-obj 'pthread_create #f (_fun (_ptr o _pointer) (_pointer = #f) _pointer _pointer -> _int)
               (lambda () #f)))
(define scheme_glib_log_message_test-pointer
  (get-ffi-obj 'scheme_glib_log_message_test #f _fpointer
               (lambda () #f)))
(define scheme_glib_log_message_test
  (get-ffi-obj 'scheme_glib_log_message_test #f (_fun _string -> _pointer)
               (lambda () #f)))

(when (and pthread_create
           scheme_glib_log_message_test-pointer)
  (define r (make-log-receiver (current-logger) 'warning))
  (scheme_glib_log_message_test "hello")
  (check-equal? '#(warning "test: hello" #f #f) (sync r))
  
  (check-equal? #f (sync/timeout 0 r))
  
  (define (make s)
    (define p (malloc (add1 (bytes-length s)) 'raw))
    (memcpy p s (add1 (bytes-length s)))
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
