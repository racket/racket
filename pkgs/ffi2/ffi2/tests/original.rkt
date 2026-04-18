#lang racket/base
(require ffi2
         ffi/unsafe/global
         racket/place
         rackunit
         "make-ffi2-lib.rkt")

(define-values (test-lib clean-ffi2-lib)
  (build-ffi2-lib))

(define-ffi2-definer define-test-procedure #:lib test-lib)

(define-ffi2-type to_double_t (int_t float_t . -> . double_t))
(define-ffi2-type to_double_in_orig_t (int_t float_t . -> . double_t #:in-original))

(define-test-procedure double_built (to_double_t . -> . double_t))
(define-test-procedure double_built_in_orig (to_double_t . -> . double_t #:in-original)
  #:c-id double_built)
(define-test-procedure double_built_in_orig_errno (to_double_t . -> . double_t #:errno #:in-original)
  #:c-id double_built)
(define-test-procedure double_built_cb_orig (to_double_in_orig_t . -> . double_t)
  #:c-id double_built)
(define-test-procedure double_built_in_orig_cb_orig (to_double_in_orig_t . -> . double_t #:in-original)
  #:c-id double_built)

(hash-set! (get-place-table) 'original-place? #t)
(define orig-thread (current-thread))
(define current-not-in-parallel-thread (make-parameter #t))

(define (check double_built)
  (check-equal? (double_built (lambda (i f)
                                (unless (and (eq? #t (hash-ref (get-place-table) 'original-place? #f))
                                             (current-not-in-parallel-thread))
                                  (printf "not in right place/thread!\n")
                                  (exit 1))
                                (+ i f 0.25))) 220.5))

(check double_built)
(check double_built_in_orig)
(check double_built_cb_orig)
(check double_built_in_orig_cb_orig)
(check (lambda (proc) (define-values (r e) (double_built_in_orig_errno proc)) r))

(thread-wait
 (thread #:pool 'own
         (lambda ()
           (when (place-enabled?)
             (current-not-in-parallel-thread #f))
           (check double_built_in_orig)
           (check double_built_cb_orig)
           (check double_built_in_orig_cb_orig)
           (check (lambda (proc) (define-values (r e) (double_built_in_orig_errno proc)) r)))))

(module run-in-place racket/base
  (require ffi2
           ffi/unsafe/global
           racket/place
           rackunit)

  (provide go)

  (define (go pch)
    (define double_built_ptr (uintptr_t->ptr_t (place-channel-get pch)))

    (define-ffi2-type to_double_t (int_t float_t . -> . double_t))
    (define-ffi2-type to_double_in_orig_t (int_t float_t . -> . double_t #:in-original))

    (define double_built_in_orig (ffi2-procedure double_built_ptr
                                                 (to_double_t . -> . double_t #:in-original)))
    (define double_built_cb_orig (ffi2-procedure double_built_ptr
                                                 (to_double_in_orig_t . -> . double_t)))
    (define double_built_in_orig_cb_orig (ffi2-procedure double_built_ptr
                                                         (to_double_in_orig_t . -> . double_t #:in-original)))
    (define (check double_built [orig-place? #t])
      (check-equal? (double_built (lambda (i f)
                                    (unless (or (not orig-place?)
                                                (eq? #t (hash-ref (get-place-table) 'original-place? #f)))
                                      (printf "not in right place!\n")
                                      (exit 1))
                                    (+ i f 0.25))) 220.5))
    (check double_built_in_orig)
    (check double_built_cb_orig #f)
    (check double_built_in_orig_cb_orig)))

(define pl (dynamic-place '(submod (lib "ffi2/tests/original.rkt") run-in-place) 'go))
(place-channel-put pl (ptr_t->uintptr_t (ffi2-lib-ref test-lib #"double_built")))
(void (place-wait pl))

(clean-ffi2-lib)
