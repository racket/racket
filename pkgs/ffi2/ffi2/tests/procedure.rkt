#lang racket/base
(require ffi2
         rackunit
         "make-ffi2-lib.rkt")

(define-values (test-lib clean-ffi2-lib)
  (build-ffi2-lib))

(define-ffi2-abi usual_abi default_abi)

;; ----------------------------------------

(define-ffi2-procedure double_sum (double_t double_t . -> . double_t
                                            #:abi usual_abi)
  #:lib test-lib)

(check-equal? (double_sum 3.25 4.0) 7.25)

(define-ffi2-type int_proc_t (int_t int_t . -> . int_t))

(define-ffi2-procedure int_sum int_proc_t
  #:lib test-lib)
(check-equal? (int_sum 3 4) 7)

(define also_int_sum (ffi2-procedure (ffi2-lib-ref test-lib 'int_sum) int_proc_t))
(check-equal? (also_int_sum 30 4) 34)

(define-ffi2-procedure one_more_int_sum int_proc_t
  #:lib  test-lib
  #:c-id int_sum)
(check-equal? (one_more_int_sum 30 5) 35)

;; ----------------------------------------

(clean-ffi2-lib)
