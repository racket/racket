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

(define-ffi2-definer define-test
  #:lib test-lib)

(let ()
  (define-test double_sum ([double_t = 1.0] [double_t = 10.0] . -> . double_t))
  (check-equal? (double_sum) 11.0))

(let ()
  (define-test double_sum (double_t double_t . -> . [r : double_t]
                                    #:result (list r)))
  (check-equal? (double_sum 2.0 9.0) (list 11.0)))

(let ()
  (define-test double_sum ([a : double_t] [double_t = (* 10 a)] . -> . [r : double_t]
                                          #:result (list a r)))
  (check-equal? (double_sum 3.0) (list 3.0 33.0)))

(let ()
  (define-test double_sum ([double_t = 1.0] [b : double_t]
                                            . -> .
                                            [r : double_t] [e : #:errno]
                           #:result (list b r (integer? e))))
  (check-equal? (double_sum 3.0) (list 3.0 4.0 #t)))

(let ()
  (define-test double_sum ([a : double_t] [b : double_t = (* 10 a)] . -> . double_t
                                          #:result b))
  (check-equal? (double_sum 3.0) 30.0))

;; ----------------------------------------

(clean-ffi2-lib)
