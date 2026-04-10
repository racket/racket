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

(let ()
  (define-test double_sum ([a : double_t]
                           #:do [(define a2 (list a))]
                           [b : double_t = (* 10 (car a2))]
                           #:do [(define b2 (list b))]
                           . -> . double_t
                           #:result (vector a a2 b b2)))
  (check-equal? (double_sum 3.0) (vector 3.0 (list 3.0) 30.0 (list 30.0))))

(let ()
  (define-test double_sum (#:do [(define-syntax-rule (top) 'ok)]
                           double_t
                           #:do [(define-syntax-rule (middle) (list (top)))]
                           #:do [(define extra 'extra)]
                           double_t
                           #:do [(define-syntax-rule (bottom) (list (top) (middle)))]
                           . -> . double_t
                           #:result (list (top) (middle) extra (bottom))))
  (check-equal? (double_sum 3.0 4.0) (list 'ok (list 'ok) 'extra (list 'ok (list 'ok)))))

;; ----------------------------------------

(let ()
  (define-test ptr_to_ptr ((array double_t *) . -> . ptr_t))
  (define a (ffi2-malloc double_t))
  (check-equal? (ptr_to_ptr a) a)
  (check-exn exn:fail:contract? (lambda () (ptr_to_ptr ptr_to_ptr))))

(let ()
  (define-test ptr_to_ptr ((array (array double_t *) *) . -> . ptr_t))
  (define a (ffi2-malloc (array double_t *)))
  (check-equal? (ptr_to_ptr a) a)
  (check-exn (lambda (exn)
               (and (exn:fail:contract? exn)
                    (regexp-match? "double_t[*][*]" (exn-message exn))))
             (lambda () (ptr_to_ptr (ffi2-malloc double_t)))))

;; ----------------------------------------

(clean-ffi2-lib)
