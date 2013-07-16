#lang racket/base

(require tests/eli-tester (for-syntax racket/base))

;; All current uses of the errortrace collection (there are currently 4:
;; in the sandbox, drracket, and in htdp and deinprogramm) rely on
;; making "self-modifying" code by using a single mpair in source code.
;; This is a fragile hole, and it might be plugged at some point in the
;; future.  (Another alternative is to inject a closure as a value into
;; the syntax, which is similarly hackish.)  The purpose of this test is
;; to fail when that happens, so we remember to deal with it in these
;; uses.  A convenient commit to refer to is 2189957 -- it touches all
;; four places.  (BTW, note that this file cannot be compiled because of
;; this.)

(define-syntax m
  (let ([b (mcons 0 0)])
    (lambda (stx)
      (with-syntax ([b b])
        #'(lambda () (set-mcar! b (add1 (mcar b))) (mcar b))))))

(provide alert-tests)
(define (alert-tests)
  (define f m)
  (test (list (f) (f) (f)) => '(1 2 3)))
