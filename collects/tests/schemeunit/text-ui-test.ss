;;;
;;; Time-stamp: <2010-03-29 13:56:54 noel>
;;;
;;; Copyright (C) 2005 by Noel Welsh.
;;;

;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser
;;; General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.

;;; This library is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.

;;; You should have received a copy of the GNU Lesser
;;; General Public License along with this library; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

#lang scheme/base

(require scheme/runtime-path
         srfi/1
         srfi/13
         schemeunit
         schemeunit/text-ui)

(provide text-ui-tests)


;; Reimplement with-output-to-string to avoid dependency on
;; io.plt, which in turn depends on SchemeUnit 1.2, which
;; has not been ported to PLT 4.
(define-syntax with-output-to-string
  (syntax-rules ()
    [(with-output-to-string expr ...)
     (let ([p (open-output-string)])
       (parameterize ([current-output-port p])
         expr ...)
       (get-output-string p))]))

(define-syntax with-error-to-string
  (syntax-rules ()
    [(with-error-to-string expr ...)
     (let ([p (open-output-string)])
       (parameterize ([current-error-port p])
         expr ...)
       (get-output-string p))]))

(define-runtime-path here ".")

;; with-silent-output (() -> any) -> any
(define (with-silent-output thunk)
  (let ([out  (open-output-string)]
        [err (open-output-string)])
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (thunk))))

(define (failing-test)
  (run-tests
   (test-suite
    "Dummy"
    (test-case "Dummy" (check-equal? 1 2)))))

(define (failing-binary-test/complex-params)
  (run-tests
   (test-suite
    "Dummy"
    (test-case "Dummy"
               (check-equal?
                (list (iota 15) (iota 15) (iota 15))
                1)))))

(define (failing-test/complex-params)
  (run-tests
   (test-suite
    "Dummy"
    (test-case "Dummy"
               (check-false
                (list (iota 15) (iota 15) (iota 15)))))))

(define (quiet-failing-test)
  (run-tests
   (test-suite
    "Dummy"
    (test-case "Dummy" (check-equal? 1 2)))
   'quiet))

(define (quiet-error-test)
  (run-tests
   (test-suite
    "Dummy"
    (test-case "Dummy" (error "kabloom!")))
   'quiet))

(define text-ui-tests
  (test-suite
   "All tests for text-ui"
   
   (test-case
    "Binary check displays actual and expected in failure error message"
    (let ((op (with-error-to-string (failing-test))))
      (check string-contains
             op
             "expected")
      (check string-contains
             op
             "actual")))
   
   (test-case
    "Binary check doesn't display params"
    (let ((op (with-error-to-string (failing-test))))
      (check (lambda (out str) (not (string-contains out str)))
             op
             "params")))
   
   (test-case
    "Binary check output is pretty printed"
    (let ([op (with-error-to-string (failing-binary-test/complex-params))])
      (check string-contains
             op
             "((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
 (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
 (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))")))
   
   (test-case
    "Non-binary check output is pretty printed"
    (let ([op (with-error-to-string (failing-test/complex-params))])
      (check string-contains
             op
             "((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
 (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
 (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))")))
   
   (test-case
    "Location trimmed when file is under current directory"
    (parameterize ((current-directory here))
      (let ((op (with-error-to-string (failing-test))))
        (check string-contains
               op
               "location:   text-ui-test.ss"))))
   
   (test-case
    "Name and location displayed before actual/expected"
    (let ((op (with-error-to-string (failing-test))))
      (let ((name-idx (string-contains op "name:"))
            (loc-idx (string-contains op "location:"))
            (actual-idx (string-contains op "actual:"))
            (expected-idx (string-contains op "expected:")))
        (check < name-idx loc-idx)
        (check < loc-idx actual-idx)
        (check < actual-idx expected-idx))))
   
   (test-case
    "Quiet mode is quiet"
    (let ((op1 (with-error-to-string (quiet-failing-test)))
          (op2 (with-error-to-string (quiet-error-test))))
      (check string=? op1 "")
      (check string=? op2 "")))

   (test-case
    "Number of unsuccessful tests returned"
    (check-equal? (with-silent-output failing-test) 1)
    (check-equal? (with-silent-output quiet-failing-test) 1)
    (check-equal? (with-silent-output quiet-error-test) 1)
    (check-equal? (with-silent-output
                   (lambda ()
                     (run-tests
                      (test-suite
                       "Dummy"
                       (test-case "Dummy" (check-equal? 1 1)))
                      'quiet)))
                  0))
   
   (test-case
    "run-tests runs suite before/after actions in quiet mode"
    (let ([foo 1])
      (run-tests
       (test-suite
        "Foo"
        #:before (lambda () (set! foo 2))
        #:after (lambda () (set! foo 3))
        (test-case
         "Foo check"
         (check = foo 2)))
       'quiet)
      (check = foo 3)))
   
   (test-case
    "run-tests runs suite before/after actions in normal mode"
    (let ([foo 1])
      (run-tests
       (test-suite
        "Foo"
        #:before (lambda () (set! foo 2))
        #:after (lambda () (set! foo 3))
        (test-case
         "Foo check"
         (check = foo 2)))
       'normal)
      (check = foo 3)))
   
   (test-case
    "run-tests runs suite before/after actions in verbose mode"
    (let ([foo 1])
      (run-tests
       (test-suite
        "Foo"
        #:before (lambda () (set! foo 2))
        #:after (lambda () (set! foo 3))
        (test-case
         "Foo check"
         (check = foo 2)))
       'verbose)
      (check = foo 3)))
   ))

