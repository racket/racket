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

#lang racket/base

(require racket/runtime-path
         racket/pretty
         racket/port
         srfi/1
         srfi/13
         rackunit
         rackunit/text-ui)

(provide text-ui-tests)

(define-syntax-rule (with-all-output-to-string e ...)
  (with-all-output-to-string* (lambda () e ...)))

(define (with-all-output-to-string* thnk)
  (with-output-to-string
      (lambda ()
        (parameterize ([current-error-port (current-output-port)])
          (thnk)))))

(define-runtime-path here ".")

;; with-silent-output (() -> any) -> any
(define (with-silent-output thunk)
  (parameterize ([current-output-port (open-output-nowhere)]
                 [current-error-port (open-output-nowhere)])
    (thunk)))

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
    (let ((op (with-all-output-to-string (failing-test))))
      (check string-contains
             op
             "expected")
      (check string-contains
             op
             "actual")))
   
   (test-case
    "Binary check doesn't display params"
    (let ((op (with-all-output-to-string (failing-test))))
      (check (lambda (out str) (not (string-contains out str)))
             op
             "params")))
   
   (test-case
    "Binary check output is pretty printed"
    (let ([op (parameterize ([pretty-print-columns 80])
                (with-all-output-to-string (failing-binary-test/complex-params)))])
      (check string-contains
             op
             "'((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
  (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
  (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))")))
   
   (test-case
    "Non-binary check output is pretty printed"
    (let ([op (parameterize ([pretty-print-columns 80])
                (with-all-output-to-string (failing-test/complex-params)))])
      (check string-contains
             op
             "'((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
  (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
  (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))")))
   
   (test-case
    "Location trimmed when file is under current directory"
    (parameterize ((current-directory here))
      (let ((op (with-all-output-to-string (failing-test))))
        (check string-contains
               op
               "location:   text-ui-test.rkt"))))
   
   (test-case
    "Name and location displayed before actual/expected"
    (let ((op (with-all-output-to-string (failing-test))))
      (let ((name-idx (string-contains op "name:"))
            (loc-idx (string-contains op "location:"))
            (actual-idx (string-contains op "actual:"))
            (expected-idx (string-contains op "expected:")))
        (check < name-idx loc-idx)
        (check < loc-idx actual-idx)
        (check < actual-idx expected-idx))))
   
   (test-case
    "Quiet mode is quiet"
    (let ((op1 (with-all-output-to-string (quiet-failing-test)))
          (op2 (with-all-output-to-string (quiet-error-test))))
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
    (with-silent-output
        (λ ()
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
            (check = foo 3)))))
   
   (test-case
    "run-tests runs suite before/after actions in normal mode"
    (with-silent-output
        (λ ()
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
            (check = foo 3)))))
   
   (test-case
    "run-tests runs suite before/after actions in verbose mode"
    (with-silent-output
        (λ ()
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
            (check = foo 3)))))

   (test-case
    "cannot kill current thread in test case"
    (check-equal? (call-in-nested-thread
                   (lambda ()
                     (with-silent-output
                      (lambda ()
                        (run-tests
                         (test-suite "tests"
                           (test-case "kill-thread"
                             (kill-thread (current-thread)))))))))
                  ;; If the kill-thread were successful, call-in-nested-thread
                  ;; would raise error. We expect kill-thread to raise error,
                  ;; caught by run-tests.
                  1))
   ))
