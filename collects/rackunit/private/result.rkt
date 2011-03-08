;;;
;;; Time-stamp: <2008-08-11 21:10:24 noel>
;;;
;;; Copyright (C) by Noel Welsh. 
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

(require "base.rkt"
         "test-suite.rkt")

(provide (all-defined-out))

;; foldts-test-suite :
;;   (test-suite string thunk thunk 'a -> 'a)
;;   (test-suite string thunk thunk 'a 'a -> 'a)
;;   (test-case string thunk 'a -> 'a)
;;   'a
;;   test
;;  ->
;;   'a
;;
;; Extended tree fold ala SSAX for tests.  Note that the
;; test-case/test-suite is passed to the functions so that
;; subtypes of test-case/test-suite can be differentiated,
;; allowing extensibility [This is an interesting difference
;; between OO and FP.  FP gives up extensibility on
;; functions, OO on data.  Here we want extensibility on
;; data so FP is a bit ugly].
(define (foldts-test-suite fdown fup fhere seed test)
  (cond
   ((rackunit-test-case? test)
    (fhere test
           (rackunit-test-case-name test)
           (rackunit-test-case-action test)
           seed))
   ((rackunit-test-suite? test)
    (apply-test-suite test fdown fup fhere seed))
   (else
    (raise
     (make-exn:test
      (format "foldts-test-suite: Don't know what to do with ~a.  It isn't a test case or test suite." test)
      (current-continuation-marks))))))


;; Useful in fold-test-results below
(define 2nd-arg (lambda (a b) b))

;; fold-test-results :
;;   ('b 'c ... 'a -> 'a)
;;   'a
;;   test
;;   #:run   (string (() -> any) -> 'b 'c ...)
;;   #:fdown (string 'a -> 'a)
;;   #:fup   (string 'a -> 'a)
;; ->
;;   'a
;;
;; Fold collector pre-order L-to-R depth-first over the
;; result of run.  By default these are test results, and
;; hence by default result-fn is
;;
;;   test-result 'a -> 'a
(define (fold-test-results result-fn seed test
                           #:run   [run run-test-case]
                           #:fdown [fdown 2nd-arg]
                           #:fup   [fup 2nd-arg])
  (foldts-test-suite
   (lambda (suite name before after seed)
     '(printf "into ~a\n" name)
     (before)
     (fdown name seed))
   (lambda (suite name before after seed kid-seed)
     '(printf "out of ~a\n" name)
     (after)
     (fup name kid-seed))
   (lambda (case name action seed)
     '(printf "running ~a\n" name)
     (apply result-fn
            ;; Get the values returned by run-fn into a
            ;; list and append the seed
            (append (call-with-values
                        (lambda () (run name action))
                      list)
                    (list seed))))
   seed
   test))

;; run-test-case : string thunk -> test-result
(define (run-test-case name action)
  '(printf "run-test-case running ~a ~a\n" name action)
  (with-handlers
      ([exn:test:check?
        (lambda (exn)
          (make-test-failure name exn))]
       [(lambda _ #t)
        (lambda (exn)
          (make-test-error name exn))])
    (let ((value (action)))
      (make-test-success name value))))

;; run-test : test -> (list-of test-result)
;;
;; Run test returning a tree of test-results.  Results are
;; ordered L-to-R as they occur in the tree.
(define (run-test test)
  (reverse
   (fold-test-results
    (lambda (result seed) (cons result seed))
    (list)
    test)))

