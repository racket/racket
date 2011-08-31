#lang racket/base
(require rackunit)
(require "1/all-1-tests.rkt"
         "2/and-let-test.rkt"
         "4/srfi-4-test.rkt"
         "11/srfi-11-test.rkt"
         "13/string-test.rkt"
         "14/char-set-test.rkt"
         "26/cut-test.rkt"
         "40/all-srfi-40-tests.rkt"
         "43/all-srfi-43-tests.rkt"
         "69/hash-tests.rkt")
(provide all-srfi-tests)

(define all-srfi-tests
  (test-suite
   "all-srfi-tests"
   all-1-tests
   and-let*-tests
   string-tests
   char-set-tests
   cut-tests
   all-srfi-40-tests
   all-srfi-43-tests
   hash-tests
   srfi-4-tests
   srfi-11-tests
   ))
