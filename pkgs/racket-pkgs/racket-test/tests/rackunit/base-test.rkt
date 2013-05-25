;;;
;;; Time-stamp: <2008-06-19 21:03:50 noel>
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

(require rackunit
         rackunit/private/base)

(provide base-tests)

(define base-tests
  (test-suite
   "All tests for base"
   (test-case
    "rackunit-test-case structure has a contract on name"
    (check-exn exn:fail?
               (lambda ()
                 (make-rackunit-test-case
                  'foo
                  (lambda () #t)))))
   (test-case
    "rackunit-test-case structure has a contract on action"
    (check-exn exn:fail?
               (lambda ()
                 (make-rackunit-test-case
                  "Name"
                  #f))))
   (test-case
    "rackunit-test-suite has a contract on its fields"
    (check-exn exn:fail?
               (lambda ()
                 (make-rackunit-test-suite
                  #f
                  (list)
                  (lambda () 3)
                  (lambda () 2))))
    (check-exn exn:fail?
               (lambda ()
                 (make-rackunit-test-suite
                  "Name"
                  #f
                  (lambda () 3)
                  (lambda () 2))))
    (check-exn exn:fail?
               (lambda ()
                 (make-rackunit-test-suite
                  "Name"
                  (list)
                  #f
                  (lambda () 2))))
    (check-exn exn:fail?
               (lambda ()
                 (make-rackunit-test-suite
                  "Name"
                  (list)
                  (lambda () 3)
                  #f))))
   ))

