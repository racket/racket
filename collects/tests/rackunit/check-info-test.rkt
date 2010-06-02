;;;
;;; <check-util-test.rkt> ---- Tests for check-util
;;; Time-stamp: <2009-06-11 17:03:21 noel>
;;;
;;; Copyright (C) 2003 by Noel Welsh.
;;;
;;; This file is part of RackUnit.

;;; RackUnit is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; RackUnit is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with RackUnit; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

#lang racket/base

(require rackunit
         rackunit/private/check-info)

(provide check-info-tests)

(define check-info-tests
  (test-suite "All check-info tests"
              (test-case
               "with-check-info stores value in lexical order"
               (let ((stack (with-check-info
                             (('a 1)
                              ('b 2)
                              ('c 3))
                             (check-info-stack (current-continuation-marks)))))
                 (for-each (lambda (actual expected)
                             (check-eq? (check-info-name actual)
                                        expected))
                           stack
                           (list 'a 'b 'c))))
              
              (test-case
               "Nested uses of with-check-info store values in lexical order"
               (let ((stack (with-check-info
                             (('a 1)
                              ('b 2)
                              ('c 3))
                             (with-check-info
                              (('d 4)
                               ('e 5)
                               ('f 6))
                              (check-info-stack (current-continuation-marks))))))
                 (for-each (lambda (actual expected)
                             (check-eq? (check-info-name actual)
                                        expected))
                           stack
                           (list 'a 'b 'c 'd 'e 'f))))
              
              (test-case
               "check-actual? and check-expected? work"
               (check-true (check-actual? (make-check-actual 1)))
               (check-true (check-expected? (make-check-expected 1)))
               (check-false (check-expected? (make-check-actual 1)))
               (check-false (check-expected? (make-check-actual 1))))
              
              (test-case
               "make-check-actual and make-check-expected store param"
               (check-equal? (check-info-value (make-check-actual 1)) 1)
               (check-equal? (check-info-value (make-check-expected 2)) 2))
              
              ))

