;;;
;;; Time-stamp: <2008-06-19 21:05:15 noel>
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

(require racket/match
         rackunit
         rackunit/private/counter
         rackunit/private/monad
         rackunit/private/hash-monad)

(provide counter-tests)

(define a-success (make-test-success "success" #t))
(define an-error (make-test-error "error" #f))
(define a-failure (make-test-failure "failure" #f))

(define counter-tests
  (test-suite
   "All tests for counter"
   (test-case
    "counter->vector is correct"
    (let ((monad ((put-initial-counter) (make-empty-hash))))
      ((compose
        (sequence* (update-counter! a-success)
                   (update-counter! a-failure)
                   (update-counter! an-error)
                   (counter->vector))
        (match-lambda
         ((vector s f e)
          (check = s 1)
          (check = f 1)
          (check = e 1)
          (return-hash (void)))))
       monad)))
   ))

