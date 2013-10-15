;;;
;;; Time-stamp: <2008-06-19 21:06:21 noel>
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
         rackunit/private/monad
         rackunit/private/hash-monad)

(provide hash-monad-tests)

(define hash-monad-tests
  (test-suite
   "All tests for hash-monad"
   
   (test-case
    "Gets retrieves puts"
    ((compose*
      (put 'foo 10)
      (lambda (v)
        (get 'foo))
      (lambda (v)
        (check = v 10)
        (return-hash #t)))
     (make-empty-hash)))
   
   (test-case
    "Get raises exception when no value exists"
    (check-exn
     exn:fail:contract?
     (lambda ()
       ((compose
         (get 'foo)
         (lambda (v)
           (fail "Should not be executed")))
        (make-empty-hash)))))
   
   (test-case
    "Put overwrites existing entries"
    ((compose*
      (put 'foo 10)
      (lambda (v)
        (get 'foo))
      (lambda (v)
        (check = v 10)
        (put 'foo 20))
      (lambda (v)
        (get 'foo))
      (lambda (v)
        (check = v 20)
        (get 'foo)))
     (make-empty-hash)))
   
   ))

