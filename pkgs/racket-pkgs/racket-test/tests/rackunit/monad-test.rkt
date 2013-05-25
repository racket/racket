;;;
;;; Time-stamp: <2008-06-19 21:07:07 noel>
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
         rackunit/private/monad)

(provide monad-tests)

;; A simple monad we'll use to thread a counter through
;; code
(define (counter-monad v s)
  (make-monad v s))

(define (return-counter value)
  (lambda (counter)
    (counter-monad value
                   (monad-state counter))))

(define (increment-counter)
  (lambda (counter)
    (set-monad-state! counter
                      (add1 (monad-state counter)))
    (set-monad-value! counter (void))
    counter))

(define (set-counter s)
  (lambda (counter)
    (set-monad-state! counter s)
    (set-monad-value! counter (void))
    counter))

(define (get-counter)
  (lambda (counter)
    (set-monad-value! counter
                      (monad-state counter))
    counter))

(define monad-tests
  (test-suite
   "All tests for monad"
   
   (test-case
    "compose threads state"
    (let ((m ((compose (get-counter)
                       (lambda (c)
                         (check = c 0)
                         (increment-counter)))
              (counter-monad (void) 0))))
      (check = (monad-state m) 1)))
   
   (test-case
    "compose* threads state"
    (let ((m
           ((compose* (get-counter)
                      (lambda (c)
                        (check = c 0)
                        (increment-counter))
                      (lambda (r)
                        (check-pred void? r)
                        (increment-counter))
                      (lambda (r)
                        (check-pred void? r)
                        (get-counter))
                      (lambda (c)
                        (check = c 2)
                        (return-counter #t)))
            (counter-monad (void) 0))))
      (check = 2 (monad-state m))
      (check-true (monad-value m))))
   
   (test-case
    "sequence threads state"
    (let ((m ((sequence (increment-counter)
                        (increment-counter))
              (counter-monad (void) 0))))
      (check = 2 (monad-state m))
      (check-pred void? (monad-value m))))
   
   (test-case
    "sequence* threads state"
    (let ((m ((sequence* (increment-counter)
                         (increment-counter)
                         (increment-counter)
                         (increment-counter))
              (counter-monad (void) 0))))
      (check = 4 (monad-state m))
      (check-pred void? (monad-value m))))
   
   (test-case
    "sequence* executes in correct order"
    (let ((m ((sequence* (set-counter 0)
                         (set-counter 1)
                         (set-counter 2)
                         (set-counter 4))
              (counter-monad (void) 0))))
      (check = 4 (monad-state m))
      (check-pred void? (monad-value m))))
   
   ))

