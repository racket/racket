;;;
;;; Time-stamp: <2008-06-19 21:14:02 noel>
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

(require "base.rkt"
         "monad.rkt"
         "hash-monad.rkt")

(provide display-counter
         update-counter!
         put-initial-counter
         counter->vector)

(define key (gensym))

;; struct counter : integer integer integer
(define-struct counter (successes failures errors) #:mutable)

;; display-counter : () -> (hash-monad-of ())
(define (display-counter)
  (compose
   (get key)
   (lambda (counter)
     (let ((s (counter-successes counter))
           (f (counter-failures counter))
           (e (counter-errors counter)))
       (display s) (display " success(es) ")
       (display f) (display " failure(s) ")
       (display e) (display " error(s) ")
       (display (+ s f e)) (display " test(s) run")
       (newline)
       (return-hash (void))))))

;; counter->vector : () -> (hash-monad-of vector)
(define (counter->vector)
  (compose
   (get key)
   (lambda (counter)
     (return-hash
      (vector (counter-successes counter)
              (counter-failures counter)
              (counter-errors counter))))))

;; update-counter! : test-result -> (hash-monad-of ())
(define (update-counter! result)
  (define (add-success! counter)
    (set-counter-successes! counter
                            (add1 (counter-successes counter))))
  (define (add-failure! counter)
    (set-counter-failures! counter
                           (add1 (counter-failures counter))))
  (define (add-error! counter)
    (set-counter-errors! counter
                         (add1 (counter-errors counter))))
  (compose
   (get key)
   (lambda (counter)
     (cond
      ((test-error? result)
       (add-error! counter))
      ((test-failure? result)
       (add-failure! counter))
      (else
       (add-success! counter)))
     (put key counter))))

;; put-initial-counter : () -> (hash-monad-of ())
(define (put-initial-counter)
  (put key (make-counter 0 0 0)))
