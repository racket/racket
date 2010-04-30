;;;
;;; Time-stamp: <2008-06-19 21:11:59 noel>
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

(provide (all-defined-out))

;; struct monad : any any 
(define-struct monad (value state) #:mutable)

;; compose : (monad-of 'a) ('a -> (monad-of 'b)) -> (monad-of 'b)
(define (compose comp build-comp)
  (lambda (seed0)
    (let* ((seed1 (comp seed0))
           (value (monad-value seed1)))
      ((build-comp value) seed1))))

;; compose*: (monad-of 'a) ('a -> (monad-of 'b)) ... -> (monad-of 'b)
(define (compose* monad . actions)
  (if (null? actions)
      monad
      (compose monad
               (lambda (value)
                 (apply
                  compose*
                  ((car actions) value)
                  (cdr actions))))))

;; sequence : (monad-of 'a) (monad-of 'b) -> (monad-of 'b)
(define (sequence monad-a monad-b)
  (compose monad-a (lambda (v) monad-b)))

(define (sequence* monad . monads)
  (if (null? monads)
      monad
      (sequence monad
                (apply
                 sequence*
                 (car monads)
                 (cdr monads)))))
