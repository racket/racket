;;;
;;; Time-stamp: <2008-06-19 21:13:49 noel>
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

(require "monad.rkt")

(provide (all-defined-out))

(define (make-empty-hash)
  (make-monad (void) (make-hash)))

(define (return-hash value)
  (lambda (hash)
    (set-monad-value! hash value)
    hash))

(define (put key val)
  (lambda (hash)
    (hash-set! (monad-state hash) key val)
    hash))

(define (get key)
  (lambda (hash)
    (let ((val (hash-ref (monad-state hash) key)))
      (set-monad-value! hash val)
      hash)))

