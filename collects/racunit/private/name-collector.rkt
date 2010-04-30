;;;
;;; Time-stamp: <2008-06-19 22:16:19 noel>
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
         "hash-monad.rkt"
         srfi/1)

(provide display-test-case-name
         push-suite-name!
         pop-suite-name!
         put-initial-name)

(define key (gensym))

;; put-initial-name : () -> (hash-monad-of void)
(define (put-initial-name)
  (put key null))

;; display-test-case-name : test-result -> (hash-monad-of void)
(define (display-test-case-name result)
  (compose
   (get key)
   (lambda (names)
     (cond
      ((test-success? result) (return-hash (void)))
      (else
       (fold-right
        (lambda (name seed)
          (printf "~a > " name))
        (void)
        names)
       (display (test-result-test-case-name result))
       (newline)
       (return-hash (void)))))))

;; push-suite-name! : string -> (hash-monad-of void)
(define (push-suite-name! name)
  (compose
   (get key)
   (lambda (names)
     (put key (cons name names)))))

;; pop-suite-name! :  -> (hash-monad-of void)
(define (pop-suite-name!)
  (compose
   (get key)
   (lambda (names)
     (put key (cdr names)))))

