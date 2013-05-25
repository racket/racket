;;;
;;; <selector.rkt> ---- List selectors
;;; Time-stamp: <02/02/27 12:49:44 noel>
;;;
;;; Copyright (C) 2002 by Noel Welsh.
;;;
;;; This file is part of SRFI-1.

;;; This SRFI-1 implementation is distributed under the same terms as
;;; Racket.

;;; Author: Noel Welsh <noelwelsh@yahoo.com>

;; Commentary:

;; Based on the reference implementation by Olin Shiver and hence:

;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;     -Olin

;; Olin Shivers verified that he is fine with redistributing this code
;; under the LGPL.  (Verified personally by Eli Barzilay.)

#lang scheme/base

(require srfi/optional
         (only-in scheme/list take drop take-right drop-right split-at))

(provide first second
         third fourth
         fifth sixth
         seventh eighth
         ninth tenth
         car+cdr
         take drop
         take-right drop-right
         (rename-out [take take!]) (rename-out [drop-right drop-right!])
         split-at (rename-out [split-at split-at!])
         last
         last-pair)

(define first  car)
(define second cadr)
(define third  caddr)
(define fourth cadddr)
(define (fifth   x) (car    (cddddr x)))
(define (sixth   x) (cadr   (cddddr x)))
(define (seventh x) (caddr  (cddddr x)))
(define (eighth  x) (cadddr (cddddr x)))
(define (ninth   x) (car  (cddddr (cddddr x))))
(define (tenth   x) (cadr (cddddr (cddddr x))))

(define (car+cdr pair) (values (car pair) (cdr pair)))

;; take & drop

#; ; provided by scheme/list
(define (take lis k)
  (check-arg integer? k 'take)
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
        (cons (car lis)
              (recur (cdr lis) (- k 1))))))

#; ; provided by scheme/list
(define (drop lis k)
  (check-arg integer? k 'drop)
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))

#; ; lists are immutable
(define (take! lis k)
  (check-arg integer? k 'take!)
  (if (zero? k) '()
      (begin (set-cdr! (drop lis (- k 1)) '())
             lis)))

;; TAKE-RIGHT and DROP-RIGHT work by getting two pointers into the list,
;; off by K, then chasing down the list until the lead pointer falls off
;; the end.

#; ; provided by scheme/list
(define (take-right lis k)
  (check-arg integer? k 'take-right)
  (let lp ((lag lis)  (lead (drop lis k)))
    (if (pair? lead)
      (lp (cdr lag) (cdr lead))
      lag)))

#; ; provided by scheme/list
(define (drop-right lis k)
  (check-arg integer? k 'drop-right)
  (let recur ((lag lis) (lead (drop lis k)))
    (if (pair? lead)
      (cons (car lag) (recur (cdr lag) (cdr lead)))
      '())))

;; In this function, LEAD is actually K+1 ahead of LAG. This lets
;; us stop LAG one step early, in time to smash its cdr to ().
#; ; lists are immutable
(define (drop-right! lis k)
  (check-arg integer? k 'drop-right!)
  (let ((lead (drop lis k)))
    (if (pair? lead)
      (let lp ((lag lis)  (lead (cdr lead))) ; Standard case
        (if (pair? lead)
          (lp (cdr lag) (cdr lead))
          (begin (set-cdr! lag '())
                 lis)))
      '()))) ; Special case dropping everything -- no cons to side-effect.

#; ; provided by scheme/list
(define (split-at x k)
  (check-arg integer? k 'split-at)
  (let recur ((lis x) (k k))
    (if (zero? k) (values '() lis)
        (let-values ([(prefix suffix) (recur (cdr lis) (- k 1))])
          (values (cons (car lis) prefix) suffix)))))

#; ; lists are immutable
(define (split-at! x k)
  (check-arg integer? k 'split-at!)
  (if (zero? k) (values '() x)
      (let* ((prev (drop x (- k 1)))
             (suffix (cdr prev)))
        (set-cdr! prev '())
        (values x suffix))))

;; these could be reprovided from scheme/base, but they don't raise an
;; error on improper lists

(define (last lis) (car (last-pair lis)))

(define (last-pair lis)
  (check-arg pair? lis 'last-pair)
  (let lp ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (lp tail) lis))))

;;; selector.rkt ends here
