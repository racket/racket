;;;
;;; <selector.ss> ---- List selectors
;;; Time-stamp: <02/02/27 12:49:44 noel>
;;;
;;; Copyright (C) 2002 by Noel Welsh. 
;;;
;;; This file is part of SRFI-1.

;;; SRFI-1 is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; SRFI-1 is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with SRFI-1; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

;; Based on the reference implementation by Olin Shiver and hence:

;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;     -Olin

(module selector
  mzscheme

  (require (lib "optional.ss" "srfi"))
  (require (lib "receive.ss" "srfi" "8"))

  (provide
   first second
   third fourth
   fifth sixth
   seventh eighth
   ninth tenth
   car+cdr
   take drop
   take-right drop-right
   take! drop-right!
   split-at split-at!
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
  
  (define (take lis k)
	(check-arg integer? k 'take)
	(let recur ((lis lis) (k k))
	  (if (zero? k) '()
		  (cons (car lis)
				(recur (cdr lis) (- k 1))))))
  
  (define (drop lis k)
	(check-arg integer? k 'drop)
	(let iter ((lis lis) (k k))
	  (if (zero? k) lis (iter (cdr lis) (- k 1)))))
  
  (define (take! lis k)
	(check-arg integer? k 'take!)
	(if (zero? k) '()
		(begin (set-cdr! (drop lis (- k 1)) '())
			   lis)))
  
  ;; TAKE-RIGHT and DROP-RIGHT work by getting two pointers into the list, 
  ;; off by K, then chasing down the list until the lead pointer falls off
  ;; the end.
  
  (define (take-right lis k)
	(check-arg integer? k 'take-right)
	(let lp ((lag lis)  (lead (drop lis k)))
	  (if (pair? lead)
		  (lp (cdr lag) (cdr lead))
		  lag)))
  
  (define (drop-right lis k)
	(check-arg integer? k 'drop-right)
	(let recur ((lag lis) (lead (drop lis k)))
	  (if (pair? lead)
		  (cons (car lag) (recur (cdr lag) (cdr lead)))
		  '())))
  
  ;; In this function, LEAD is actually K+1 ahead of LAG. This lets
  ;; us stop LAG one step early, in time to smash its cdr to ().
  (define (drop-right! lis k)
	(check-arg integer? k 'drop-right!)
	(let ((lead (drop lis k)))
	  (if (pair? lead)
		  
		  (let lp ((lag lis)  (lead (cdr lead)))	; Standard case
			(if (pair? lead)
				(lp (cdr lag) (cdr lead))
				(begin (set-cdr! lag '())
					   lis)))
		  
		  '())))	; Special case dropping everything -- no cons to side-effect.
  
  (define (split-at x k)
	(check-arg integer? k 'split-at)
	(let recur ((lis x) (k k))
	  (if (zero? k) (values '() lis)
		  (receive (prefix suffix) (recur (cdr lis) (- k 1))
				   (values (cons (car lis) prefix) suffix)))))
  
  (define (split-at! x k)
	(check-arg integer? k 'split-at!)
	(if (zero? k) (values '() x)
		(let* ((prev (drop x (- k 1)))
			   (suffix (cdr prev)))
		  (set-cdr! prev '())
		  (values x suffix))))
  
  
  (define (last lis) (car (last-pair lis)))
  
  (define (last-pair lis)
	(check-arg pair? lis 'last-pair)
	(let lp ((lis lis))
	  (let ((tail (cdr lis)))
		(if (pair? tail) (lp tail) lis))))
  
  
  )
;;; selector.ss ends here