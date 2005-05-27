;;;
;;; <predicate.ss> ---- List Predicates
;;; Time-stamp: <02/02/27 12:57:15 noel>
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


(module predicate
  mzscheme

  (require (lib "optional.ss" "srfi"))

  (provide pair?
		   null?
		   proper-list?
		   circular-list?
		   dotted-list?
		   not-pair?
		   null-list?
		   list=)

  ;; <proper-list> ::= ()			; Empty proper list
  ;;		  |   (cons <x> <proper-list>)	; Proper-list pair
  ;; Note that this definition rules out circular lists -- and this
  ;; function is required to detect this case and return false.
  
  (define (proper-list? x)
	(let lp ((x x) (lag x))
	  (if (pair? x)
		  (let ((x (cdr x)))
			(if (pair? x)
				(let ((x   (cdr x))
					  (lag (cdr lag)))
				  (and (not (eq? x lag)) (lp x lag)))
				(null? x)))
		  (null? x))))
  
  
  ;; A dotted list is a finite list (possibly of length 0) terminated
  ;; by a non-nil value. Any non-cons, non-nil value (e.g., "foo" or 5)
  ;; is a dotted list of length 0.
  ;;
  ;; <dotted-list> ::= <non-nil,non-pair>	; Empty dotted list
  ;;               |   (cons <x> <dotted-list>)	; Proper-list pair
  
  (define (dotted-list? x)
	(let lp ((x x) (lag x))
	  (if (pair? x)
		  (let ((x (cdr x)))
			(if (pair? x)
				(let ((x   (cdr x))
					  (lag (cdr lag)))
				  (and (not (eq? x lag)) (lp x lag)))
				(not (null? x))))
		  (not (null? x)))))
  
  (define (circular-list? x)
	(let lp ((x x) (lag x))
	  (and (pair? x)
		   (let ((x (cdr x)))
			 (and (pair? x)
				  (let ((x   (cdr x))
						(lag (cdr lag)))
					(or (eq? x lag) (lp x lag))))))))
  
  (define (not-pair? x) (not (pair? x)))	; Inline me.
  
  ;; This is a legal definition which is fast and sloppy:
  ;;     (define null-list? not-pair?)
  ;; but we'll provide a more careful one:
  (define (null-list? l)
	(cond ((pair? l) #f)
		  ((null? l) #t)
		  (else (error "null-list?: argument out of domain" l))))
  
  
  (define (list= = . lists)
	(or (null? lists) ; special case
		
		(let lp1 ((list-a (car lists)) (others (cdr lists)))
		  (or (null? others)
			  (let ((list-b (car others))
					(others (cdr others)))
				(if (eq? list-a list-b)	; EQ? => LIST=
					(lp1 list-b others)
					(let lp2 ((list-a list-a) (list-b list-b))
					  (if (null-list? list-a)
						  (and (null-list? list-b)
							   (lp1 list-b others))
						  (and (not (null-list? list-b))
							   (= (car list-a) (car list-b))
							   (lp2 (cdr list-a) (cdr list-b)))))))))))
  
  
  
  )
  
;;; predicate.ss ends here