;;;
;;; <cons.ss> ---- List constructors
;;; Time-stamp: <02/02/27 12:19:59 noel>
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

(module cons
  mzscheme

  (require (lib "etc.ss" "mzlib")
		   (lib "optional.ss" "srfi")
		   "selector.ss")

  (provide xcons 
		   make-list
		   list-tabulate
		   cons*
		   list-copy
		   circular-list
		   iota)

  ;; Occasionally useful as a value to be passed to a fold or other
  ;; higher-order procedure.
  (define (xcons d a) (cons a d))


  ;; Make a list of length LEN.
  
  (define make-list
	(opt-lambda (len  [elt #f])
	  (check-arg (lambda (n) (and (integer? n) (>= n 0))) len 'make-list)
	  (do ((i len (- i 1))
		   (ans '() (cons elt ans)))
		  ((<= i 0) ans))))
  

  ;; Make a list of length LEN. Elt i is (PROC i) for 0 <= i < LEN.
  
  (define (list-tabulate len proc)
	(check-arg (lambda (n) (and (integer? n) (>= n 0))) len 'list-tabulate)
	(check-arg procedure? proc 'list-tabulate)
	(do ((i (- len 1) (- i 1))
		 (ans '() (cons (proc i) ans)))
		((< i 0) ans)))
  
  ;; (cons* a1 a2 ... an) = (cons a1 (cons a2 (cons ... an)))
  ;; (cons* a1) = a1	(cons* a1 a2 ...) = (cons a1 (cons* a2 ...))
  ;;
  ;; (cons first (unfold not-pair? car cdr rest values))

  (define (cons* first . rest)
	(let recur ((x first) (rest rest))
	  (if (pair? rest)
		  (cons x (recur (car rest) (cdr rest)))
		  x)))
  

  (define (list-copy lis)				
	(let recur ((lis lis))			
	  (if (pair? lis)				
				(cons (car lis) (recur (cdr lis)))	
				lis)))					
  
  (define (circular-list val1 . vals)
	(let ((ans (cons val1 vals)))
	  (set-cdr! (last-pair ans) ans)
	  ans))


  ;; IOTA count [start step]	(start start+step ... start+(count-1)*step)
  
  (define iota
	(opt-lambda (count [start 0] [step 1])
	  (check-arg integer? count 'iota)
	  (check-arg number? start 'iota)
	  (check-arg number? step 'iota)
	  (let ((last-val (+ start (* (- count 1) step))))
		(do ((count count (- count 1))
			 (val last-val (- val step))
			 (ans '() (cons val ans)))
			((<= count 0)  ans)))))

  )

;;; cons.ss ends here