;;;
;;; <util.ss> ---- Utility functions
;;; Time-stamp: <02/02/28 12:05:00 noel>
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

(module util
  mzscheme

  (require (lib "optional.ss" "srfi")
		   "predicate.ss"
		   "selector.ss")
  (require (lib "receive.ss" "srfi" "8"))

  (provide %cdrs
		   %cars+
		   %cars+cdrs
		   %cars+cdrs+
		   %cars+cdrs/no-test)

  ;; Fold/map internal utilities
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; These little internal utilities are used by the general
  ;; fold & mapper funs for the n-ary cases . It'd be nice if they got inlined.
  ;; One the other hand, the n-ary cases are painfully inefficient as it is.
  ;; An aggressive implementation should simply re-write these functions 
  ;; for raw efficiency; I have written them for as much clarity, portability,
  ;; and simplicity as can be achieved.
  ;;
  ;; I use the dreaded call/cc to do local aborts. A good compiler could
  ;; handle this with extreme efficiency. An implementation that provides
  ;; a one-shot, non-persistent continuation grabber could help the compiler
  ;; out by using that in place of the call/cc's in these routines.
  ;;
  ;; These functions have funky definitions that are precisely tuned to
  ;; the needs of the fold/map procs -- for example, to minimize the number
  ;; of times the argument lists need to be examined.
  
  ;; Return (map cdr lists). 
  ;; However, if any element of LISTS is empty, just abort and return '().
  (define (%cdrs lists)
	(call-with-current-continuation
	 (lambda (abort)
	   (let recur ((lists lists))
		 (if (pair? lists)
			 (let ((lis (car lists)))
			   (if (null-list? lis) (abort '())
				   (cons (cdr lis) (recur (cdr lists)))))
			 '())))))
  
  (define (%cars+ lists last-elt)	; (append! (map car lists) (list last-elt))
	(let recur ((lists lists))
	  (if (pair? lists) (cons (caar lists) (recur (cdr lists))) (list last-elt))))
  
  ;; LISTS is a (not very long) non-empty list of lists.
  ;; Return two lists: the cars & the cdrs of the lists.
  ;; However, if any of the lists is empty, just abort and return [() ()].
  
  (define (%cars+cdrs lists)
	(call-with-current-continuation
	 (lambda (abort)
	   (let recur ((lists lists))
		 (if (pair? lists)
			 (receive (list other-lists) (car+cdr lists)
					  (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
						  (receive (a d) (car+cdr list)
								   (receive (cars cdrs) (recur other-lists)
											(values (cons a cars) (cons d cdrs))))))
			 (values '() '()))))))
  
  ;; Like %CARS+CDRS, but we pass in a final elt tacked onto the end of the
  ;; cars list. What a hack.
  (define (%cars+cdrs+ lists cars-final)
	(call-with-current-continuation
	 (lambda (abort)
	   (let recur ((lists lists))
		 (if (pair? lists)
			 (receive (list other-lists) (car+cdr lists)
					  (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
						  (receive (a d) (car+cdr list)
								   (receive (cars cdrs) (recur other-lists)
											(values (cons a cars) (cons d cdrs))))))
			 (values (list cars-final) '()))))))
  
  ;; Like %CARS+CDRS, but blow up if any list is empty.
  (define (%cars+cdrs/no-test lists)
	(let recur ((lists lists))
	  (if (pair? lists)
		  (receive (list other-lists) (car+cdr lists)
				   (receive (a d) (car+cdr list)
							(receive (cars cdrs) (recur other-lists)
									 (values (cons a cars) (cons d cdrs)))))
		  (values '() '()))))
  
  )

;;; util.ss ends here