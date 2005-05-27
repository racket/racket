;;;
;;; <delete.ss> ---- List deletion functions
;;; Time-stamp: <02/03/01 07:26:12 noel>
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

(module delete
  mzscheme

  (require (lib "etc.ss" "mzlib")
		   (lib "optional.ss" "srfi")
		   "predicate.ss"
		   "filter.ss")

  (provide delete
		   delete!
		   delete-duplicates
		   delete-duplicates!)

  (define delete
	(opt-lambda (x lis (maybe-= equal?))
				(let ((= maybe-=))
				  (filter (lambda (y) (not (= x y))) lis))))

  (define delete!
	(opt-lambda (x lis  (maybe-= equal?))
				(let ((= maybe-=))
				  (filter! (lambda (y) (not (= x y))) lis))))

  ;; right-duplicate deletion
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; delete-duplicates delete-duplicates!
  ;;
  ;; Beware -- these are N^2 algorithms. To efficiently remove duplicates
  ;; in long lists, sort the list to bring duplicates together, then use a 
  ;; linear-time algorithm to kill the dups. Or use an algorithm based on
  ;; element-marking. The former gives you O(n lg n), the latter is linear.

  (define delete-duplicates
	(opt-lambda (lis (maybe-= equal?))
				(let ((elt= maybe-=))
				  (check-arg procedure? elt= 'delete-duplicates)
				  (let recur ((lis lis))
					(if (null-list? lis) lis
						(let* ((x (car lis))
							   (tail (cdr lis))
							   (new-tail (recur (delete x tail elt=))))
						  (if (eq? tail new-tail) lis (cons x new-tail))))))))

  (define delete-duplicates!
	(opt-lambda (lis (maybe-= equal?))
				(let ((elt= maybe-=))
				  (check-arg procedure? elt= 'delete-duplicates!)
				  (let recur ((lis lis))
					(if (null-list? lis) lis
						(let* ((x (car lis))
							   (tail (cdr lis))
							   (new-tail (recur (delete! x tail elt=))))
						  (if (eq? tail new-tail) lis (cons x new-tail))))))))

  )


;;; delete.ss ends here