;;;
;;; <alist.ss> ---- Association list functions
;;; Time-stamp: <02/03/01 13:56:33 noel>
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

(module alist
  mzscheme

  (require (lib "etc.ss" "mzlib")
		   (lib "optional.ss" "srfi")
		   (rename "search.ss" find find)
		   "filter.ss"
		   (rename "fold.ss" s:map map))

  (provide (rename my-assoc assoc)
		   alist-cons
		   alist-copy
		   alist-delete
		   alist-delete!)

  
  ;; Extended from R4RS to take an optional comparison argument.
  (define my-assoc
	(opt-lambda (x lis (maybe-= equal?))
	  (let ((= maybe-=))
		(find (lambda (entry) (= x (car entry))) lis))))

  (define (alist-cons key datum alist) (cons (cons key datum) alist))

  (define (alist-copy alist)
	(s:map (lambda (elt) (cons (car elt) (cdr elt)))
		   alist))

  (define alist-delete
	(opt-lambda (key alist (maybe-= equal?))
	  (let ((= maybe-=))
		(filter (lambda (elt) (not (= key (car elt)))) alist))))

  (define alist-delete!
	(opt-lambda (key alist (maybe-= equal?))
	  (let ((= maybe-=))
		(filter! (lambda (elt) (not (= key (car elt)))) alist))))

  )

;;; alist.ss ends here