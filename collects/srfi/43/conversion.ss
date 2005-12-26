;;;
;;; <conversion.ss> ---- Vector conversion
;;; Time-stamp: <05/03/07 18:19:59 Zhu Chongkai>
;;;
;;; Copyright (C) 2005-2006 by Zhu Chongkai. 
;;;
;;; This file is part of SRFI-43.

;;; SRFI-43 is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; SRFI-43 is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with SRFI-43; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

;;; Author: Zhu Chongkai <mrmathematica@yahoo.com>
;;
;;
;; Commentary:

;; Based on the reference implementation by Taylor Campbell and hence:

;;; Copyright (C) 2003, 2004 Taylor Campbell.
;;; All rights reserved.
;;;
;;; You may do as you please with this code, as long as you refrain
;;; from removing this copyright notice or holding me liable in _any_
;;; circumstances for _any_ damages that may be caused by it; and you
;;; may quote sections from it as you please, as long as you credit me.

(module conversion mzscheme
  
  (require "util.ss"
           (lib "etc.ss"))
  
  (provide (rename my-vector->list vector->list)
           reverse-vector->list
           reverse-list->vector)
  
  ;;; (VECTOR->LIST <vector> [<start> <end>]) -> list
  ;;;   [R5RS+] Produce a list containing the elements in the locations
  ;;;   between START, whose default is 0, and END, whose default is the
  ;;;   length of VECTOR, from VECTOR.
  (define (my-vector->list vec . maybe-start+end)
    (unless (vector? vec)
      (apply raise-type-error
             'vector->list "vector" 0
             vec maybe-start+end))
    (if (null? maybe-start+end)
        (vector->list vec)           ;+++
        (apply (opt-lambda (vec (start 0) (end (vector-length vec)))
                 (check-indices vec start end 'vector->list)
                 ;(unfold (lambda (i)        ; No SRFI 1.
                 ;          (< i start))
                 ;        (lambda (i) (vector-ref vec i))
                 ;        (lambda (i) (sub1 i))
                 ;        (sub1 end))
                 (do ((i (sub1 end) (sub1 i))
                      (result '() (cons (vector-ref vec i) result)))
                   ((< i start) result)))
               vec maybe-start+end)))
  
  ;;; (REVERSE-VECTOR->LIST <vector> [<start> <end>]) -> list
  ;;;   Produce a list containing the elements in the locations between
  ;;;   START, whose default is 0, and END, whose default is the length
  ;;;   of VECTOR, from VECTOR, in reverse order.
  (define (reverse-vector->list vec . maybe-start+end)
    (unless (vector? vec)
      (apply raise-type-error
             'reverse-vector->list "vector" 0
             vec maybe-start+end))
    (apply (opt-lambda (vec (start 0) (end (vector-length vec)))
             (check-indices vec start end 'reverse-vector->list)
             ;(unfold (lambda (i) (= i end))     ; No SRFI 1.
             ;        (lambda (i) (vector-ref vec i))
             ;        (lambda (i) (add1 i))
             ;        start)
             (do ((i start (add1 i))
                  (result '() (cons (vector-ref vec i) result)))
               ((= i end) result)))
           vec maybe-start+end))
  
  ;;; (REVERSE-LIST->VECTOR <list> -> vector
  ;;;   Produce a vector containing the elements in LIST in reverse order.
  (define (reverse-list->vector lst)
    (unless (list? lst)
      (raise-type-error 'reverse-list->vector "proper list" lst))
    (let* ((len (length lst))
           (vec (make-vector len)))
      (unfold1! (lambda (index l) (values (car l) (cdr l)))
                vec
                (sub1 len)
                lst)
      vec)))
