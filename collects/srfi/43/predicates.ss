;;;
;;; <predicates.ss> ---- Vector predicates
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

(module predicates mzscheme
  
  (require "util.ss")
  
  (provide vector-empty?
           vector=)
  
  ;;; (VECTOR-EMPTY? <vector>) -> boolean
  ;;;   Return #T if VECTOR has zero elements in it, i.e. VECTOR's length
  ;;;   is 0, and #F if not.
  (define (vector-empty? vec)
    (unless (vector? vec)
      (raise-type-error 'vector-empty? "vector" vec))
    (zero? (vector-length vec)))
  
  ;;; (VECTOR= <elt=?> <vector> ...) -> boolean
  ;;;     (ELT=? <value> <value>) -> boolean
  ;;;   Determine vector equality generalized across element comparators.
  ;;;   Vectors A and B are equal iff their lengths are the same and for
  ;;;   each respective elements E_a and E_b (element=? E_a E_b) returns
  ;;;   a true value.  ELT=? is always applied to two arguments.  Element
  ;;;   comparison must be consistent wtih EQ?; that is, if (eq? E_a E_b)
  ;;;   results in a true value, then (ELEMENT=? E_a E_b) must result in a
  ;;;   true value.  This may be exploited to avoid multiple unnecessary
  ;;;   element comparisons.  (This implementation does, but does not deal
  ;;;   with the situation that ELEMENT=? is EQ? to avoid more unnecessary
  ;;;   comparisons, but I believe this optimization is probably fairly
  ;;;   insignificant.)
  ;;;   
  ;;;   If the number of vector arguments is zero or one, then #T is
  ;;;   automatically returned.  If there are N vector arguments,
  ;;;   VECTOR_1 VECTOR_2 ... VECTOR_N, then VECTOR_1 & VECTOR_2 are
  ;;;   compared; if they are equal, the vectors VECTOR_2 ... VECTOR_N
  ;;;   are compared.  The precise order in which ELT=? is applied is not
  ;;;   specified.
  (define (vector= elt=? . vectors)
    (unless (procedure-arity-includes? elt=? 2)
      (apply raise-type-error
             'vector= "procedure of arity 2" 0
             elt=? vectors))
    (cond ((null? vectors)
           #t)
          ((null? (cdr vectors))
           (unless (vector? (car vectors))
             (apply raise-type-error
                    'vector= "vector" 1
                    elt=? vectors))
           #t)
          (else
           (check-list-of-vecs vectors 'vector=
                               1 (cons elt=? vectors))
           (let loop ((vecs vectors))
             (let ((vec1 (car vecs))
                   (vec2+ (cdr vecs)))
               (or (null? vec2+)
                   (and (binary-vector= elt=? vec1 (car vec2+))
                        (loop vec2+))))))))
  (define (binary-vector= elt=? vector-a vector-b)
    (or (eq? vector-a vector-b)           ;+++
        (let ((length-a (vector-length vector-a)))
          (and (= length-a (vector-length vector-b))
               (let loop ((i 0))
                 (or (= i length-a)
                     (and (elt=? (vector-ref vector-a i)
                                 (vector-ref vector-b i))
                          (loop (add1 i))))))))))
