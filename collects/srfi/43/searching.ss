;;;
;;; <searching.ss> ---- Vector searching
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

(module searching mzscheme
  
  (require "util.ss")
  
  (provide vector-index
           vector-index-right
           vector-skip
           vector-skip-right
           vector-binary-search
           vector-any
           vector-every)
  
  ;; All the functions (except vector-binary-search) here can be 
  ;; abstracted, but for performance I didn't do so.
  
  ;;; (VECTOR-INDEX <predicate?> <vector> ...)
  ;;;       -> exact, nonnegative integer or #F
  ;;;     (PREDICATE? <elt> ...) -> boolean ; N vectors -> N args
  ;;;   Search left-to-right across VECTOR ... in parallel, returning the
  ;;;   index of the first set of values VALUE ... such that (PREDICATE?
  ;;;   VALUE ...) returns a true value; if no such set of elements is
  ;;;   reached, return #F.
  (define vector-index
    (letrec ((loop1  (lambda (pred? vec len i)
                       (cond ((= i len) #f)
                             ((pred? (vector-ref vec i)) i)
                             (else (loop1 pred? vec len (add1 i))))))
             (loop2+ (lambda (pred? vectors len i)
                       (cond ((= i len) #f)
                             ((apply pred? (vectors-ref vectors i)) i)
                             (else (loop2+ pred? vectors len (add1 i)))))))
      (lambda (pred? vec . vectors)
        (unless (procedure? pred?)
          (apply raise-type-error
                 'vector-index "procedure" 0
                 pred? vec vectors))
        (unless (vector? vec)
          (apply raise-type-error
                 'vector-index "vector" 1
                 pred? vec vectors))
        (if (null? vectors)
            (loop1 pred? vec (vector-length vec) 0)
            (begin (check-list-of-vecs vectors 'vector-index 2
                                       (list* pred? vec vectors))
                   (loop2+ pred? (cons vec vectors)
                           (%smallest-length vectors
                                             (vector-length vec))
                           0))))))
  
  ;;; (VECTOR-SKIP <predicate?> <vector> ...)
  ;;;       -> exact, nonnegative integer or #F
  ;;;     (PREDICATE? <elt> ...) -> boolean ; N vectors -> N args
  ;;;   (vector-index (lambda elts (not (apply PREDICATE? elts)))
  ;;;                 VECTOR ...)
  ;;;   Like VECTOR-INDEX, but find the index of the first set of values
  ;;;   that do _not_ satisfy PREDICATE?.
  (define vector-skip
    (letrec ((loop1  (lambda (pred? vec len i)
                       (cond ((= i len) #f)
                             ((pred? (vector-ref vec i))
                              (loop1 pred? vec len (add1 i)))
                             (else i))))
             (loop2+ (lambda (pred? vectors len i)
                       (cond ((= i len) #f)
                             ((apply pred? (vectors-ref vectors i))
                              (loop2+ pred? vectors len (add1 i)))
                             (else i)))))
      (lambda (pred? vec . vectors)
        (unless (procedure? pred?)
          (apply raise-type-error
                 'vector-skip "procedure" 0
                 pred? vec vectors))
        (unless (vector? vec)
          (apply raise-type-error
                 'vector-skip "vector" 1
                 pred? vec vectors))
        (if (null? vectors)
            (loop1 pred? vec (vector-length vec) 0)
            (begin (check-list-of-vecs vectors 'vector-skip 2
                                       (list* pred? vec vectors))
                   (loop2+ pred? (cons vec vectors)
                           (%smallest-length vectors
                                             (vector-length vec))
                           0))))))
  
  ;;; (VECTOR-INDEX-RIGHT <predicate?> <vector> ...)
  ;;;       -> exact, nonnegative integer or #F
  ;;;     (PREDICATE? <elt> ...) -> boolean ; N vectors -> N args
  ;;;   Right-to-left variant of VECTOR-INDEX.
  (define vector-index-right
    (letrec ((loop1  (lambda (pred? vec i)
                       (if (zero? i)
                           #f
                           (let ((i (sub1 i)))
                             (if (pred? (vector-ref vec i))
                                 i
                                 (loop1 pred? vec i))))))
             (loop2+ (lambda (pred? vectors i)
                       (if (zero? i)
                           #f
                           (let ((i (sub1 i)))
                             (if (apply pred? (vectors-ref vectors i))
                                 i
                                 (loop2+ pred? vectors i)))))))
      (lambda (pred? vec . vectors)
        (unless (procedure? pred?)
          (apply raise-type-error
                 'vector-index-right "procedure" 0
                 pred? vec vectors))
        (unless (vector? vec)
          (apply raise-type-error
                 'vector-index-right "vector" 1
                 pred? vec vectors))
        (if (null? vectors)
            (loop1 pred? vec (vector-length vec))
            (begin (check-list-of-vecs vectors 'vector-index-right 2
                                       (list* pred? vec vectors))
                   (loop2+ pred? (cons vec vectors)
                           (%smallest-length vectors
                                             (vector-length vec))))))))
  
  ;;; (VECTOR-SKIP-RIGHT <predicate?> <vector> ...)
  ;;;       -> exact, nonnegative integer or #F
  ;;;     (PREDICATE? <elt> ...) -> boolean ; N vectors -> N args
  ;;;   Right-to-left variant of VECTOR-SKIP.
  (define vector-skip-right
    (letrec ((loop1  (lambda (pred? vec i)
                       (if (zero? i)
                           #f
                           (let ((i (sub1 i)))
                             (if (pred? (vector-ref vec i))
                                 (loop1 pred? vec i)
                                 i)))))
             (loop2+ (lambda (pred? vectors i)
                       (if (zero? i)
                           #f
                           (let ((i (sub1 i)))
                             (if (apply pred? (vectors-ref vectors i))
                                 (loop2+ pred? vectors i)
                                 i))))))
      (lambda (pred? vec . vectors)
        (unless (procedure? pred?)
          (apply raise-type-error
                 'vector-skip-right "procedure" 0
                 pred? vec vectors))
        (unless (vector? vec)
          (apply raise-type-error
                 'vector-skip-right "vector" 1
                 pred? vec vectors))
        (if (null? vectors)
            (loop1 pred? vec (vector-length vec))
            (begin (check-list-of-vecs vectors 'vector-skip-right 2
                                       (list* pred? vec vectors))
                   (loop2+ pred? (cons vec vectors)
                           (%smallest-length vectors
                                             (vector-length vec))))))))
  
  ;;; (VECTOR-BINARY-SEARCH <vector> <value> <cmp>)
  ;;;       -> exact, nonnegative integer or #F
  ;;;     (CMP <value1> <value2>) -> integer
  ;;;       positive -> VALUE1 > VALUE2
  ;;;       zero     -> VALUE1 = VALUE2
  ;;;       negative -> VALUE1 < VALUE2
  ;;;   Perform a binary search through VECTOR for VALUE, comparing each
  ;;;   element to VALUE with CMP.
  (define (vector-binary-search vec value cmp)
    (unless (vector? vec)
      (raise-type-error 'vector-binary-search "vector" 0
                        vec value cmp))
    (unless (procedure-arity-includes? cmp 2)
      (raise-type-error 'vector-binary-search "procedure of arity 2" 2
                        vec value cmp))
    (let loop ((start 0)
               (end (vector-length vec))
               (j -1))
      (let ((i (quotient (+ start end) 2)))
        (if (= i j)
            #f
            (let ((comparison (cmp (vector-ref vec i) value)))
              (unless (integer? comparison)
                (raise-type-error 'vector-binary-search
                                  "procedure that returns an integer"
                                  2
                                  vec value cmp))
              (cond ((zero?     comparison) i)
                    ((positive? comparison) (loop start i i))
                    (else                   (loop i end i))))))))
  
  ;;; (VECTOR-ANY <pred?> <vector> ...) -> value
  ;;;   Apply PRED? to each parallel element in each VECTOR ...; if PRED?
  ;;;   should ever return a true value, immediately stop and return that
  ;;;   value; otherwise, when the shortest vector runs out, return #F.
  ;;;   The iteration and order of application of PRED? across elements
  ;;;   is of the vectors is strictly left-to-right.
  (define vector-any
    (letrec ((loop1 (lambda (pred? vec i len)
                      (and (not (= i len))
                           (or (pred? (vector-ref vec i))
                               (loop1 pred? vec (add1 i) len)))))
             (loop2+ (lambda (pred? vectors i len)
                       (and (not (= i len))
                            (or (apply pred? (vectors-ref vectors i))
                                (loop2+ pred? vectors (add1 i) len))))))
      (lambda (pred? vec . vectors)
        (unless (procedure? pred?)
          (apply raise-type-error
                 'vector-any "procedure" 0
                 pred? vec vectors))
        (unless (vector? vec)
          (apply raise-type-error
                 'vector-any "vector" 1
                 pred? vec vectors))
        (if (null? vectors)
            (loop1 pred? vec 0 (vector-length vec))
            (begin (check-list-of-vecs vectors 'vector-any 2
                                       (list* pred? vec vectors))
                   (loop2+ pred? (cons vec vectors)
                           0 (%smallest-length vectors
                                               (vector-length vec))))))))
  
  ;;; (VECTOR-EVERY <pred?> <vector> ...) -> value
  ;;;   Apply PRED? to each parallel value in each VECTOR ...; if PRED?
  ;;;   should ever return #F, immediately stop and return #F; otherwise,
  ;;;   if PRED? should return a true value for each element, stopping at
  ;;;   the end of the shortest vector, return the last value that PRED?
  ;;;   returned.  In the case that there is an empty vector, return #T.
  ;;;   The iteration and order of application of PRED? across elements
  ;;;   is of the vectors is strictly left-to-right.
  (define vector-every
    (letrec ((loop1 (lambda (pred? vec i len)
                      (or (> i len)
                          (if (= i len)
                              (pred? (vector-ref vec i))
                              (and (pred? (vector-ref vec i))
                                   (loop1 pred? vec (add1 i) len))))))
             (loop2+ (lambda (pred? vectors i len)
                       (or (> i len)
                           (if (= i len)
                               (apply pred? (vectors-ref vectors i))
                               (and (apply pred? (vectors-ref vectors i))
                                    (loop2+ pred? vectors (add1 i) len)))))))
      (lambda (pred? vec . vectors)
        (unless (procedure? pred?)
          (apply raise-type-error
                 'vector-every "procedure" 0
                 pred? vec vectors))
        (unless (vector? vec)
          (apply raise-type-error
                 'vector-every "vector" 1
                 pred? vec vectors))
        (if (null? vectors)
            (loop1 pred? vec 0 (sub1 (vector-length vec)))
            (begin (check-list-of-vecs vectors 'vector-every 2
                                       (list* pred? vec vectors))
                   (loop2+ pred?
                           (cons vec vectors)
                           0
                           (sub1
                            (%smallest-length vectors
                                              (vector-length vec))))))))))

