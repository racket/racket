;;;
;;; <iteration.ss> ---- Vector iteration
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

(module iteration mzscheme
  
  (require "util.ss")
  
  (provide vector-fold
           vector-fold-right
           vector-map
           vector-map!
           vector-for-each
           vector-count)
  
  ;;; (VECTOR-FOLD <kons> <initial-knil> <vector> ...) -> knil
  ;;;     (KONS <knil> <elt> ...) -> knil' ; N vectors -> N+1 args
  ;;;   The fundamental vector iterator.  KONS is iterated over each
  ;;;   index in all of the vectors in parallel, stopping at the end of
  ;;;   the shortest; KONS is applied to an argument list of (list I
  ;;;   STATE (vector-ref VEC I) ...), where STATE is the current state
  ;;;   value -- the state value begins with KNIL and becomes whatever
  ;;;   KONS returned at the respective iteration --, and I is the
  ;;;   current index in the iteration.  The iteration is strictly left-
  ;;;   to-right.
  ;;;     (vector-fold KONS KNIL (vector E_1 E_2 ... E_N))
  ;;;       <=>
  ;;;     (KONS (... (KONS (KONS KNIL E_1) E_2) ... E_N-1) E_N)
  (define (vector-fold kons knil vec . vectors)
    (unless (procedure? kons)
      (apply raise-type-error
             'vector-fold "procedure" 0
             kons knil vec vectors))
    (unless (vector? vec)
      (apply raise-type-error
             'vector-fold "vector" 2
             kons knil vec vectors))
    (if (null? vectors)
        (%vector-fold1 kons knil (vector-length vec) vec)
        (begin (check-list-of-vecs vectors 'vector-fold 3
                                   (list* kons knil vec vectors))
               (%vector-fold2+ kons knil
                               (%smallest-length vectors
                                                 (vector-length vec))
                               (cons vec vectors)))))
  
  (define %vector-fold1
    (letrec ((loop (lambda (kons knil len vec i)
                     (if (= i len)
                         knil
                         (loop kons
                               (kons i knil (vector-ref vec i))
                               len vec (add1 i))))))
      (lambda (kons knil len vec)
        (loop kons knil len vec 0))))
  (define %vector-fold2+
    (letrec ((loop (lambda (kons knil len vectors i)
                     (if (= i len)
                         knil
                         (loop kons
                               (apply kons i knil
                                      (vectors-ref vectors i))
                               len vectors (add1 i))))))
      (lambda (kons knil len vectors)
        (loop kons knil len vectors 0))))
  
  ;;; (VECTOR-COUNT <predicate?> <vector> ...)
  ;;;       -> exact, nonnegative integer
  ;;;     (PREDICATE? <index> <value> ...) ; N vectors -> N+1 args
  ;;;   PREDICATE? is applied element-wise to the elements of VECTOR ...,
  ;;;   and a count is tallied of the number of elements for which a
  ;;;   true value is produced by PREDICATE?.  This count is returned.
  (define (vector-count pred? vec . vectors)
    (unless (procedure? pred?)
      (apply raise-type-error
             'vector-count "procedure" 0
             pred? vec vectors))
    (if (null? vectors)
        (%vector-fold1 (lambda (index count elt)
                         (if (pred? index elt)
                             (add1 count)
                             count))
                       0
                       (vector-length vec)
                       vec)
        (begin (check-list-of-vecs vectors 'vector-count 2
                                   (list* pred? vec vectors))
               (%vector-fold2+ (lambda (index count . elts)
                                 (if (apply pred? index elts)
                                     (add1 count)
                                     count))
                               0
                               (%smallest-length vectors
                                                 (vector-length vec))
                               (cons vec vectors)))))
  
  ;;; (VECTOR-FOLD-RIGHT <kons> <initial-knil> <vector> ...) -> knil
  ;;;     (KONS <knil> <elt> ...) -> knil' ; N vectors => N+1 args
  ;;;   The fundamental vector recursor.  Iterates in parallel across
  ;;;   VECTOR ... right to left, applying KONS to the elements and the
  ;;;   current state value; the state value becomes what KONS returns
  ;;;   at each next iteration.  KNIL is the initial state value.
  ;;;     (vector-fold-right KONS KNIL (vector E_1 E_2 ... E_N))
  ;;;       <=>
  ;;;     (KONS (... (KONS (KONS KNIL E_N) E_N-1) ... E_2) E_1)
  ;;;
  ;;; Not implemented in terms of a more primitive operations that might
  ;;; called %VECTOR-FOLD-RIGHT due to the fact that it wouldn't be very
  ;;; useful elsewhere.
  (define vector-fold-right
    (letrec ((loop1 (lambda (kons knil vec i)
                      (if (zero? i)
                          knil
                          (let ((j (sub1 i)))
                            (loop1 kons
                                   (kons j knil (vector-ref vec j))
                                   vec
                                   j)))))
             (loop2+ (lambda (kons knil vectors i)
                       (if (zero? i)
                           knil
                           (let ((j (sub1 i)))
                             (loop2+ kons
                                     (apply kons j knil
                                            (vectors-ref vectors j))
                                     vectors
                                     j))))))
      (lambda (kons knil vec . vectors)
        (unless (procedure? kons)
          (apply raise-type-error
                 'vector-fold-right "procedure" 0
                 kons knil vec vectors))
        (unless (vector? vec)
          (apply raise-type-error
                 'vector-fold-right "vector" 2
                 kons knil vec vectors))
        (if (null? vectors)
            (loop1 kons knil vec (vector-length vec))
            (begin (check-list-of-vecs vectors 'vector-fold-right 3
                                       (list* kons knil vec vectors))
                   (loop2+ kons knil (cons vec vectors)
                           (%smallest-length vectors
                                             (vector-length vec))))))))
  
  ;;; (VECTOR-MAP <f> <vector> ...) -> vector
  ;;;     (F <elt> ...) -> value ; N vectors -> N args
  ;;;   Constructs a new vector of the shortest length of the vector
  ;;;   arguments.  Each element at index I of the new vector is mapped
  ;;;   from the old vectors by (F I (vector-ref VECTOR I) ...).  The
  ;;;   dynamic order of application of F is unspecified.
  (define (vector-map f vec . vectors)
    (unless (procedure? f)
      (apply raise-type-error
             'vector-map "procedure" 0
             f vec vectors))
    (unless (vector? vec)
      (apply raise-type-error
             'vector-map "vector" 1
             f vec vectors))
    (if (null? vectors)
        (let ((len (vector-length vec)))
          (%vector-map1! f (make-vector len) vec len))
        (begin (check-list-of-vecs vectors 'vector-map 2
                                   (list* f vec vectors))
               (let ((len (%smallest-length vectors
                                            (vector-length vec))))
                 (%vector-map2+! f (make-vector len)
                                 (cons vec vectors) len)))))
  
  ;;; (%VECTOR-MAP1! <f> <target> <length> <vector>)
  ;;;     (F <index> <elt>) -> elt'
  (define (%vector-map1! f target vec i)
    (if (zero? i)
        target
        (let ((j (sub1 i)))
          (vector-set! target j
                       (f j (vector-ref vec j)))
          (%vector-map1! f target vec j))))
  (define (%vector-map2+! f target vectors i)
    (if (zero? i)
        target
        (let ((j (sub1 i)))
          (vector-set! target j
                       (apply f j (vectors-ref vectors j)))
          (%vector-map2+! f target vectors j))))
  
  ;;; (VECTOR-MAP! <f> <vector> ...) -> vector
  ;;;     (F <elt> ...) -> element' ; N vectors -> N args
  ;;;   Similar to VECTOR-MAP, but rather than mapping the new elements
  ;;;   into a new vector, the new mapped elements are destructively
  ;;;   inserted into the first vector.  Again, the dynamic order of
  ;;;   application of F is unspecified, so it is dangerous for F to
  ;;;   manipulate the first VECTOR.
  (define (vector-map! f vec . vectors)
    (unless (procedure? f)
      (apply raise-type-error
             'vector-map! "procedure" 0
             f vec vectors))
    (unless (vector? vec)
      (apply raise-type-error
             'vector-map! "vector" 1
             f vec vectors))
    (if (null? vectors)
        (%vector-map1! f vec vec (vector-length vec))
        (begin (check-list-of-vecs vectors 'vector-map! 2
                                   (list* f vec vectors))
               (%vector-map2+! f vec (cons vec vectors)
                               (%smallest-length vectors
                                                 (vector-length vec))))))
  
  ;;; (VECTOR-FOR-EACH <f> <vector> ...) -> void
  ;;;     (F <elt> ...) ; N vectors -> N args
  ;;;   Simple vector iterator: applies F to each index in the range [0,
  ;;;   LENGTH), where LENGTH is the length of the smallest vector
  ;;;   argument passed, and the respective element at that index.  In
  ;;;   contrast with VECTOR-MAP, F is reliably applied to each
  ;;;   subsequent elements, starting at index 0 from left to right, in
  ;;;   the vectors.
  (define vector-for-each
    (letrec ((for-each1
              (lambda (f vec i len)
                (when (< i len)
                  (f i (vector-ref vec i))
                  (for-each1 f vec (add1 i) len))))
             (for-each2+
              (lambda (f vecs i len)
                (when (< i len)
                  (apply f i (vectors-ref vecs i))
                  (for-each2+ f vecs (add1 i) len)))))
      (lambda (f vec . vectors)
        (unless (procedure? f)
          (apply raise-type-error
                 'vector-for-each "procedure" 0
                 f vec vectors))
        (unless (vector? vec)
          (apply raise-type-error
                 'vector-for-each "vector" 1
                 f vec vectors))
        (if (null? vectors)
            (for-each1 f vec 0 (vector-length vec))
            (begin (check-list-of-vecs vectors 'vector-for-each 2
                                       (list* f vec vectors))
                   (for-each2+ f (cons vec vectors) 0
                               (%smallest-length vectors
                                                 (vector-length vec)))))))))
