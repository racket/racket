;;;
;;; <constructors.ss> ---- Vector constructors
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


(module constructors mzscheme
  
  (require (lib "receive.ss" "srfi" "8")
           "util.ss"
           (lib "etc.ss" "mzlib"))
  
  (provide vector-unfold
           vector-unfold-right
           vector-copy
           vector-reverse-copy
           vector-append
           vector-concatenate)
  
  ;;; (VECTOR-UNFOLD <f> <length> <initial-seed> ...) -> vector
  ;;;     (F <index> <seed> ...) -> [elt seed' ...]
  ;;;   The fundamental vector constructor.  Creates a vector whose
  ;;;   length is LENGTH and iterates across each index K between 0 and
  ;;;   LENGTH, applying F at each iteration to the current index and the
  ;;;   current seeds to receive N+1 values: first, the element to put in
  ;;;   the Kth slot and then N new seeds for the next iteration.
  (define vector-unfold
    (letrec ((tabulate!                   ; Special zero-seed case.
              (lambda (f vec i len)
                (cond ((< i len)
                       (vector-set! vec i (f i))
                       (tabulate! f vec (add1 i) len)))))
             (unfold1!                    ; Fast path for one seed.
              (lambda (f vec i len seed)
                (if (< i len)
                    (receive (elt new-seed)
                             (f i seed)
                             (vector-set! vec i elt)
                             (unfold1! f vec (add1 i) len new-seed)))))
             (unfold2+!                   ; Slower variant for N seeds.
              (lambda (f vec i len seeds)
                (if (< i len)
                    (receive (elt . new-seeds)
                             (apply f i seeds)
                             (vector-set! vec i elt)
                             (unfold2+! f vec (add1 i) len new-seeds))))))
      (lambda (f len . initial-seeds)
        (unless (procedure? f)
          (apply raise-type-error
                 'vector-unfold "procedure" 0
                 f len initial-seeds))
        (unless (nonneg-int? len)
          (apply raise-type-error
                 'vector-unfold "non-negative exact integer" 1
                 f len initial-seeds))
        (let ((vec (make-vector len)))
          (cond ((null? initial-seeds)
                 (tabulate! f vec 0 len))
                ((null? (cdr initial-seeds))
                 (unfold1! f vec 0 len (car initial-seeds)))
                (else
                 (unfold2+! f vec 0 len initial-seeds)))
          vec))))
  
  ;;; (VECTOR-UNFOLD-RIGHT <f> <length> <initial-seed> ...) -> vector
  ;;;     (F <seed> ...) -> [seed' ...]
  ;;;   Like VECTOR-UNFOLD, but it generates elements from LENGTH to 0
  ;;;   (still exclusive with  LENGTH and inclusive with 0), not 0 to
  ;;;   LENGTH as with VECTOR-UNFOLD.
  (define vector-unfold-right
    (letrec ((tabulate!
              (lambda (f vec i)
                (cond ((>= i 0)
                       (vector-set! vec i (f i))
                       (tabulate! f vec (sub1 i))))))
             (unfold2+!
              (lambda (f vec i seeds)
                (if (>= i 0)
                    (receive (elt . new-seeds)
                             (apply f i seeds)
                             (vector-set! vec i elt)
                             (unfold2+! f vec (sub1 i) new-seeds))))))
      (lambda (f len . initial-seeds)
        (unless (procedure? f)
          (apply raise-type-error
                 'vector-unfold-right "procedure" 0
                 f len initial-seeds))
        (unless (nonneg-int? len)
          (apply raise-type-error
                 'vector-unfold-right "non-negative exact integer" 1
                 f len initial-seeds))
        (let ((vec (make-vector len))
              (i (sub1 len)))
          (cond ((null? initial-seeds)
                 (tabulate! f vec i))
                ((null? (cdr initial-seeds))
                 (unfold1!  f vec i (car initial-seeds)))
                (else
                 (unfold2+! f vec i initial-seeds)))
          vec))))
  
  ;;; (VECTOR-COPY <vector> [<start> <end> <fill>]) -> vector
  ;;;   Create a newly allocated vector containing the elements from the
  ;;;   range [START,END) in VECTOR.  START defaults to 0; END defaults
  ;;;   to the length of VECTOR.  END may be greater than the length of
  ;;;   VECTOR, in which case the vector is enlarged; if FILL is passed,
  ;;;   the new locations from which there is no respective element in
  ;;;   VECTOR are filled with FILL.
  (define (vector-copy vec . arg)
    (unless (vector? vec)
      (raise-type-error 'vector-copy "vector" vec))
    (apply
     (opt-lambda (vec (start 0) (end (vector-length vec)) . fill)
       (check-start vec start 'vector-copy)
       (unless (nonneg-int? end)
         (raise-type-error 'vector-copy "non-negative exact integer" end))
       (unless (<= start end)
         (raise
          (make-exn:fail:contract
           (string->immutable-string
            (format "~a: indices (~a, ~a) out of range for vector: ~a"
                    'vector-copy start end vec))
           (current-continuation-marks))))
       (let ((new-vector
              (apply make-vector (cons (- end start) fill))))
         (%vector-copy! new-vector 0
                        vec        start
                        (min end (vector-length vec)))
         new-vector))
     vec arg))
  
  ;;; (VECTOR-REVERSE-COPY <vector> [<start> <end>]) -> vector
  ;;;   Create a newly allocated vector whose elements are the reversed
  ;;;   sequence of elements between START and END in VECTOR.  START's
  ;;;   default is 0; END's default is the length of VECTOR.
  (define (vector-reverse-copy vec . arg)
    (unless (vector? vec)
      (raise-type-error 'vector-reverse-copy "vector" vec))
    (apply
     (opt-lambda (vec (start 0) (end (vector-length vec)))
       (check-indices vec start end 'vector-reverse-copy)
       (let ((new (make-vector (- end start))))
         (%vector-reverse-copy! new 0 vec start end)
         new))
     vec arg))
  
  ;;; (VECTOR-APPEND <vector> ...) -> vector
  ;;;   Append VECTOR ... into a newly allocated vector and return that
  ;;;   new vector.
  (define (vector-append . vectors)
    (check-list-of-vecs vectors 'vector-append)
    (vector-concatenate:aux vectors))
  
  ;;; (VECTOR-CONCATENATE <vector-list>) -> vector
  ;;;   Concatenate the vectors in VECTOR-LIST.  This is equivalent to
  ;;;     (apply vector-append VECTOR-LIST)
  ;;; Actually, they're both implemented in terms of an internal routine.
  (define (vector-concatenate vector-list)
    (unless (and (list? vector-list)
                 (andmap vector? vector-list))
      (raise-type-error 'vector-concatenate "list of vectors" vector-list))
    (vector-concatenate:aux vector-list))
  
  ;;; Auxiliary for VECTOR-APPEND and VECTOR-CONCATENATE
  (define vector-concatenate:aux
    (letrec ((compute-length
              (lambda (vectors len)
                (if (null? vectors)
                    len
                    (let ((vec (car vectors)))
                      (compute-length (cdr vectors)
                                      (+ (vector-length vec) len))))))
             (concatenate!
              (lambda (vectors target to)
                (if (null? vectors)
                    target
                    (let* ((vec1 (car vectors))
                           (len (vector-length vec1)))
                      (%vector-copy! target to vec1 0 len)
                      (concatenate! (cdr vectors) target
                                    (+ to len)))))))
      (lambda (vectors)
        (let ((new-vector
               (make-vector (compute-length vectors 0))))
          (concatenate! vectors new-vector 0)
          new-vector)))))
