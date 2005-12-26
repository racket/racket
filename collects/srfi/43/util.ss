;;;
;;; <util.ss> ---- Utility functions
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

(module util mzscheme
  
  (require (lib "etc.ss" "mzlib")
           (lib "receive.ss" "srfi" "8"))
  
  (provide (all-defined))
  
  ;;; (CHECK-INDEX <vector> <index> <callee>) -> 
  ;;;   Ensure that INDEX is a valid index into VECTOR; if not, signal an
  ;;;   error stating that it is not and that this happened in a call to
  ;;;   CALLEE.  (Note that this does NOT check that VECTOR is indeed a
  ;;;   vector.)
  (define (check-index vec index callee)
    (unless (nonneg-int? index)
      (raise-type-error callee "non-negative exact integer" index))
    (unless (and (<= 0 index)
                 (< index (vector-length vec)))
      (raise
       (make-exn:fail:contract
        (string->immutable-string
         (format "~a: index ~a out of range for vector: ~a"
                 callee index vec))
        (current-continuation-marks)))))
  
  
  ;;; (CHECK-START <vector> <index> <callee>) -> 
  ;;;   Ensure that INDEX is a valid bound of VECTOR; if not, signal an
  ;;;   error stating that it is not and that this happened in a call to
  ;;;   CALLEE.  (Note that this does NOT check that VECTOR is indeed a
  ;;;   vector.)
  (define (check-start vec index callee)
    (unless (nonneg-int? index)
      (raise-type-error callee "non-negative exact integer" index))
    (unless (<= 0 index (vector-length vec))
      (raise
       (make-exn:fail:contract
        (string->immutable-string
         (format "~a: index ~a out of range for vector: ~a"
                 callee index vec))
        (current-continuation-marks)))))
  
  ;;; (CHECK-INDICES <vector> <start> <end> <caller>) -> 
  ;;;   Ensure that START and END are valid bounds of a range within
  ;;;   VECTOR; if not, signal an error stating that they are not
  ;;;   while calling CALLEE. 
  (define (check-indices vec start end callee)
    (unless (nonneg-int? start)
      (raise-type-error callee "non-negative exact integer" start))
    (unless (nonneg-int? end)
      (raise-type-error callee "non-negative exact integer" end))
    (unless (<= 0 start end (vector-length vec))
      (raise
       (make-exn:fail:contract
        (string->immutable-string
         (format "~a: indices (~a, ~a) out of range for vector: ~a"
                 callee start end vec))
        (current-continuation-marks)))))
  
  (define (nonneg-int? x)
    (and (integer? x)
         (exact? x)
         (not (negative? x))))
  
  ;;; (%VECTOR-COPY! <target> <tstart> <source> <sstart> <send>)
  ;;;   Copy elements at locations SSTART to SEND from SOURCE to TARGET,
  ;;;   starting at TSTART in TARGET.
  (define (%vector-copy! target tstart source sstart send)
    (let loop ((i sstart)
               (j tstart))
      (cond ((< i send)
             (vector-set! target j
                          (vector-ref source i))
             (loop (add1 i) (add1 j))))))
  
  ;;; (%VECTOR-REVERSE-COPY! <target> <tstart> <source> <sstart> <send>)
  ;;;   Copy elements from SSTART to SEND from SOURCE to TARGET, in the
  ;;;   reverse order.
  (define %vector-reverse-copy!
    (letrec ((loop (lambda (target source sstart i j)
                     (cond ((>= i sstart)
                            (vector-set! target j (vector-ref source i))
                            (loop target source sstart
                                  (sub1 i)
                                  (add1 j)))))))
      (lambda (target tstart source sstart send)
        (loop target source sstart
              (sub1 send)
              tstart))))
  
  ;; type-check : check whether list-of-vecs is list of VECTORs
  (define check-list-of-vecs
    (opt-lambda (list-of-vecs caller (n 0) (all-args list-of-vecs))
      (let loop ((l list-of-vecs)
                 (i 0))
        (unless (null? l)
          (if (vector? (car l))
              (loop (cdr l) (add1 i))
              (apply raise-type-error
                     caller "vector"
                     (+ n i)
                     all-args))))))
  
  ;;; (%SMALLEST-LENGTH <vector-list> <default-length>)
  ;;;       -> exact, nonnegative integer
  ;;;   Compute the smallest length of VECTOR-LIST.  DEFAULT-LENGTH is
  ;;;   the length that is returned if VECTOR-LIST is empty.  Common use
  ;;;   of this is in n-ary vector routines:
  ;;;     (define (f vec . vectors)
  (define (%smallest-length vector-list length)
    (if (null? vector-list)
        length
        (%smallest-length (cdr vector-list)
                          (min length
                               (vector-length (car vector-list))))))
  
  (define (vectors-ref vectors i)
    (map (lambda (v) (vector-ref v i)) vectors))
  
  ;;; from vector-unfold-right
  (define (unfold1! f vec i seed)
    (if (>= i 0)
        (receive (elt new-seed)
                 (f i seed)
                 (vector-set! vec i elt)
                 (unfold1! f vec (sub1 i) new-seed)))))
