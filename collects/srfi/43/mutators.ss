;;;
;;; <mutators.ss> ---- Vector mutators
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

(module mutators mzscheme
  
  (require "util.ss"
           (lib "etc.ss"))
  
  (provide vector-swap!
           (rename my-vector-fill! vector-fill!)
           vector-reverse!
           vector-copy!
           vector-reverse-copy!)
  
  ;;; (VECTOR-SWAP! <vector> <index1> <index2>) -> void
  ;;;   Swap the values in the locations at INDEX1 and INDEX2.
  (define (vector-swap! vec i j)
    (unless (vector? vec)
      (raise-type-error 'vector-swap! "vector" 0
                        vec i j))
    (check-index vec i 'vector-swap!)
    (check-index vec j 'vector-swap!)
    (%vector-swap! vec i j))
  
  (define (%vector-swap! vec i j)
    (let ((x (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j x)))
  
  ;;; (VECTOR-FILL! <vector> <value> [<start> <end>]) -> <vector>
  ;;;   [R5RS+] Fill the locations in VECTOR between START, whose default
  ;;;   is 0, and END, whose default is the length of VECTOR, with VALUE.
  ;;;
  ;;; This one can probably be made really fast natively.
  (define (my-vector-fill! vec value . maybe-start+end)
    (cond ((null? maybe-start+end)
           (vector-fill! vec value))     ;+++
          ((not (vector? vec))
           (apply raise-type-error
                  'vector-fill! "vector" 0
                  vec value maybe-start+end))
          (else
           (apply (opt-lambda (vec value (start 0) (end (vector-length vec)))
                    (check-indices vec start end 'vector-fill!)
                    (do ((i start (add1 i)))
                      ((= i end))
                      (vector-set! vec i value))
                    vec)
                  vec value maybe-start+end))))
  
  (define %vector-reverse!
    (letrec ((loop (lambda (vec i j)
                     (when (< i j)
                       (%vector-swap! vec i j)
                       (loop vec (add1 i) (sub1 j))))))
      (lambda (vec start end)
        (loop vec start (sub1 end)))))
  
  ;;; (VECTOR-REVERSE! <vector> [<start> <end>]) -> void
  ;;;   Destructively reverse the contents of the sequence of locations
  ;;;   in VECTOR between START, whose default is 0, and END, whose
  ;;;   default is the length of VECTOR.
  (define (vector-reverse! vec . maybe-start+end)
    (unless (vector? vec)
      (apply raise-type-error
             'vector-reverse! "vector" 0
             vec maybe-start+end))
    (apply (opt-lambda (vec (start 0) (end (vector-length vec)))
             (check-indices vec start end 'vector-reverse!)
             (%vector-reverse! vec start end))
           vec maybe-start+end))
  
  ;;; (VECTOR-COPY! <target> <tstart> <source> [<sstart> <send>])
  ;;;       -> unspecified
  ;;;   Copy the values in the locations in [SSTART,SEND) from SOURCE to
  ;;;   to TARGET, starting at TSTART in TARGET.
  (define (vector-copy! target tstart source . maybe-sstart+send)
    (unless (vector? target)
      (apply raise-type-error
             'vector-copy! "vector" 0
             target tstart source maybe-sstart+send))
    (check-start target tstart 'vector-copy!)
    (unless (vector? source)
      (apply raise-type-error
             'vector-copy! "vector" 2
             target tstart source maybe-sstart+send))
    (apply (opt-lambda (target
                        tstart
                        source
                        (sstart 0)
                        (send (vector-length source)))
             (check-indices source sstart send 'vector-copy!)
             (if (< (- (vector-length target) tstart)
                    (- send sstart))
                 (error 'vector-copy!
                        "target vector not long enough to copy"))
             (%vector-copy! target tstart source sstart send))
           target tstart source maybe-sstart+send))
  
  ;;; (VECTOR-REVERSE-COPY! <target> <tstart> <source> [<sstart> <send>])
  (define (vector-reverse-copy! target tstart source . maybe-sstart+send)
    (unless (vector? target)
      (apply raise-type-error
             'vector-reverse-copy! "vector" 0
             target tstart source maybe-sstart+send))
    (check-start target tstart 'vector-reverse-copy!)
    (unless (vector? source)
      (apply raise-type-error
             'vector-reverse-copy! "vector" 2
             target tstart source maybe-sstart+send))
    (apply (opt-lambda (target
                        tstart
                        source
                        (sstart 0)
                        (send (vector-length source)))
             (check-indices source sstart send 'vector-reverse-copy!)
             (cond ((< (- (vector-length target) tstart)
                       (- send sstart))
                    (error 'vector-reverse-copy!
                           "target vector not long enough to copy"))
                   ((and (eq? target source)
                         (= sstart tstart))
                    (%vector-reverse! target tstart send))
                   ((and (eq? target source)
                         (or (between? sstart tstart send)
                             (between? tstart sstart
                                       (+ tstart (- send sstart)))))
                    ;an error in the reference implement here
                    (error 'vector-reverse-copy!
                           "Vector range for self-copying overlaps"))
                   (else
                    (%vector-reverse-copy! target tstart
                                           source sstart send))))
           target tstart source maybe-sstart+send))
  (define (between? x y z)
    (and (<  x y)
         (<= y z))))
