(module visort mzscheme
  (provide vector-insert-sort
           vector-insert-sort!
           %vector-insert-sort!)
  
  (require "vector-util.scm")
  
  ;;; The sort package -- stable vector insertion sort	-*- Scheme -*-
  ;;; Copyright (c) 1998 by Olin Shivers.
  ;;; This code is open-source; see the end of the file for porting and
  ;;; more copyright information.
  ;;; Olin Shivers 10/98.
  
  ;;; Exports:
  ;;; vector-insert-sort  < v [start end] -> vector
  ;;; vector-insert-sort! < v [start end] -> unspecific
  ;;;
  ;;; %vector-insert-sort! is also called from vqsort.scm's quick-sort function.
  
  (define (vector-insert-sort elt< v . maybe-start+end)
    (call-with-values
     (lambda () (vector-start+end v maybe-start+end))
     (lambda (start end)
       (let ((ans (vector-portion-copy v start end)))
         (%vector-insert-sort! elt< ans 0 (- end start))
         ans))))
  
  (define (vector-insert-sort! < v . maybe-start+end)
    (call-with-values
     (lambda () (vector-start+end v maybe-start+end))
     (lambda (start end)
       (%vector-insert-sort! < v start end))))
  
  (define (%vector-insert-sort! elt< v start end)
    (do ((i (+ 1 start) (+ i 1)))	; Invariant: [start,i) is sorted.
      ((>= i end))
      (let ((val (vector-ref v i)))
        (vector-set! v (let lp ((j i))		; J is the location of the
                         (if (<= j start)
                             start	; "hole" as it bubbles down.
                             (let* ((j-1 (- j 1))
                                    (vj-1 (vector-ref v j-1)))
                               (cond ((elt< val vj-1)
                                      (vector-set! v j vj-1)
                                      (lp j-1))
                                     (else j)))))
                     val))))
  
  ;;; Copyright
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; This code is
  ;;;     Copyright (c) 1998 by Olin Shivers.
  ;;; The terms are: You may do as you please with this code, as long as
  ;;; you do not delete this notice or hold me responsible for any outcome
  ;;; related to its use.
  ;;;
  ;;; Blah blah blah. Don't you think source files should contain more lines
  ;;; of code than copyright notice?
  
  
  ;;; Code tuning & porting
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; This code is tightly bummed as far as I can go in portable Scheme.
  ;;;
  ;;; The code can be converted to use unsafe vector-indexing and
  ;;; fixnum-specific arithmetic ops -- the safety checks done on entry
  ;;; to VECTOR-INSERT-SORT and VECTOR-INSERT-SORT! are sufficient to
  ;;; guarantee nothing bad will happen. However, note that if you alter
  ;;; %VECTOR-INSERT-SORT! to use dangerous primitives, you must ensure
  ;;; it is only called from clients that guarantee to observe its
  ;;; preconditions. In the implementation, %VECTOR-INSERT-SORT! is only
  ;;; called from VECTOR-INSERT-SORT! and the quick-sort code in
  ;;; vqsort.scm, and the preconditions are guaranteed for these two
  ;;; clients.  This should provide *big* speedups. In fact, all the
  ;;; code bumming I've done pretty much disappears in the noise unless
  ;;; you have a good compiler and also can dump the vector-index checks
  ;;; and generic arithmetic -- so I've really just set things up for
  ;;; you to exploit.
  ;;;
  ;;; If your Scheme has a faster mechanism for handling optional arguments
  ;;; (e.g., Chez), you should definitely port over to it. Note that argument
  ;;; defaulting and error-checking are interleaved -- you don't have to
  ;;; error-check defaulted START/END args to see if they are fixnums that are
  ;;; legal vector indices for the corresponding vector, etc.
  )
