;;;
;;; <filter.rkt> ---- List filtering and partitioning functions
;;; Time-stamp: <02/03/01 07:26:43 noel>
;;;
;;; Copyright (C) 2002 by Noel Welsh.
;;;
;;; This file is part of SRFI-1.

;;; This SRFI-1 implementation is distributed under the same terms as
;;; Racket.

;;; Author: Noel Welsh <noelwelsh@yahoo.com>

;; Commentary:

;; Based on the reference implementation by Olin Shiver and hence:

;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;     -Olin

;; Olin Shivers verified that he is fine with redistributing this code
;; under the LGPL.  (Verified personally by Eli Barzilay.)

#lang racket/base

(require srfi/optional "predicate.rkt")

(provide filter (rename-out [filter filter!])
         (rename-out [my-filter filter-with-sharing]) ; see comment below
         partition (rename-out [partition partition!])
         (rename-out [my-remove remove] [my-remove remove!]))

;; filter, remove, partition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILTER, REMOVE, PARTITION and their destructive counterparts do not
;; disorder the elements of their argument.

;; The following version of filter is not really needed, so we reprovide the
;; one from "scheme/private/list.rkt".  That one does not keep the longest tail,
;; but running a few benchmarks (on v3.99.0.18) shows that on long lists the
;; code below is slower, and on short lists it is a little faster, but not by
;; much.  However, seems that "lset.rkt" relies on tail-sharing, so it is
;; provided under an alternative name above.

;; This FILTER shares the longest tail of L that has no deleted
;; elements.  If Scheme had multi-continuation calls, they could be
;; made more efficient.

(define (my-filter pred lis)            ; Sleazing with EQ? makes this
  (check-arg procedure? pred 'filter)   ; one faster.
  (let recur ((lis lis))
    (if (null-list? lis) lis            ; Use NOT-PAIR? to handle dotted lists.
        (let ((head (car lis))
              (tail (cdr lis)))
          (if (pred head)
            (let ((new-tail (recur tail))) ; Replicate the RECUR call so
              (if (eq? tail new-tail) lis
                  (cons head new-tail)))
            (recur tail))))))           ; this one can be a tail call.

;; This implementation of FILTER!
;; - doesn't cons, and uses no stack;
;; - is careful not to do redundant SET-CDR! writes, as writes to memory are
;;   usually expensive on modern machines, and can be extremely expensive on
;;   modern Schemes (e.g., ones that have generational GC's).
;; It just zips down contiguous runs of in and out elts in LIS doing the
;; minimal number of SET-CDR!s to splice the tail of one run of ins to the
;; beginning of the next.
#; ; lists are immutable
(define (filter! pred lis)
  (check-arg procedure? pred 'filter!)
  (let lp ((ans lis))
    (cond ((null-list? ans)       ans) ; Scan looking for
          ((not (pred (car ans))) (lp (cdr ans))) ; first cons of result.
          ;; ANS is the eventual answer.
          ;; SCAN-IN: (CDR PREV) = LIS and (CAR PREV) satisfies PRED.
          ;;          Scan over a contiguous segment of the list that
          ;;          satisfies PRED.
          ;; SCAN-OUT: (CAR PREV) satisfies PRED. Scan over a contiguous
          ;;           segment of the list that *doesn't* satisfy PRED.
          ;;           When the segment ends, patch in a link from PREV
          ;;           to the start of the next good segment, and jump to
          ;;           SCAN-IN.
          (else
           (letrec ((scan-in (lambda (prev lis)
                               (if (pair? lis)
                                 (if (pred (car lis))
                                   (scan-in lis (cdr lis))
                                   (scan-out prev (cdr lis))))))
                    (scan-out (lambda (prev lis)
                                (let lp ((lis lis))
                                  (if (pair? lis)
                                    (if (pred (car lis))
                                      (begin (set-cdr! prev lis)
                                             (scan-in lis (cdr lis)))
                                      (lp (cdr lis)))
                                    (set-cdr! prev lis))))))
             (scan-in ans (cdr ans))
             ans)))))

;; Answers share common tail with LIS where possible;
;; the technique is slightly subtle.
(define (partition pred lis)
  (check-arg procedure? pred 'partition)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis) ; Use NOT-PAIR? to handle dotted lists.
        (let ((elt (car lis))
              (tail (cdr lis)))
          (let-values ([(in out) (recur tail)])
            (if (pred elt)
              (values (if (pair? out) (cons elt in) lis) out)
              (values in (if (pair? in) (cons elt out) lis))))))))

;; This implementation of PARTITION!
;; - doesn't cons, and uses no stack;
;; - is careful not to do redundant SET-CDR! writes, as writes to memory are
;;   usually expensive on modern machines, and can be extremely expensive on
;;   modern Schemes (e.g., ones that have generational GC's).
;; It just zips down contiguous runs of in and out elts in LIS doing the
;; minimal number of SET-CDR!s to splice these runs together into the result
;; lists.
#; ; lists are immutable
(define (partition! pred lis)
  (check-arg procedure? pred 'partition!)
  (if (null-list? lis) (values lis lis)
      ;; This pair of loops zips down contiguous in & out runs of the
      ;; list, splicing the runs together. The invariants are
      ;;   SCAN-IN:  (cdr in-prev)  = LIS.
      ;;   SCAN-OUT: (cdr out-prev) = LIS.
      (letrec ((scan-in (lambda (in-prev out-prev lis)
                          (let lp ((in-prev in-prev) (lis lis))
                            (if (pair? lis)
                              (if (pred (car lis))
                                (lp lis (cdr lis))
                                (begin (set-cdr! out-prev lis)
                                       (scan-out in-prev lis (cdr lis))))
                              (set-cdr! out-prev lis))))) ; Done.
               (scan-out (lambda (in-prev out-prev lis)
                           (let lp ((out-prev out-prev) (lis lis))
                             (if (pair? lis)
                               (if (pred (car lis))
                                 (begin (set-cdr! in-prev lis)
                                        (scan-in lis out-prev (cdr lis)))
                                 (lp lis (cdr lis)))
                               (set-cdr! in-prev lis)))))) ; Done.
        ;; Crank up the scan&splice loops.
        (if (pred (car lis))
          ;; LIS begins in-list. Search for out-list's first pair.
          (let lp ((prev-l lis) (l (cdr lis)))
            (cond ((not (pair? l)) (values lis l))
                  ((pred (car l)) (lp l (cdr l)))
                  (else (scan-out prev-l l (cdr l))
                        (values lis l)))) ; Done.
          ;; LIS begins out-list. Search for in-list's first pair.
          (let lp ((prev-l lis) (l (cdr lis)))
            (cond ((not (pair? l)) (values l lis))
                  ((pred (car l))
                   (scan-in l prev-l (cdr l))
                   (values l lis)) ; Done.
                  (else (lp l (cdr l)))))))))

;; Inline us, please.
(define (my-remove pred l) (filter (lambda (x) (not (pred x))) l))
#; ; lists are immutable
(define (remove! pred l) (filter! (lambda (x) (not (pred x))) l))

;;; filter.rkt ends here
