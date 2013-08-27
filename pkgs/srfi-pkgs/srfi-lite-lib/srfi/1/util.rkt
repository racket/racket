;;;
;;; <util.rkt> ---- Utility functions
;;; Time-stamp: <02/02/28 12:05:00 noel>
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

(require "predicate.rkt"
         "selector.rkt")

(provide %cdrs
         %cars+
         %cars+cdrs
         %cars+cdrs+
         %cars+cdrs/no-test)

;; Fold/map internal utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These little internal utilities are used by the general
;; fold & mapper funs for the n-ary cases . It'd be nice if they got inlined.
;; One the other hand, the n-ary cases are painfully inefficient as it is.
;; An aggressive implementation should simply re-write these functions
;; for raw efficiency; I have written them for as much clarity, portability,
;; and simplicity as can be achieved.
;;
;; I use the dreaded call/cc to do local aborts. A good compiler could
;; handle this with extreme efficiency. An implementation that provides
;; a one-shot, non-persistent continuation grabber could help the compiler
;; out by using that in place of the call/cc's in these routines.
;;
;; These functions have funky definitions that are precisely tuned to
;; the needs of the fold/map procs -- for example, to minimize the number
;; of times the argument lists need to be examined.

;; Return (map cdr lists).
;; However, if any element of LISTS is empty, just abort and return '().
(define (%cdrs lists)
  (let/ec abort
    (let recur ((lists lists))
      (if (pair? lists)
        (let ((lis (car lists)))
          (if (null-list? lis) (abort '())
              (cons (cdr lis) (recur (cdr lists)))))
        '()))))

(define (%cars+ lists last-elt) ; (append! (map car lists) (list last-elt))
  (let recur ((lists lists))
    (if (pair? lists) (cons (caar lists) (recur (cdr lists))) (list last-elt))))

;; LISTS is a (not very long) non-empty list of lists.
;; Return two lists: the cars & the cdrs of the lists.
;; However, if any of the lists is empty, just abort and return [() ()].

(define (%cars+cdrs lists)
  (let/ec abort
    (let recur ((lists lists))
      (if (pair? lists)
        (let-values ([(list other-lists) (car+cdr lists)])
          (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
              (let-values ([(a d) (car+cdr list)]
                           [(cars cdrs) (recur other-lists)])
                (values (cons a cars) (cons d cdrs)))))
        (values '() '())))))

;; Like %CARS+CDRS, but we pass in a final elt tacked onto the end of the
;; cars list. What a hack.
(define (%cars+cdrs+ lists cars-final)
  (let/ec abort
    (let recur ((lists lists))
      (if (pair? lists)
        (let-values ([(list other-lists) (car+cdr lists)])
          (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
              (let-values ([(a d) (car+cdr list)]
                           [(cars cdrs) (recur other-lists)])
                (values (cons a cars) (cons d cdrs)))))
        (values (list cars-final) '())))))

;; Like %CARS+CDRS, but blow up if any list is empty.
(define (%cars+cdrs/no-test lists)
  (let recur ((lists lists))
    (if (pair? lists)
      (let*-values ([(list other-lists) (car+cdr lists)]
                    [(a d) (car+cdr list)]
                    [(cars cdrs) (recur other-lists)])
        (values (cons a cars) (cons d cdrs)))
      (values '() '()))))

;;; util.rkt ends here
