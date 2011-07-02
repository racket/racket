;;;
;;; <delete.rkt> ---- List deletion functions
;;; Time-stamp: <02/03/01 07:26:12 noel>
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

#lang scheme/base

(require srfi/optional "predicate.rkt")

(provide delete (rename-out [delete delete!])
         delete-duplicates (rename-out [delete-duplicates delete-duplicates!]))

(define (delete x lis [= equal?])
  (filter (lambda (y) (not (= x y))) lis))

#; ; lists are immutable
(define delete! (x lis [= equal?])
  (filter! (lambda (y) (not (= x y))) lis))

;; right-duplicate deletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-duplicates delete-duplicates!
;;
;; Beware -- these are N^2 algorithms. To efficiently remove duplicates
;; in long lists, sort the list to bring duplicates together, then use a
;; linear-time algorithm to kill the dups. Or use an algorithm based on
;; element-marking. The former gives you O(n lg n), the latter is linear.

(define (delete-duplicates lis [elt= equal?])
  (check-arg procedure? elt= 'delete-duplicates)
  (let recur ((lis lis))
    (if (null-list? lis) lis
        (let* ((x (car lis))
               (tail (cdr lis))
               (new-tail (recur (delete x tail elt=))))
          (if (eq? tail new-tail) lis (cons x new-tail))))))

#; ; lists are immutable
(define (delete-duplicates! lis [elt= equal?])
  (check-arg procedure? elt= 'delete-duplicates!)
  (let recur ((lis lis))
    (if (null-list? lis) lis
        (let* ((x (car lis))
               (tail (cdr lis))
               (new-tail (recur (delete! x tail elt=))))
          (if (eq? tail new-tail) lis (cons x new-tail))))))

;;; delete.rkt ends here
