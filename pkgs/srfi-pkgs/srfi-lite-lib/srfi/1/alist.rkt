;;;
;;; <alist.rkt> ---- Association list functions
;;; Time-stamp: <02/03/01 13:56:33 noel>
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

(require (only-in "search.rkt" find))

(provide (rename-out [my-assoc assoc])
         alist-cons
         alist-copy
         alist-delete
         #; alist-delete! ; lists are immutable
         )

;; Extended from R4RS to take an optional comparison argument.
(define (my-assoc x lis [= equal?])
  (find (lambda (entry) (= x (car entry))) lis))

(define (alist-cons key datum alist) (cons (cons key datum) alist))

(define (alist-copy alist)
  (map (lambda (elt) (cons (car elt) (cdr elt))) alist))

(define (alist-delete key alist [= equal?])
  (filter (lambda (elt) (not (= key (car elt)))) alist))

#; ; lists are immutable
(define (alist-delete! key alist [= equal?])
  (filter! (lambda (elt) (not (= key (car elt)))) alist))

;;; alist.rkt ends here
