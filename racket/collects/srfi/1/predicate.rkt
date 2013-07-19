;;;
;;; <predicate.rkt> ---- List Predicates
;;; Time-stamp: <02/02/27 12:57:15 noel>
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

(provide pair?
         null?
         proper-list?
         circular-list?
         dotted-list?
         not-pair?
         null-list?
         list=)

;; <proper-list> ::= ()                       ; Empty proper list
;;                 | (cons <x> <proper-list>) ; Proper-list pair
;; Note that this definition rules out circular lists -- and this
;; function is required to detect this case and return false.

(define (proper-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
      (let ((x (cdr x)))
        (if (pair? x)
          (let ((x   (cdr x))
                (lag (cdr lag)))
            (and (not (eq? x lag)) (lp x lag)))
          (null? x)))
      (null? x))))

;; A dotted list is a finite list (possibly of length 0) terminated
;; by a non-nil value. Any non-cons, non-nil value (e.g., "foo" or 5)
;; is a dotted list of length 0.
;;
;; <dotted-list> ::= <non-nil,non-pair>       ; Empty dotted list
;;                 | (cons <x> <dotted-list>) ; Proper-list pair

(define (dotted-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
      (let ((x (cdr x)))
        (if (pair? x)
          (let ((x   (cdr x))
                (lag (cdr lag)))
            (and (not (eq? x lag)) (lp x lag)))
          (not (null? x))))
      (not (null? x)))))

(define (circular-list? x)
  (let lp ((x x) (lag x))
    (and (pair? x)
         (let ((x (cdr x)))
           (and (pair? x)
                (let ((x   (cdr x))
                      (lag (cdr lag)))
                  (or (eq? x lag) (lp x lag))))))))

(define (not-pair? x) (not (pair? x))) ; Inline me.

;; This is a legal definition which is fast and sloppy:
;;     (define null-list? not-pair?)
;; but we'll provide a more careful one:
(define (null-list? l)
  (cond ((pair? l) #f)
        ((null? l) #t)
        (else (error "null-list?: argument out of domain" l))))

(define (list= = . lists)
  (or (null? lists) ; special case
      (let lp1 ((list-a (car lists)) (others (cdr lists)))
        (or (null? others)
            (let ((list-b (car others))
                  (others (cdr others)))
              (if (eq? list-a list-b)        ; EQ? => LIST=
                (lp1 list-b others)
                (let lp2 ((la list-a) (lb list-b))
                  (if (null-list? la)
                    (and (null-list? lb)
                         (lp1 list-b others))
                    (and (not (null-list? lb))
                         (= (car la) (car lb))
                         (lp2 (cdr la) (cdr lb)))))))))))

;;; predicate.rkt ends here
