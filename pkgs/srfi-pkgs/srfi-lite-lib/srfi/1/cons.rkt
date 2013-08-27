;;;
;;; <cons.rkt> ---- List constructors
;;; Time-stamp: <02/02/27 12:19:59 noel>
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

(require srfi/optional "selector.rkt"
         (only-in racket/list [make-list make-list*]))

(provide xcons
         make-list
         list-tabulate
         (rename-out [list* cons*])
         list-copy
         circular-list
         iota)

;; Occasionally useful as a value to be passed to a fold or other
;; higher-order procedure.
(define (xcons d a) (cons a d))

;; Make a list of length LEN.

(define (make-list len [elt #f]) (make-list* len elt))

;; Make a list of length LEN. Elt i is (PROC i) for 0 <= i < LEN.

(define (list-tabulate len proc)
  (check-arg (lambda (n) (and (integer? n) (>= n 0))) len 'list-tabulate)
  (check-arg procedure? proc 'list-tabulate)
  (for/list ([i (in-range len)]) (proc i)))

;; (cons* a1 a2 ... an) = (cons a1 (cons a2 (cons ... an)))
;; (cons* a1) = a1; (cons* a1 a2 ...) = (cons a1 (cons* a2 ...))
;;
;; (cons first (unfold not-pair? car cdr rest values))

;; reprovided as racket's list*
;; (define (cons* first . rest)
;;   (let recur ((x first) (rest rest))
;;     (if (pair? rest)
;;       (cons x (recur (car rest) (cdr rest)))
;;       x)))

(define (list-copy lis)
  (let recur ((lis lis))
    (if (pair? lis)
      (cons (car lis) (recur (cdr lis)))
      lis)))

(define (circular-list val1 . vals)
  (let ([ph (make-placeholder #f)])
    (placeholder-set! ph
      (cons val1 (let loop ([vals vals])
                   (if (null? vals)
                     ph
                     (cons (car vals) (loop (cdr vals)))))))
    (make-reader-graph ph)))

;; IOTA count [start step]  (start start+step ... start+(count-1)*step)

(define (iota count [start 0] [step 1])
  (check-arg integer? count 'iota)
  (check-arg number? start 'iota)
  (check-arg number? step 'iota)
  (unless (or (zero? count) (positive? count))
    (error 'iota "count expected to be non-negative, got: ~a" count))
  (let loop ([n 0])
    (if (= n count) '()
        (cons (+ start (* n step)) (loop (add1 n))))))

;;; cons.rkt ends here
