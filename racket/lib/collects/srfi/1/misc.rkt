;;;
;;; <misc.rkt> ---- Miscellaneous list procedures
;;; Time-stamp: <02/03/01 13:52:22 noel>
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

(require srfi/optional
         "predicate.rkt"
         "selector.rkt"
         "util.rkt"
         (only-in "fold.rkt" reduce-right)
         (rename-in "fold.rkt" [map s:map] [for-each s:for-each])
         (only-in scheme/list count append*))

(provide length+
         (rename-out [append* concatenate] [append* concatenate!])
         (rename-out [append append!])
         (rename-out [reverse reverse!])
         append-reverse (rename-out [append-reverse append-reverse!])
         zip unzip1 unzip2 unzip3 unzip4 unzip5
         count)

#; ; reprovided from scheme/list
;; count
;;;;;;;;
(define (count pred list1 . lists)
  (check-arg procedure? pred 'count)
  (if (pair? lists)
    ;; N-ary case
    (let lp ((list1 list1) (lists lists) (i 0))
      (if (null-list? list1) i
          (let-values ([(as ds) (%cars+cdrs lists)])
            (if (null? as) i
                (lp (cdr list1) ds
                    (if (apply pred (car list1) as) (+ i 1) i))))))
    ;; Fast path
    (let lp ((lis list1) (i 0))
      (if (null-list? lis) i
          (lp (cdr lis) (if (pred (car lis)) (+ i 1) i))))))

(define (length+ x)  ; Returns #f if X is circular.
  (let lp ((x x) (lag x) (len 0))
    (if (pair? x)
      (let ((x (cdr x))
            (len (+ len 1)))
        (if (pair? x)
          (let ((x   (cdr x))
                (lag (cdr lag))
                (len (+ len 1)))
            (and (not (eq? x lag)) (lp x lag len)))
          len))
      len)))

(define (zip list1 . more-lists) (apply s:map list list1 more-lists))

;; Unzippers -- 1 through 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unzip1 lis) (map car lis))

(define (unzip2 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis) ; Use NOT-PAIR? to handle
        (let ((elt (car lis)))            ; dotted lists.
          (let-values ([(a b) (recur (cdr lis))])
            (values (cons (car  elt) a)
                    (cons (cadr elt) b)))))))

(define (unzip3 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis)
        (let ((elt (car lis)))
          (let-values ([(a b c) (recur (cdr lis))])
            (values (cons (car   elt) a)
                    (cons (cadr  elt) b)
                    (cons (caddr elt) c)))))))

(define (unzip4 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis)
        (let ((elt (car lis)))
          (let-values ([(a b c d) (recur (cdr lis))])
            (values (cons (car    elt) a)
                    (cons (cadr   elt) b)
                    (cons (caddr  elt) c)
                    (cons (cadddr elt) d)))))))

(define (unzip5 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis lis)
        (let ((elt (car lis)))
          (let-values ([(a b c d e) (recur (cdr lis))])
            (values (cons (car     elt) a)
                    (cons (cadr    elt) b)
                    (cons (caddr   elt) c)
                    (cons (cadddr  elt) d)
                    (cons (car (cddddr  elt)) e)))))))

;; append! append-reverse append-reverse! concatenate concatenate!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#; ; lists are immutable
(define (my-append! . lists)
  ;; First, scan through lists looking for a non-empty one.
  (let lp ((lists lists) (prev '()))
    (if (not (pair? lists)) prev
        (let ((first (car lists))
              (rest (cdr lists)))
          (if (not (pair? first)) (lp rest first)
              ;; Now, do the splicing.
              (let lp2 ((tail-cons (last-pair first))
                        (rest rest))
                (if (pair? rest)
                  (let ((next (car rest))
                        (rest (cdr rest)))
                    (set-cdr! tail-cons next)
                    (lp2 (if (pair? next) (last-pair next) tail-cons)
                         rest))
                  first)))))))


;;(define (append-reverse rev-head tail) (fold cons tail rev-head))

;;(define (append-reverse! rev-head tail)
;;  (pair-fold (lambda (pair tail) (set-cdr! pair tail) pair)
;;             tail
;;             rev-head))

;; Hand-inline the FOLD and PAIR-FOLD ops for speed.

(define (append-reverse rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
        (lp (cdr rev-head) (cons (car rev-head) tail)))))

#; ; lists are immutable
(define (append-reverse! rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
        (let ((next-rev (cdr rev-head)))
          (set-cdr! rev-head tail)
          (lp next-rev rev-head)))))

#; ; reprovide scheme/list's `append*' function
(define (concatenate lists) (reduce-right append '() lists))
#; ; lists are immutable
(define (concatenate! lists) (reduce-right my-append! '() lists))

#; ; lists are immutable
(define (my-reverse! lis)
  (let lp ((lis lis) (ans '()))
    (if (null-list? lis) ans
        (let ((tail (cdr lis)))
          (set-cdr! lis ans)
          (lp tail lis)))))

;;; misc.rkt ends here
