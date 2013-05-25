;;;
;;; <fold.rkt> ---- List folds
;;; Time-stamp: <02/02/28 12:02:38 noel>
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
         "util.rkt")

(provide (rename-out [my-map map])
         (rename-out [my-for-each for-each])
         fold
         unfold
         pair-fold
         reduce
         fold-right
         unfold-right
         pair-fold-right
         reduce-right
         append-map
         (rename-out [append-map append-map!])
         (rename-out [my-map map!])
         pair-for-each
         filter-map
         map-in-order)

;; fold/unfold
;;;;;;;;;;;;;;

(define (unfold-right p f g seed . maybe-tail)
  (check-arg procedure? p 'unfold-right)
  (check-arg procedure? f 'unfold-right)
  (check-arg procedure? g 'unfold-right)
  (let lp ((seed seed) (ans (if (pair? maybe-tail) (car maybe-tail) '())))
    (if (p seed) ans
        (lp (g seed)
            (cons (f seed) ans)))))

(define (unfold p f g seed . maybe-tail-gen)
  (check-arg procedure? p 'unfold)
  (check-arg procedure? f 'unfold)
  (check-arg procedure? g 'unfold)
  (if (pair? maybe-tail-gen)
    (let ((tail-gen (car maybe-tail-gen)))
      (if (pair? (cdr maybe-tail-gen))
        (apply error "Too many arguments" unfold p f g seed maybe-tail-gen)
        (let recur ((seed seed))
          (if (p seed) (tail-gen seed)
              (cons (f seed) (recur (g seed)))))))
    (let recur ((seed seed))
      (if (p seed) '()
          (cons (f seed) (recur (g seed)))))))

(define (fold kons knil lis1 . lists)
  (check-arg procedure? kons 'fold)
  (if (pair? lists)
    (let lp ((lists (cons lis1 lists)) (ans knil))   ; N-ary case
      (let-values ([(cars+ans cdrs) (%cars+cdrs+ lists ans)])
        (if (null? cars+ans) ans ; Done.
            (lp cdrs (apply kons cars+ans)))))
    (let lp ((lis lis1) (ans knil))                  ; Fast path
      (if (null-list? lis) ans
          (lp (cdr lis) (kons (car lis) ans))))))

(define (fold-right kons knil lis1 . lists)
  (check-arg procedure? kons 'fold-right)
  (if (pair? lists)
    (let recur ((lists (cons lis1 lists)))        ; N-ary case
      (let ((cdrs (%cdrs lists)))
        (if (null? cdrs) knil
            (apply kons (%cars+ lists (recur cdrs))))))

    (let recur ((lis lis1))                       ; Fast path
      (if (null-list? lis) knil
          (let ((head (car lis)))
            (kons head (recur (cdr lis))))))))

(define (pair-fold-right f zero lis1 . lists)
  (check-arg procedure? f 'pair-fold-right)
  (if (pair? lists)
    (let recur ((lists (cons lis1 lists)))        ; N-ary case
      (let ((cdrs (%cdrs lists)))
        (if (null? cdrs) zero
            (apply f (append lists (list (recur cdrs)))))))
    (let recur ((lis lis1))                       ; Fast path
      (if (null-list? lis) zero (f lis (recur (cdr lis)))))))

(define (pair-fold f zero lis1 . lists)
  (check-arg procedure? f 'pair-fold)
  (if (pair? lists)
    (let lp ((lists (cons lis1 lists)) (ans zero)) ; N-ary case
      (let ((tails (%cdrs lists)))
        (if (null? tails) ans
            (lp tails (apply f (append lists (list ans)))))))
    (let lp ((lis lis1) (ans zero))
      (if (null-list? lis) ans
          (let ((tail (cdr lis)))                  ; Grab the cdr now,
            (lp tail (f lis ans))))))) ; in case F SET-CDR!s LIS.

;; REDUCE and REDUCE-RIGHT only use RIDENTITY in the empty-list case.
;; These cannot meaningfully be n-ary.

(define (reduce f ridentity lis)
  (check-arg procedure? f 'reduce)
  (if (null-list? lis) ridentity
      (fold f (car lis) (cdr lis))))

(define (reduce-right f ridentity lis)
  (check-arg procedure? f 'reduce-right)
  (if (null-list? lis) ridentity
      (let recur ((head (car lis)) (lis (cdr lis)))
        (if (pair? lis)
          (f head (recur (car lis) (cdr lis)))
          head))))

;; Mappers: append-map append-map! pair-for-each map! filter-map map-in-order
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (append-map f lis1 . lists)
  (really-append-map append-map  append  f lis1 lists))
#; ; lists are immutable
(define (append-map! f lis1 . lists)
  (really-append-map append-map! append! f lis1 lists))

(define (really-append-map who appender f lis1 lists)
  (check-arg procedure? f 'who)
  (if (pair? lists)
    (let-values ([(cars cdrs) (%cars+cdrs (cons lis1 lists))])
      (if (null? cars) '()
          (let recur ((cars cars) (cdrs cdrs))
            (let ((vals (apply f cars)))
              (let-values ([(cars2 cdrs2) (%cars+cdrs cdrs)])
                (if (null? cars2) vals
                    (appender vals (recur cars2 cdrs2))))))))
    ;; Fast path
    (if (null-list? lis1) '()
        (let recur ((elt (car lis1)) (rest (cdr lis1)))
          (let ((vals (f elt)))
            (if (null-list? rest) vals
                (appender vals (recur (car rest) (cdr rest)))))))))

(define (pair-for-each proc lis1 . lists)
  (check-arg procedure? proc 'pair-for-each)
  (if (pair? lists)
    (let lp ((lists (cons lis1 lists)))
      (let ((tails (%cdrs lists)))
        (when (pair? tails) (apply proc lists) (lp tails))))
    ;; Fast path.
    (let lp ((lis lis1))
      (unless (null-list? lis)
        (let ((tail (cdr lis))) ; Grab the cdr now,
          (proc lis)            ; in case PROC SET-CDR!s LIS.
          (lp tail))))))

;; We stop when LIS1 runs out, not when any list runs out.
#; ; lists are immutable
(define (map! f lis1 . lists)
  (check-arg procedure? f 'map!)
  (if (pair? lists)
    (let lp ((lis1 lis1) (lists lists))
      (if (not (null-list? lis1))
        (let-values ([(heads tails) (%cars+cdrs/no-test lists)])
          (set-car! lis1 (apply f (car lis1) heads))
          (lp (cdr lis1) tails))))
    ;; Fast path.
    (pair-for-each (lambda (pair) (set-car! pair (f (car pair)))) lis1))
  lis1)

;; Map F across L, and save up all the non-false results.
(define (filter-map f lis1 . lists)
  (check-arg procedure? f 'filter-map)
  (if (pair? lists)
    (let recur ((lists (cons lis1 lists)))
      (let-values ([(cars cdrs) (%cars+cdrs lists)])
        (if (pair? cars)
          (cond ((apply f cars) => (lambda (x) (cons x (recur cdrs))))
                (else (recur cdrs))) ; Tail call in this arm.
          '())))
    ;; Fast path.
    (let recur ((lis lis1))
      (if (null-list? lis) lis
          (let ((tail (recur (cdr lis))))
            (cond ((f (car lis)) => (lambda (x) (cons x tail)))
                  (else tail)))))))

;; Map F across lists, guaranteeing to go left-to-right.
;; NOTE: Some implementations of R5RS MAP are compliant with this spec;
;; in which case this procedure may simply be defined as a synonym for MAP.

(define (map-in-order f lis1 . lists)
  (check-arg procedure? f 'map-in-order)
  (if (pair? lists)
    (let recur ((lists (cons lis1 lists)))
      (let-values ([(cars cdrs) (%cars+cdrs lists)])
        (if (pair? cars)
          (let ((x (apply f cars))) ; Do head first,
            (cons x (recur cdrs)))  ; then tail.
          '())))
    ;; Fast path.
    (let recur ((lis lis1))
      (if (null-list? lis) lis
          (let ((tail (cdr lis))
                (x (f (car lis))))         ; Do head first,
            (cons x (recur tail)))))))     ; then tail.

;; We extend MAP to handle arguments of unequal length.
(define my-map map-in-order)


;;; Apply F across lists, guaranteeing to go left-to-right.
;;; NOTE: Some implementations of R5RS MAP are compliant with this spec;
;;; in which case this procedure may simply be defined as a synonym for FOR-EACH.

(define (my-for-each f lis1 . lists)
  (check-arg procedure? f for-each)
  (if (pair? lists)
    (let recur ((lists (cons lis1 lists)))
      (let-values ([(cars cdrs) (%cars+cdrs lists)])
        (when (pair? cars)
          (apply f cars)  ; Do head first,
          (recur cdrs)))) ; then tail.
    ;; Fast path.
    (let recur ((lis lis1))
      (unless (null-list? lis)
        (f (car lis))             ; Do head first,
        (recur (cdr lis))))))

;;; fold.rkt ends here
