;;;
;;; <cons-test.rkt> ---- List constructor tests
;;; Time-stamp: <05/12/16 21:14:31 noel>
;;;
;;; Copyright (C) 2002 by Noel Welsh.
;;;
;;; This file is part of SRFI-1.

;;; SRFI-1 is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; SRFI-1 is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with SRFI-1; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

;; Originally created by:

;; John David Stone
;; Department of Mathematics and Computer Science
;; Grinnell College
;; stone@math.grin.edu

(module cons-test
  mzscheme
  (require rackunit)
  (require srfi/1/cons)

  (provide cons-tests)

  (define cons-tests
    (test-suite
     "List constructor tests"

     ;; XCONS

     (test-case
      "xcons:null-cdr"
      (check-equal? (xcons '() 'Andromeda) '(Andromeda)))

     (test-case
      "xcons:pair-cdr"
      (let* ((base '(Antlia))
             (result (xcons base 'Apus)))
        (check-equal? result '(Apus Antlia))
        (check-eq? (cdr result) base)))

     (test-case
      "xcons:datum-cdr"
      (check-equal? (xcons 'Aquarius 'Aquila) '(Aquila . Aquarius)))

     ;; MAKE-LIST

     (test-case
      "make-list:zero-length"
      (check-true (null? (make-list 0))))

     (test-case
      "make-list:default-element"
      (check-equal? (make-list 5) '(#f #f #f #f #f)))

     (test-case
      "make-list:fill-element"
      (check-equal? (make-list 7 'Circinus)
                     '(Circinus Circinus Circinus Circinus
                                Circinus Circinus Circinus)))

     ;; LIST-TABULATE

     (test-case
      "list-tabulate:zero-length"
      (check-true (null? (list-tabulate 0 (lambda (position) #f)))))

     (test-case
      "list-tabulate:identity"
      (check-equal? (list-tabulate 5 (lambda (position) position))
                     '(0 1 2 3 4)))

     (test-case
      "list-tabulate:factorial"
      (check-equal? (list-tabulate 7 (lambda (position)
                                        (do ((multiplier 1 (+ multiplier 1))
                                             (product 1 (* product multiplier)))
                                            ((< position multiplier) product))))
                     '(1 1 2 6 24 120 720)))

     ;; LIST*

     (test-case
      "list*:one-argument"
      (check-eq? (list* 'Columba)
                  'Columba))

     (test-case
      "list*:two-arguments"
      (check-equal? (list* 'Corvus 'Crater)
                     '(Corvus . Crater)))

     (test-case
      "list*:many-arguments"
      (check-equal? (list* 'Crux 'Cygnus 'Delphinus 'Dorado 'Draco)
                     '(Crux Cygnus Delphinus Dorado . Draco)))

     (test-case
      "list*:last-argument-null"
      (check-equal? (list* 'Equuleus 'Fornax '())
                     '(Equuleus Fornax)))

     (test-case
      "list*:last-argument-non-empty-list"
      (let* ((base '(Gemini Grus))
             (result (list* 'Hercules 'Horologium 'Hydra 'Hydrus base)))
        (check-equal? result
                       '(Hercules Horologium Hydra Hydrus Gemini Grus))
        (check-eq? (cddddr result) base)))

     ;; LIST-COPY

     (test-case
      "list-copy:null-list"
      (check-true (null? (list-copy '()))))

     (test-case
      "list-copy:flat-list"
      (let* ((original '(Indus Lacerta Leo Lepus Libra))
             (result (list-copy original)))
        (check-equal? result original)
        (check-true (not (eq? result original)))
        (check-true (not (eq? (cdr result) (cdr original))))
        (check-true (not (eq? (cddr result) (cddr original))))
        (check-true (not (eq? (cdddr result) (cdddr original))))
        (check-true (not (eq? (cddddr result) (cddddr original))))))

     (test-case
      "list-copy:bush"
      (let* ((first '(Lupus))
             (second '(Lynx Malus Mensa (Microscopium Monoceros)
                            ((Musca Norma Octans))))
             (third 'Ophiuchus)
             (original (list first second third))
             (result (list-copy original)))
        (check-equal? result original)
        (check-true (not (eq? result original)))
        (check-eq? (car result) first)
        (check-true (not (eq? (cdr result) (cdr original))))
        (check-eq? (cadr result) second)
        (check-true (not (eq? (cddr result) (cddr original))))
        (check-eq? (caddr result) third)))

     ;; CIRCULAR-LIST

     (test-case
      "circular-list:one-element"
      (let ((result (circular-list 'Orion)))
        (check-true (and (pair? result)
                          (eq? (car result) 'Orion)
                          (eq? (cdr result) result)))))

     (test-case
      "circular-list:many-elements"
      (let ((result (circular-list 'Pavo 'Pegasus 'Perseus 'Phoenix 'Pictor)))
        (check-true (and (pair? result)
                          (eq? (car result) 'Pavo)
                          (pair? (cdr result))
                          (eq? (cadr result) 'Pegasus)
                          (pair? (cddr result))
                          (eq? (caddr result) 'Perseus)
                          (pair? (cdddr result))
                          (eq? (cadddr result) 'Phoenix)
                          (pair? (cddddr result))
                          (eq? (car (cddddr result)) 'Pictor)
                          (eq? (cdr (cddddr result)) result)))))

     ;; IOTA

     (test-case
      "iota:zero-count"
      (check-equal? (iota 0) (list)))

     (test-case
      "iota:zero-count-and-step"
      (check-equal? (iota 0 0) (list)))

     (test-case
      "iota:count-only"
      (check-equal? (iota 4) (list 0 1 2 3)))

     (test-case
      "iota:count-and-start"
      (check-equal? (iota 3 1) (list 1 2 3)))

     (test-case
      "iota:count-start-and-step"
      (check-equal? (iota 4 3 2) (list 3 5 7 9)))

     (test-case
      "iota:negative-step"
      (check-equal? (iota 4 0 -1) (list 0 -1 -2 -3)))

     (test-case
      "iota:non-integer-step"
      (check-equal? (iota 5 0 1/2) (list 0 1/2 1 3/2 2)))

     ))
  )
;;; cons-test.rkt ends here
