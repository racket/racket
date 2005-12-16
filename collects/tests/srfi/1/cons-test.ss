;;;
;;; <cons-test.ss> ---- List constructor tests
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

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           (lib "cons.ss" "srfi" "1"))

  (provide cons-tests)

  (define cons-tests
    (make-test-suite
     "List constructor tests"

     ;; XCONS

     (make-test-case
      "xcons:null-cdr"
      (assert-equal? (xcons '() 'Andromeda) '(Andromeda)))

     (make-test-case
      "xcons:pair-cdr"
      (let* ((base '(Antlia))
             (result (xcons base 'Apus)))
        (assert-equal? result '(Apus Antlia))
        (assert-eq? (cdr result) base)))

     (make-test-case
      "xcons:datum-cdr"
      (assert-equal? (xcons 'Aquarius 'Aquila) '(Aquila . Aquarius)))

     ;; MAKE-LIST

     (make-test-case
      "make-list:zero-length"
      (assert-true (null? (make-list 0))))

     (make-test-case
      "make-list:default-element"
      (assert-equal? (make-list 5) '(#f #f #f #f #f)))

     (make-test-case
      "make-list:fill-element"
      (assert-equal? (make-list 7 'Circinus)
                     '(Circinus Circinus Circinus Circinus
                                Circinus Circinus Circinus)))

     ;; LIST-TABULATE

     (make-test-case
      "list-tabulate:zero-length"
      (assert-true (null? (list-tabulate 0 (lambda (position) #f)))))

     (make-test-case
      "list-tabulate:identity"
      (assert-equal? (list-tabulate 5 (lambda (position) position))
                     '(0 1 2 3 4)))

     (make-test-case
      "list-tabulate:factorial"
      (assert-equal? (list-tabulate 7 (lambda (position)
                                        (do ((multiplier 1 (+ multiplier 1))
                                             (product 1 (* product multiplier)))
                                            ((< position multiplier) product))))
                     '(1 1 2 6 24 120 720)))

     ;; LIST*

     (make-test-case
      "list*:one-argument"
      (assert-eq? (list* 'Columba)
                  'Columba))

     (make-test-case
      "list*:two-arguments"
      (assert-equal? (list* 'Corvus 'Crater)
                     '(Corvus . Crater)))

     (make-test-case
      "list*:many-arguments"
      (assert-equal? (list* 'Crux 'Cygnus 'Delphinus 'Dorado 'Draco)
                     '(Crux Cygnus Delphinus Dorado . Draco)))

     (make-test-case
      "list*:last-argument-null"
      (assert-equal? (list* 'Equuleus 'Fornax '())
                     '(Equuleus Fornax)))

     (make-test-case
      "list*:last-argument-non-empty-list"
      (let* ((base '(Gemini Grus))
             (result (list* 'Hercules 'Horologium 'Hydra 'Hydrus base)))
        (assert-equal? result
                       '(Hercules Horologium Hydra Hydrus Gemini Grus))
        (assert-eq? (cddddr result) base)))

     ;; LIST-COPY

     (make-test-case
      "list-copy:null-list"
      (assert-true (null? (list-copy '()))))

     (make-test-case
      "list-copy:flat-list"
      (let* ((original '(Indus Lacerta Leo Lepus Libra))
             (result (list-copy original)))
        (assert-equal? result original)
        (assert-true (not (eq? result original)))
        (assert-true (not (eq? (cdr result) (cdr original))))
        (assert-true (not (eq? (cddr result) (cddr original))))
        (assert-true (not (eq? (cdddr result) (cdddr original))))
        (assert-true (not (eq? (cddddr result) (cddddr original))))))

     (make-test-case
      "list-copy:bush"
      (let* ((first '(Lupus))
             (second '(Lynx Malus Mensa (Microscopium Monoceros)
                            ((Musca Norma Octans))))
             (third 'Ophiuchus)
             (original (list first second third))
             (result (list-copy original)))
        (assert-equal? result original)
        (assert-true (not (eq? result original)))
        (assert-eq? (car result) first)
        (assert-true (not (eq? (cdr result) (cdr original))))
        (assert-eq? (cadr result) second)
        (assert-true (not (eq? (cddr result) (cddr original))))
        (assert-eq? (caddr result) third)))

     ;; CIRCULAR-LIST

     (make-test-case
      "circular-list:one-element"
      (let ((result (circular-list 'Orion)))
        (assert-true (and (pair? result)
                          (eq? (car result) 'Orion)
                          (eq? (cdr result) result)))))

     (make-test-case
      "circular-list:many-elements"
      (let ((result (circular-list 'Pavo 'Pegasus 'Perseus 'Phoenix 'Pictor)))
        (assert-true (and (pair? result)
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

     (make-test-case
      "iota:zero-count"
      (assert-equal? (iota 0) (list)))

     (make-test-case
      "iota:zero-count-and-step"
      (assert-equal? (iota 0 0) (list)))

     (make-test-case
      "iota:count-only"
      (assert-equal? (iota 4) (list 0 1 2 3)))

     (make-test-case
      "iota:count-and-start"
      (assert-equal? (iota 3 1) (list 1 2 3)))

     (make-test-case
      "iota:count-start-and-step"
      (assert-equal? (iota 4 3 2) (list 3 5 7 9)))

     (make-test-case
      "iota:negative-step"
      (assert-equal? (iota 4 0 -1) (list 0 -1 -2 -3)))

     (make-test-case
      "iota:non-integer-step"
      (assert-equal? (iota 5 0 1/2) (list 0 1/2 1 3/2 2)))

     (make-test-case
      "iota;negative-count"
      (assert-equal? (iota -1) (list)))

     ))
  )
;;; cons-test.ss ends here
