;;;
;;; <lset-test.ss> ---- Lists as Sets Tests
;;; Time-stamp: <05/12/16 21:15:22 noel>
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

(module lset-test
  mzscheme

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           (lib "lset.ss" "srfi" "1"))

  (provide lset-tests)

  (define lset-tests
    (make-test-suite
     "List as set procedures tests"

     (make-test-case
      "lset<=:singleton"
      (assert-true (lset<= eq?)))

     (make-test-case
      "lset<=:empty-list"
      (assert-true (lset<= eq? (list))))

     (make-test-case
      "lset<=:empty-lists"
      (assert-true (lset<= eq? (list) (list))))

     (make-test-case
      "lset<=:normal-case"
      (assert-true (lset<= = (list 1 2 3 4) (list 1 2 3 4))))

     (make-test-case
      "lset<=:normal-case-fail"
      (assert-true (not (lset<= = (list 2 3 4 5) (list 1 2 3 4)))))

     (make-test-case
      "lset=:empty-list"
      (assert-true (lset= eq?)))

     (make-test-case
      "lset=:singleton"
      (assert-true (lset= eq? '(a b c d e))))

     (make-test-case
      "lset=:normal-case"
      (assert-true (lset= = '(1 2 3 4 5) '(5 4 3 2 1))))

     (make-test-case
      "lset=:normal-case-fail"
      (assert-false (lset= eq? '(a b c d e) '(a b c d))))

     (make-test-case
      "lset-xor:empty-list"
      (assert-equal? (lset-xor eq?) '()))

     (make-test-case
      "lset-xor:singleton"
      (assert-equal? (lset-xor eq? '(a b c d e)) '(a b c d e)))

     (make-test-case
      "lset-xor:normal-case"
      (assert-true (lset= eq?
                          (lset-xor eq? '(a b c d e) '(a e i o u))
                          '(d c b i o u))))

     (make-test-case
      "lset-xor!:empty-list"
      (assert-equal? (lset-xor! eq?) '()))

     (make-test-case
      "lset-xor!:singleton"
      (assert-equal? (lset-xor! eq? '(a b c d e)) '(a b c d e)))

     (make-test-case
      "lset-xor!:normal-case"
      (assert-true (lset= eq?
                          (lset-xor! eq? '(a b c d e) '(a e i o u))
                          '(d c b i o u))))
     ))
  )
;;; lset-test.ss ends here
