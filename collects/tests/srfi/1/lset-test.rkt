;;;
;;; <lset-test.rkt> ---- Lists as Sets Tests
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
  (require rackunit)
  (require srfi/1/lset)

  (provide lset-tests)

  (define lset-tests
    (test-suite
     "List as set procedures tests"

     (test-case
      "lset<=:singleton"
      (check-true (lset<= eq?)))

     (test-case
      "lset<=:empty-list"
      (check-true (lset<= eq? (list))))

     (test-case
      "lset<=:empty-lists"
      (check-true (lset<= eq? (list) (list))))

     (test-case
      "lset<=:normal-case"
      (check-true (lset<= = (list 1 2 3 4) (list 1 2 3 4))))

     (test-case
      "lset<=:normal-case-fail"
      (check-true (not (lset<= = (list 2 3 4 5) (list 1 2 3 4)))))

     (test-case
      "lset=:empty-list"
      (check-true (lset= eq?)))

     (test-case
      "lset=:singleton"
      (check-true (lset= eq? '(a b c d e))))

     (test-case
      "lset=:normal-case"
      (check-true (lset= = '(1 2 3 4 5) '(5 4 3 2 1))))

     (test-case
      "lset=:normal-case-fail"
      (check-false (lset= eq? '(a b c d e) '(a b c d))))

     (test-case
      "lset-xor:empty-list"
      (check-equal? (lset-xor eq?) '()))

     (test-case
      "lset-xor:singleton"
      (check-equal? (lset-xor eq? '(a b c d e)) '(a b c d e)))

     (test-case
      "lset-xor:normal-case"
      (check-true (lset= eq?
                          (lset-xor eq? '(a b c d e) '(a e i o u))
                          '(d c b i o u))))

     (test-case
      "lset-xor!:empty-list"
      (check-equal? (lset-xor! eq?) '()))

     (test-case
      "lset-xor!:singleton"
      (check-equal? (lset-xor! eq? '(a b c d e)) '(a b c d e)))

     (test-case
      "lset-xor!:normal-case"
      (check-true (lset= eq?
                          (lset-xor! eq? '(a b c d e) '(a e i o u))
                          '(d c b i o u))))
     ))
  )
;;; lset-test.rkt ends here
