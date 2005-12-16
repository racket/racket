;;;
;;; <predicate-test.ss> ---- List predicate tests
;;; Time-stamp: <05/12/16 21:16:27 noel>
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

(module predicate-test
  mzscheme

  (require
   (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
   (lib "predicate.ss" "srfi" "1")
   (lib "cons.ss" "srfi" "1"))

  (provide predicate-tests)

  (define predicate-tests
    (make-test-suite
     "List predicate tests"

     ;; PROPER-LIST?

     (make-test-case
      "proper-list?:list"
      (assert-true (proper-list? (list 1 2 3 4 5))))

     (make-test-case
      "proper-list?:dotted-list"
      (assert-true (not (proper-list? (cons 1 (cons 2 (cons 3 4)))))))

     (make-test-case
      "proper-list?:zero-length"
      (assert-true (proper-list? (list))))

     (make-test-case
      "proper-list?:circular-list"
      (assert-true (not (proper-list? (circular-list 'a 'b 'c 'd)))))

     (make-test-case
      "proper-list?:simple-value"
      (assert-true (not (proper-list? 1))))

     ;; DOTTED-LIST?

     (make-test-case
      "dotted-list?:dotted-list"
      (assert-true (dotted-list? '(1 2 3 . 4))))

     (make-test-case
      "dotted-list?:proper-list"
      (assert-true (not (dotted-list? (list 'a 'b 'c 'd)))))

     (make-test-case
      "dotted-list?:empty-list"
      (assert-true (not (dotted-list? (list)))))

     (make-test-case
      "dotted-list?:simple-value"
      (assert-true (dotted-list? "hello")))

     ;; CIRCULAR-LIST

     (make-test-case
      "circular-list?:proper-list"
      (assert-true (not (circular-list? (list 1 2 3 4)))))

     (make-test-case
      "circular-list?:dotted-list"
      (assert-true (not (circular-list? '(a b c . d)))))

     (make-test-case
      "circular-list?:simple-value"
      (assert-true (not (circular-list? 1))))

     (make-test-case
      "circular-list?:circular-list"
      (assert-true (circular-list? (circular-list 1 2 3 4))))

     ;; NOT-PAIR

     (make-test-case
      "not-pair?:list"
      (assert-true (not (not-pair? (list 1 2 3 4)))))

     (make-test-case
      "not-pair?:number"
      (assert-true (not-pair? 1)))

     (make-test-case
      "not-pair?:symbol"
      (assert-true (not-pair? 'symbol)))

     (make-test-case
      "not-pair?:string"
      (assert-true (not-pair? "string")))

     ;; NULL-LIST?

     (make-test-case
      "null-list?:null-list"
      (assert-true (null-list? (list))))

     (make-test-case
      "null-list?:list"
      (assert-true (not (null-list? (list 'a 'b 'c)))))

     (make-test-case
      "null-list?:pair"
      (assert-true (not (null-list? (cons 1 2)))))

     ;; LIST=

     (make-test-case
      "list=:number-list"
      (assert-true (list= = (list 1.0 2.0 3.0) (list 1 2 3))))

     (make-test-case
      "list=:symbol-vs-string-list"
      (assert-true (list= (lambda (x y)
                            (string=? (symbol->string x) y))
                          (list 'a 'b 'c)
                          (list "a" "b" "c"))))

     (make-test-case
      "list=:unequal-lists"
      (assert-true (not (list= eq? (list 1 2 3) (list 'a 'b 'c) (list 1 2 3)))))

     (make-test-case
      "list=:unequal-lengths"
      (assert-true (not (list= eq? (list 1 2 3) (list 1 2 3 4)))))

     (make-test-case
      "list=:empty-lists"
      (assert-true (list= eq? (list) (list) (list))))

     (make-test-case
      "list=:no-list"
      (assert-true (list= eq?)))

     ))
  )
;;; predicate-test.ss ends here
